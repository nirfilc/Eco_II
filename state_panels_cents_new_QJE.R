library(haven)
library(dplyr)       # for data wrangling
library(tidyr)       # for reshape
library(lubridate)   # for dates
library(data.table)  # for efficient merging
setwd("C://Users//t-nirfilc//OneDrive - Microsoft//Desktop//Eco II")


dt <- read_dta("data/totransfer_sample.dta")
summary(dt$gradecp)
#' State Panel Data Processing
#' 
#' This script processes state-level panel data to create wage bins and demographic
#' aggregations similar to the QJE analysis. It includes functions for data loading,
#' cleaning, and aggregation at the state-quarter level.

#' Main configuration function to set up parameters
#' 
#' @param data_dir Directory containing input data files
#' @param base_year Base year for CPI adjustments (default: 2016)
#' @param wage_min Minimum wage threshold in cents (default: 125)
#' @param wage_max Maximum wage threshold in cents (default: 3000)
#' @param wage_bin_size Bin size in cents (default: 25)
#' @param vars List of required variables from input data
#' @return List of configuration parameters
setup_config <- function(
    data_dir,
    base_year = 2016,
    wage_min = 125,
    wage_max = 3000,
    wage_bin_size = 25,
    vars = c(
      "month", "state", "age", "marital", "race", "sex", "esr", "ethnic",
      "uhours", "earnhr", "uearnwk", "earnwt", "uhourse", "paidhre",
      "earnhre", "earnwke", "I25a", "I25b", "I25c", "I25d", "year",
      "lfsr89", "lfsr94", "statenum", "monthdate", "quarterdate", "gradeat",
      "gradecp", "ihigrdc", "grade92"
    )
) {
  list(
    data_dir = data_dir,
    base_year = base_year,
    wage_min = wage_min,
    wage_max = wage_max,
    wage_bin_size = wage_bin_size,
    vars = vars
  )
}

#' Load and prepare CPI data
#' 
#' @param config Configuration list from setup_config()
#' @param cpi_file CPI data file name
#' @return Data frame with monthly CPI values
load_cpi_data <- function(config, cpi_file) {
  library(tidyr)
  library(dplyr)
  
  cpi <- read_dta(file.path(config$data_dir, cpi_file))
  
  cpi <- cpi %>%
    select(-v14, -avg) %>%
    pivot_longer(
      cols = starts_with("month"),
      names_to = "month",
      values_to = "cpi"
    ) %>%
    filter(year >= 1979)
  
  # Adjust CPI to base year
  base_cpi <- cpi %>%
    filter(year == config$base_year) %>%
    pull(cpi) %>%
    mean()
  
  cpi %>%
    mutate(
      cpi = 100 * cpi / base_cpi,
      monthdate = (12 * (as.numeric(year) - 1960) + as.integer(gsub("month", "", month)))
    )
}

#' Clean employment data
#' 
#' @param data Input employment data frame
#' @return Cleaned data frame with employment indicators
clean_employment <- function(data) {
  cleaned <- data %>%
    mutate(
      wage = case_when(
        paidhre == 1 ~ earnhre / 100,
        paidhre == 2 ~ earnwke / uhourse,
        TRUE ~ NA_real_
      ),
      hoursimputed = (I25a > 0 & !is.na(I25a)),
      earningsimputed = (I25d > 0 & !is.na(I25d)),
      wageimputed = (I25c > 0 & !is.na(I25c))
    ) 
  cleaned <- cleaned %>%
    # Handle 1989-1993 imputations
    mutate(
      across(
        c(hoursimputed, earningsimputed, wageimputed),
        ~case_when(
          year >= 1989 & year <= 1993 ~ FALSE,
          year == 1994 | (year == 1995 & month <= 8) ~ FALSE,
          TRUE ~ .
        )
      )
    ) 
  cleaned <- cleaned %>%
    mutate(
      imputed = case_when(
        paidhre == 2 & (hoursimputed | earningsimputed) ~ TRUE,
        paidhre == 1 & wageimputed ~ TRUE,
        TRUE ~ FALSE
      ),
      wage = if_else(imputed, NA_real_, wage),
      logwage = log(wage)
    ) %>%
    filter(!is.na(logwage))
}

#' Create demographic variables
#' 
#' @param data Input data frame
#' @return Data frame with added demographic indicators
create_demographics <- function(data) {
  data %>%
    mutate(
      hispanic = case_when(
        year >= 1976 & year <= 2002 & between(ethnic, 1, 7) ~ 1,
        year >= 2003 & year <= 2013 & between(ethnic, 1, 5) ~ 1,
        year >= 2014 & between(ethnic, 1, 8) ~ 1,
        TRUE ~ 0
      ),
      race = if_else(race >= 3, 3, race),
      black = (race == 2 & hispanic == 0),
      white = (race == 1 & hispanic == 0),
      dmarried = (marital <= 2),
      teen = between(age, 16, 19),
      sex = (sex == 1)
    )
}

#' Create education variables
#' 
#' @param data Input data frame
#' @return Data frame with education indicators
create_education <- function(data) {
  # Grade92 mapping
  grade92_map <- tibble(
    grade92 = c(31:46),
    impute92 = c(0, 2.5, 5.5, 7.5, 9, 10, 11, 12, 12, 13, 14, 14, 16, 18, 18, 18)
  )
  
  mutated <- data %>%
    mutate(
      hgradecp = case_when(
        is.na(gradecp) ~ NA_real_,
        gradecp == 1 ~ gradeat,
        gradecp == 2 ~ gradeat - 1,
        !is.na(ihigrdc) ~ ihigrdc,
        TRUE ~ NA_real_
      )
    )
  mutated <- mutated %>%
    left_join(grade92_map, by = "grade92") %>%
    mutate(
      hgradecp = coalesce(hgradecp, impute92),
      hgradecp = if_else(hgradecp == -1, 0, hgradecp),
      hsl = (hgradecp <= 12),
      hsd = case_when(
        year >= 1992 & grade92 <= 38 ~ TRUE,
        year < 1992 & hgradecp < 12 ~ TRUE,
        TRUE ~ FALSE
      ),
      hsl40 = (hsl & age < 40),
      hsd40 = (hsd & age < 40)
    )
  mutated <- mutated %>%
    mutate(
      conshours = if_else(I25a == 0 & !is.na(uhourse), uhourse, NA_real_)
    )
  
}

#' Create wage bins
#' 
#' @param data Input data frame
#' @param config Configuration list
#' @return Data frame with wage bins
create_wage_bins <- function(data, config) {
  data %>%
    mutate(
      wage = wage * (100 / cpi), # Convert to real wages
      wage = wage * 100,  # Convert to cents
      wagebins = case_when(
        wage < config$wage_min ~ config$wage_min,
        wage >= config$wage_max ~ config$wage_max,
        TRUE ~ floor(wage / config$wage_bin_size) * config$wage_bin_size
      )
    )
}

#' Aggregate data by state-quarter-wagebin
#' 
#' @param data Input data frame
#' @return List of aggregated data frames
aggregate_data <- function(data) {
  # Adjust weights for quarterly
  data <- data %>%
    mutate(earnwt = earnwt / 3)
  
  # Wage bin counts
  counts <- data %>%
    group_by(statenum, quarterdate, wagebins) %>%
    summarise(totpopcount = sum(earnwt), .groups = "drop")
  
  # Demographic counts
  demog <- data %>%
    group_by(statenum, quarterdate, wagebins) %>%
    summarise(
      across(
        c(hispanic, dmarried, hsl, hsl40, hsd40, hsd, black, white, sex, teen),
        ~sum(. * earnwt),
        .names = "{.col}count"
      ),
      .groups = "drop"
    )
  
  # Averages
  avgs <- data %>%
    group_by(statenum, quarterdate, wagebins) %>%
    summarise(
      across(
        c(age, hgradecp, conshours, wage),
        ~weighted.mean(., earnwt),
        .names = "ave{.col}"
      ),
      .groups = "drop"
    )
  
  # FTE
  fte <- data %>%
    mutate(FTE_orig = (earnwt * conshours) / 40) %>%
    group_by(statenum, quarterdate, wagebins) %>%
    summarise(FTE_orig = sum(FTE_orig), .groups = "drop")
  
  # Total population
  totpop <- counts %>%
    group_by(statenum, quarterdate) %>%
    summarise(totalpopulation = sum(totpopcount), .groups = "drop")
  
  list(
    counts = counts,
    demographics = demog,
    averages = avgs,
    fte = fte,
    totpop = totpop
  )
}

#' Main processing function
#' 
#' @param config Configuration from setup_config()
#' @param input_file Main input data file name
#' @param cpi_file CPI data file name
#' @param output_file Output file name
#' @return Processed data frame saved to output_file
process_state_panels <- function(config, input_file, cpi_file, output_file) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  # Load and merge CPI data
  cpi_data <- load_cpi_data(config, cpi_file)
  # Load main data
  data <- read_dta(file.path(config$data_dir, input_file)) %>%
    select(all_of(config$vars))
  
  setDT(data)
  setDT(cpi_data)
  # Merge CPI data
  data <- merge(data, cpi_data[, .(monthdate, cpi)], by = "monthdate", all.x = TRUE)

  # Process data
  processed <- data %>%
    clean_employment()
  
  processed <- processed%>%
    create_demographics()
  
  processed <- processed%>%
    create_education()
  
  processed <- processed %>%
    create_wage_bins(config)
  
  # Aggregate data
  aggs <- aggregate_data(processed)
  
  # Combine aggregations
  final <- aggs$counts %>%
    left_join(aggs$demographics, by = c("statenum", "quarterdate", "wagebins")) %>%
    left_join(aggs$averages, by = c("statenum", "quarterdate", "wagebins")) %>%
    left_join(aggs$fte, by = c("statenum", "quarterdate", "wagebins")) %>%
    left_join(aggs$totpop, by = c("statenum", "quarterdate"))
  
  # Save output
  saveRDS(final, file.path(config$data_dir, output_file))
  
  final
}

#' Usage example:
config <- setup_config(
   data_dir = "data",
   base_year = 2016 )
result <- process_state_panels(
   config,
   input_file = "totransfer_sample.dta",
   cpi_file = "cpiursai1977-2016.dta",
   output_file = "state_panels_cents_QJE.rds"
 )

real_result <- read_dta("data/state_panels_cents_QJE.dta")
enum1 <- real_result %>% filter(statenum == 1) %>% filter(quarterdate>216)

filtered_result <- result %>% filter(quarterdate>216)
