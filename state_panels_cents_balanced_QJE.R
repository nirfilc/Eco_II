library(haven)
library(dplyr)
library(tidyr)
library(data.table)
# Load original panel data
df <- read_dta(file.path("data//state_panels_cents_QJE.dta")) %>%
  filter(!is.na(year))

# Determine the range of quarters
minqrt <- min(df$quarterdate, na.rm = TRUE)
maxqrt <- max(df$quarterdate, na.rm = TRUE)
numberqrt <- maxqrt - minqrt + 1

# Select distinct wagebin-state pairs
shell <- df %>%
  distinct(wagebins, statenum) %>%
  slice(rep(1:n(), each = numberqrt)) %>%
  group_by(wagebins, statenum) %>%
  mutate(quarterdate = minqrt + row_number() - 1) %>%
  ungroup() %>%
  arrange(wagebins, statenum, quarterdate)

df_full <- full_join(df, shell, by = c("wagebins", "statenum", "quarterdate"))


fill_vars <- c("year", "cpi", "blackcountall", "hispaniccountall",
               "teencountall", "hslcountall", "hsl40countall", "hsdcountall",
               "hsd40countall", "gendercountall", "totalpopulation", 
               "whitecountall", "countall")

df_full <- df_full %>%
  group_by(statenum, quarterdate) %>%
  mutate(across(all_of(fill_vars), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

zero_fill_vars <- c("blackcount", "dmarriedcount", "gendercount", "hispaniccount",
                    "hslcount", "hsl40count", "hsdcount", "hsd40count", "teencount",
                    "totpopcount", "whitecount", "FTE_orig")

df_full <- df_full %>%
  mutate(across(all_of(zero_fill_vars), ~ replace_na(., 0)))

df_full <- df_full %>%
  mutate(wagebinstate = as.integer(interaction(wagebins, statenum, drop = TRUE)))

# Verify: Each wagebinstate should have exactly numberqrt entries
balanced_check <- df_full %>%
  count(wagebinstate) %>%
  filter(n != numberqrt)

stopifnot(nrow(balanced_check) == 0)

stata_data <- read_dta("data//state_panels_cents_balanced_QJE.dta")
write_dta(df_full %>% select(-wagebinstate), 
          file.path(data_path, "state_panels_cents_balanced_QJE.dta"))



dim(stata_data)
dim(df_full)

library(purrr)

# Function to summarize numeric variables
summary_stats <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))
}

stata_summary <- summary_stats(stata_data)
r_summary     <- summary_stats(df_full)

# Check side-by-side
all.equal(stata_summary, r_summary, tolerance = 1e-6)

