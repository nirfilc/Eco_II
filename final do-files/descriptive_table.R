setwd("E:/uni_R_2025/ECON_B/F_proj")
gc()
rm(list=ls())


library(haven)
library(dplyr)
library(tidyr)
library(tibble)

########### The data for the table ########
##### ----event data -------
eventclassification <- read_dta("./data/eventclassification.dta") 
start_quarter <- 76
end_quarter   <- 227

Date_list <- tibble(
  quarterdate = start_quarter:end_quarter,
  year    = 1979 + ((quarterdate - start_quarter) %/% 4),
  quarter = 1 +   ((quarterdate - start_quarter) %% 4)
) %>% dplyr::select(quarterdate, year)

events_full <- merge(eventclassification, Date_list, by = "quarterdate", all.x = TRUE,
                     all.y = FALSE)
true_events <- events_full %>% filter(toosmall == 0, fedincrease == 0)

rm(eventclassification, events_full, Date_list, end_quarter, start_quarter)

##### ---- main data -------
# using state panels cents new QJE
# Here we create the income_levels by which we shall split the data.
df <- read_dta("./data/state_panels_cents_QJE.dta") %>% 
  mutate(
    income_level = case_when(
      wagebins > 2000 ~ "high",
      wagebins > 1000 ~ "med",
      TRUE            ~ "low"
    ),
    income_level = factor(income_level, levels = c("low","med","high"))
  )

ratio_vars <- c(
  "blackcount", "dmarriedcount", "gendercount", "hispaniccount",
  "hsdcount", "hsd40count", "hslcount", "hsl40count",
  "teencount", "whitecount"
)
#First we shall create state, quarter, income level share of each group
df_by_income_q <- df %>%
  group_by(statenum, quarterdate, year, income_level) %>% 
  # summing the different groups across the 3 income levels
  summarise(countall = mean(countall, na.rm = TRUE),
    across(all_of(ratio_vars), ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"), # summing across the 3 income levels
            .groups = "drop") %>%
  # calculating the share of each group out of the total population (survey based)
  mutate(
    across(
      ends_with("_sum"),
      ~ if_else(countall > 0, .x / countall, NA_real_),   # calculation the share out of total population
      .names = "{sub('_sum$', '', .col)}_share"
    )
  )

# Now we shall transform it to the yearly level
# This creates the ratio of said group out of total population in that income level
df_by_income_y <- df_by_income_q %>%
  group_by(statenum, year, income_level) %>%
  summarise(
    across(ends_with("_share"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # rename *_share -> *_ratio for the final output
  rename_with(~ sub("_share$", "_ratio", .x), ends_with("_share")) %>%
  arrange(statenum, year, income_level)

# --- Allocation across income levels so each subgroup's low/med/high sum to 1 ---
# This creates the allocation of said group in income level out of 
# total population of that group. that means if we sum all __alloc across income_level
# for that group we will total to 1.
df_by_income_y_alloc <- df_by_income_y %>%
  group_by(statenum, year) %>%
  mutate(
    across(
      ends_with("_ratio"),
      ~ {
        den <- sum(.x, na.rm = TRUE)
        if (is.finite(den) && den > 0) .x / den else rep(NA_real_, length(.x))
      },
      .names = "{.col}_alloc"
    )
  ) %>%
  ungroup()


#Share of group out of population 
# this creates the total share of that group out of population, with no regard to income levels.
df_by_income_out_of_pop <- df_by_income_y %>%
  group_by(statenum, year) %>%
  summarise(
    across(ends_with("_ratio"), ~ sum(.x, na.rm = TRUE), .names = "{sub('_ratio$', '', .col)}_pop"),
    .groups = "drop"
  )

# Aggregated age and share of population per income, state, year level
# This creates the average age, and share of population per income level
# first we get move from bin quarter to income quarter level
agg_df <- df %>% 
  group_by(statenum, year, income_level, quarterdate) %>% 
  # summing the different groups across the 3 income levels
  summarise(ave_age = mean(aveage, na.rm = TRUE),
            counted_total_pop = sum(totpopcount, na.rm = TRUE),
            countall =  mean(countall, na.rm = TRUE),
            .groups = "drop") %>% 
  # now we  move from bin quarter to income year level
  group_by(statenum, year, income_level) %>% 
  summarise(ave_age = mean(ave_age, na.rm = TRUE),
            counted_total_pop = mean(counted_total_pop/countall, na.rm = TRUE), 
            .groups = "drop")


df_add <- merge(df_by_income_y_alloc, agg_df, by = c("statenum", "year", "income_level"),
                all.x = TRUE, all.y = TRUE)
df_demographics <- merge(df_add, df_by_income_out_of_pop, by = c("statenum", "year"),
                         all.x = TRUE, all.y = TRUE)
#remove the extra created objects for better performance 
rm(df, agg_df, df_add, df_by_income_out_of_pop, df_by_income_y, df_by_income_y_alloc,
   ratio_vars, df_by_income_q)

##### ---- merge of data and events -------

# NOTE: we noticed that there are cases where the authors use two cases per state, year.
# In our opinion this might be problematic, as such we shall treat it as one event, with  137 in total.
# 

#First we calculate the proper one year backwards of each event
treat_map <- true_events %>% dplyr::select(statenum, year) %>% 
  mutate(treat_year = year,
         year = year - 1) %>% 
  distinct()

descriptive_data <- merge(df_demographics, treat_map,
  by = c("statenum", "year"),  all = FALSE)

descriptive_data_summ <- descriptive_data %>% group_by(income_level) %>% 
  summarise(ave_age = mean(ave_age, na.rm = TRUE),
            counted_total_pop = mean(counted_total_pop, na.rm = TRUE),
            across(ends_with("_ratio"), ~ mean(.x, na.rm = TRUE)),
            across(ends_with("_alloc"), ~ mean(.x, na.rm = TRUE)),
            across(ends_with("_pop"), ~ mean(.x, na.rm = TRUE))
            , 
            .groups = "drop")



##### ---- Creating the table -------

# Map rows → share var and matching _pop var (if any)
row_map <- tribble(
  ~row_label,                ~share_var,                  ~pop_var,
  "Share of population",     "counted_total_pop",         NA_character_,
  "Average age",             "ave_age",                   NA_character_,
  "Female (share)",          "gendercount_ratio_alloc",   "gendercount_pop",
  "HS education (share)",    "hslcount_ratio_alloc",      "hslcount_pop",
  "White (share)",           "whitecount_ratio_alloc",    "whitecount_pop",
  "Black (share)",           "blackcount_ratio_alloc",    "blackcount_pop",
  "Hispanic (share)",        "hispaniccount_ratio_alloc", "hispaniccount_pop"
)

# 1) Shares → wide (low/med/high)
shares_wide <- descriptive_data_summ %>%
  dplyr::select(income_level, dplyr::all_of(row_map$share_var)) %>%
  tidyr::pivot_longer(-income_level, names_to = "share_var", values_to = "value") %>%
  dplyr::left_join(dplyr::select(row_map, row_label, share_var), by = "share_var") %>%
  dplyr::select(row_label, income_level, value) %>%
  tidyr::pivot_wider(names_from = income_level, values_from = value) %>%
  dplyr::arrange(match(row_label, row_map$row_label))

# 2) Pop totals (sum across income levels) → one number per row
pop_vars <- row_map$pop_var[!is.na(row_map$pop_var)]

counts_total <- descriptive_data_summ %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(pop_vars), ~ mean(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "pop_var", values_to = "pop_count") %>%
  dplyr::right_join(dplyr::select(row_map, row_label, pop_var), by = "pop_var") %>%
  dplyr::select(row_label, pop_count)

# scalar total of counted_total_pop (used to fill that single row)
total_share_pop <- sum(descriptive_data_summ$counted_total_pop, na.rm = TRUE)

# 3) Final table (no duplicate rows)
table_out <- shares_wide %>%
  dplyr::left_join(counts_total, by = "row_label") %>%
  dplyr::mutate(
    pop_count = dplyr::coalesce(
      pop_count,
      dplyr::if_else(row_label == "Share of population", total_share_pop, NA_real_)
    )
  ) %>%
  dplyr::relocate(row_label) %>%
  dplyr::arrange(match(row_label, row_map$row_label)) %>%
  dplyr::rename(`share of pop` = pop_count)


knitr::kable(table_out, booktabs = TRUE, digits = 3,
             col.names = c("","Low","Med","High","Population count"))

