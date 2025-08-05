library(haven)
library(dplyr)
library(data.table)

qcew <- read_dta(file.path("data\\qcew_state_overall_counts_2016.dta")) %>%
  rename(statenum = statefips) %>%
  mutate(quarterdate = 4 * (year - 1960) + (quarter - 1))
cps_counts <- read_dta(file.path("data\\state_panels_with3quant1979.dta")) %>%
  select(statenum, quarterdate, countall) %>%
  distinct()

merged <- left_join(cps_counts, qcew, by = c("statenum", "quarterdate"))
# Find unmatched except for quarterdate == 227
bad_merge <- merged %>%
  filter(is.na(emp) | is.na(countall)) %>%
  filter(quarterdate != 227)

stopifnot(nrow(bad_merge) == 0)
merged <- merged %>%
  mutate(
    qcew_multiplier = emp / countall,
    multiplier = ifelse(qcew_multiplier == 0 | is.na(qcew_multiplier), 1, qcew_multiplier)
  )
merged_real <- read_dta("data\\qcew_multiplier.dta")
