library(haven)
library(dplyr)
library(zoo)      # for year/quarter extraction
library(data.table)

mw <- read_dta(file.path("data//VZ_mw_state_quarterly_new.dta")) %>%
  rename(
    quarterdate = quarterly_date,
    statenum = statefips
  ) %>%
  mutate(
    mw = max_mw,
    logmw = log(max_mw),
    year = 1960 + floor(quarterdate / 4),
    quarter = (quarterdate %% 4) + 1
    )

setDT(mw)
setkey(mw, statenum, quarterdate)

# Generate 12 lags and 12 leads of log(mw)
for (i in 1:12) {
  mw[, paste0("L", i, "logmw") := shift(logmw, n = i, type = "lag"), by = statenum]
  mw[, paste0("F", i, "logmw") := shift(logmw, n = i, type = "lead"), by = statenum]
}

# L0logmw = current logmw
mw[, L0logmw := logmw]

mw <- mw[year >= 1979 & year <= 2016]

# Check balanced panel: all statenum should have same number of time points
panel_lengths <- mw[, .N, by = statenum]
stopifnot(length(unique(panel_lengths$N)) == 1)

mw <- mw %>%
  select(statenum, quarterdate, logmw, matches("^[LF]\\d+logmw$"))

# Load teen-level CPS data for division info
cps <- read_dta(file.path("data//macrocps_basic_teens.dta")) %>%
  mutate(tagger = !duplicated(statenum)) %>%
  filter(tagger) %>%
  select(statenum, division)

# Merge back into mw data
mw <- left_join(mw, cps, by = "statenum")
mw_real <- read_dta("data//VZmw_quarterly_lagsleads_1979_2016.dta")
