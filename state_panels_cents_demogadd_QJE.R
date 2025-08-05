library(dplyr)
library(haven)
library(data.table)

balanced_data <- read_dta(file.path("data//state_panels_cents_balanced_QJE.dta"))

balanced_data <- balanced_data %>%
  rename(
    avehours = aveconshours,
    population = totalpopulation,
    count = totpopcount
  )

write_dta(balanced_data, file.path(data_path, "state_panels_cents_balanced_QJE.dta"))

micro <- read_dta(file.path("data//totransfer_sample.dta")) %>%
  select(month, state, age, marital, race, sex, esr, ethnic, uhours, earnhr, uearnwk, earnwt,
         uhourse, paidhre, earnhre, earnwke, I25a, I25b, I25c, I25d, year,
         lfsr89, lfsr94, statenum, orgwt, monthdate, quarterdate, quarter, division, region,
         censusnum, stfips, gradeat, gradecp, ihigrdc, grade92, smsastat, smsa80, smsa93, smsa04)


micro <- micro %>%
  mutate(
    hispanic = case_when(
      between(year, 1976, 2002) & ethnic %in% 1:7 ~ 1,
      between(year, 2003, 2013) & ethnic %in% 1:5 ~ 1,
      year >= 2014 & ethnic %in% 1:8 ~ 1,
      TRUE ~ 0
    )
  )

micro <- micro %>%
  mutate(race = ifelse(race >= 3, 3, race))

micro <- micro %>%
  mutate(
    black = as.integer(race == 2 & hispanic == 0)
  )

micro <- micro %>%
  mutate(
    dmarried = ifelse(!is.na(marital), as.integer(marital <= 2), NA)
  )

micro <- micro %>%
  mutate(sex = ifelse(sex == 2, 0, sex))

micro <- micro %>%
  mutate(
    hgradecp = case_when(
      gradecp == 1 ~ gradeat,
      gradecp == 2 ~ gradeat - 1,
      TRUE ~ NA_real_
    ),
    hgradecp = ifelse(!is.na(ihigrdc) & is.na(hgradecp), ihigrdc, hgradecp)
  )

# Impute from grade92
grade92code <- c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46)
impute92code <- c(0,2.5,5.5,7.5,9,10,11,12,12,13,14,14,16,18,18,18)
for (i in seq_along(grade92code)) {
  micro$hgradecp[micro$grade92 == grade92code[i]] <- impute92code[i]
}

micro <- micro %>%
  mutate(hgradecp = ifelse(hgradecp == -1, 0, hgradecp))

micro <- micro %>%
  mutate(
    hsl = as.integer(hgradecp <= 12),
    hsd = as.integer((grade92 <= 38 & year >= 1992) | (hgradecp < 12 & year < 1992)),
    hsl40 = as.integer(hsl == 1 & age < 40),
    hsd40 = as.integer(hsd == 1 & age < 40)
  )

# Adjust weight
micro <- micro %>%
  mutate(earnwt = earnwt / 3)

micro <- micro %>%
  mutate(
    teen = as.integer(age >= 16 & age <= 19),
    white = as.integer(race == 1 & hispanic == 0)
  )

# Collapse by statenum-quarterdate
agg_demo <- micro %>%
  group_by(statenum, quarterdate) %>%
  summarise(
    hispanicpop = sum(hispanic * earnwt, na.rm = TRUE),
    dmarriedpop = sum(dmarried * earnwt, na.rm = TRUE),
    hslpop = sum(hsl * earnwt, na.rm = TRUE),
    hsl40pop = sum(hsl40 * earnwt, na.rm = TRUE),
    hsdpop = sum(hsd * earnwt, na.rm = TRUE),
    hsd40pop = sum(hsd40 * earnwt, na.rm = TRUE),
    blackpop = sum(black * earnwt, na.rm = TRUE),
    whitepop = sum(white * earnwt, na.rm = TRUE),
    genderpop = sum(sex * earnwt, na.rm = TRUE),
    teenpop = sum(teen * earnwt, na.rm = TRUE),
    .groups = "drop"
  )

final <- left_join(balanced_data, agg_demo, by = c("statenum", "quarterdate"))

write_dta(final, file.path(data_path, "state_panels_cents_balanced_add_QJE.dta"))
