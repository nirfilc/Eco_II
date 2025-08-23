####### Labour Force Survey Data Perparation
setwd("C://Users//t-nirfilc//OneDrive - Microsoft//Desktop//Eco II//Eco_II//Extension//Data")
source("C://Users//t-nirfilc//OneDrive - Microsoft//Desktop//Eco II//Eco_II//Replication//figure_ii_helpers.R")

# install & load
library(data.table)
library(fs)
library(fixest)

# list all CSVs
csv_files <- dir_ls(
  getwd(), 
  recurse = TRUE, 
  regexp = "pub\\d{2}[0-1][0-9]\\.csv$"
)
# merge
dt <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

# Place this block right after the dt <- rbindlist(...) command

# 3. Standardize key variables
# HRLYEARN: Usual hourly wage (employees) -- 2 decimals implied, so divide by 100!
# FINALWT: Survey weight
# Filter:    LFSSTAT == 1 or 2 (employed), MJH == 1 or 2 (jobholders only), HRLYEARN valid

dt[, HRLYEARN := as.numeric(HRLYEARN) / 100]   # Convert to numeric and dollars
dt[, FINALWT := as.numeric(FINALWT)]           # Convert weights to numeric

# 4. Generate quarter variable
dt[, SURVYEAR := as.integer(SURVYEAR)]
dt[, SURVMNTH := as.integer(SURVMNTH)]
dt[, QUARTER := paste0(SURVYEAR, "Q", ceiling(SURVMNTH / 3))]

### NEW: Calculate province-quarter population BEFORE filtering
# The key is to calculate population from the full, unfiltered sample.
# We sum the final weights (FINALWT) for each province-quarter combination.
pop_data_2 <- dt[, .(pop = sum(as.numeric(FINALWT), na.rm = TRUE)), by = .(SURVYEAR, PROV, SURVMNTH)]
pop_data_2[, QUARTER := paste0(SURVYEAR, "Q", ceiling(SURVMNTH / 3))]  # Recreate QUARTER
pop_data_2[, QUARTER_POP := .(quarter_pop = ave(pop, na.rm = TRUE)), by = .(SURVYEAR, PROV, QUARTER)]

# collapse to one row per (PROV, QUARTER)
pop_quarter <- pop_data_2[, .(QUARTER_POP = unique(QUARTER_POP)), by = .(PROV, QUARTER)]
# Keep only employed workers (at work or absent), with valid wage and jobholder status
dt <- dt[LFSSTAT %in% c("1","2") & MJH %in% c("1","2") & !is.na(HRLYEARN) & HRLYEARN > 0]


# 5. Load CPI data and process for merging
cpi <- fread("CPI_2006_2025_data_only.csv")
cpi_ref <- cpi[REF_DATE == "2016-01", VALUE]         # Reference: Jan 2016 CPI
cpi[, YEAR := as.integer(substr(REF_DATE, 1, 4))]
cpi[, MONTH := as.integer(substr(REF_DATE, 6, 7))]
setnames(cpi, "VALUE", "CPI_VAL")

# 6. Merge CPI with dt by year/month
dt <- merge(dt, cpi[, .(YEAR, MONTH, CPI_VAL)], by.x = c("SURVYEAR", "SURVMNTH"), by.y = c("YEAR", "MONTH"), all.x = TRUE)

# 7. Deflate hourly wage to 2016 dollars
dt[, HRLYEARN_2016 := HRLYEARN * (cpi_ref / CPI_VAL)]

# 8. Bin real wages in $0.25 intervals from $0 to $40
bin_cut <- seq(0, 40, by=0.25)
dt[, WAGE_BIN := cut(HRLYEARN_2016, breaks=bin_cut, right=FALSE, labels=FALSE)]

# 9. Aggregate: employment-weighted count per province/quarter/wage_bin
wage_dist <- dt[!is.na(WAGE_BIN),
                .(EMPLOYMENT = sum(FINALWT, na.rm=TRUE)),
                by = .(QUARTER, PROV, WAGE_BIN)
]

# Optionally, add wage bin midpoint for plotting or summary
wage_dist[, WAGE_MID := bin_cut[WAGE_BIN] + 0.125]

wage_dist[, `:=`(YEAR = as.integer(substr(QUARTER, 1, 4)),
                         QTR_NUM = as.integer(substr(QUARTER, 6, 6)))]
# Define the last month of each quarter to create an end-of-quarter date
wage_dist[, EOMONTH := QTR_NUM * 3]
wage_dist[, Effective_Date := as.IDate(paste(YEAR, EOMONTH, "01", sep="-"))]
wage_dist <- merge(wage_dist, pop_quarter, by = c("PROV", "QUARTER"), all.x = TRUE)


# ----- END -----

mw <- fread(
  "general-historical-minimum-wage.csv",
  select = c("Jurisdiction", "Effective Date", "Minimum Wage")
)

# 3. Parse Effective Date ("01-Apr-23" -> Date object)
# We'll use as.IDate from data.table (or as.Date if you prefer)
mw[, Effective_Date := as.IDate(`Effective Date`, format = "%d-%b-%y")]
mw[, YEAR  := year(Effective_Date)]
mw[, MONTH := month(Effective_Date)]

# 6. Merge CPI with dt by year/month
mw <- merge(mw, cpi[, .(YEAR, MONTH, CPI_VAL)], by.x = c("YEAR", "MONTH"), by.y = c("YEAR", "MONTH"), all.x = TRUE)
# 2. Clean Minimum Wage (remove $ and convert to numeric)
mw[, Minimum_Wage_Num := as.numeric(gsub("[$]", "", `Minimum Wage`))]
# 7. Deflate hourly wage to 2016 dollars
mw[, MW_Real := Minimum_Wage_Num * (cpi_ref / CPI_VAL)]


# Ensure correct column types and order by each jurisdiction's date
setorder(mw, Jurisdiction, Effective_Date)

# Compute previous MW for each jurisdiction
mw[, Prev_MW := shift(Minimum_Wage_Num, type = "lag"), by = Jurisdiction]
mw[, Prev_MW_real := shift(MW_Real, type = "lag"), by = Jurisdiction]

# Compute change from previous, per jurisdiction
mw[, DMW := Minimum_Wage_Num - Prev_MW]
mw[, DMW_real := MW_Real - Prev_MW_real]


# Keep only "significant" raises: more than $0.25 higher than the previous period
mw <- mw[is.na(Prev_MW) | DMW_real > 0.25]


mw1 <- mw[YEAR < 2020 & YEAR > 2005]
mw2 <- mw1[is.na(Prev_MW) | DMW_real > 0.25]
# Define the mapping: Jurisdiction code to Province numeric code
mw_map <- data.table(
  Jurisdiction = c("NL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC"),
  PROV = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59)
)

# Merge to add numeric code
mw <- merge(mw, mw_map, by = "Jurisdiction", all.x = TRUE)
mw[, PROV := as.integer(PROV)]  # Ensure PROV is numeric

setorder(mw, PROV, Effective_Date)

setkey(mw, PROV, Effective_Date)
setkey(wage_dist, PROV, Effective_Date)

wage_dist[, MW_CURRENT := mw[wage_dist, on=.(PROV, Effective_Date), 
                             roll=TRUE, x.Minimum_Wage_Num]]
wage_dist[, MW_CURRENT_real := mw[wage_dist, on=.(PROV, Effective_Date), 
                                  roll=TRUE, MW_Real]]
wage_dist[, DMW_real := mw[wage_dist, on=.(PROV, Effective_Date), 
                            roll=TRUE, x.DMW_real]]
wage_dist[, DMW := mw[wage_dist, on=.(PROV, Effective_Date), 
                           roll=TRUE, x.DMW]]





# Assumptions:
# - mw has: PROV, Effective_Date (IDate), Minimum_Wage_Num, MW_Change
# - wage_dist has: PROV, Effective_Date (IDate), WAGE_MID
# - Quarter indexing uses floor(event_qtr/4): tau=0 => 0..3 qtrs after hike; tau=-1 => -4..-1 qtrs, etc.

setkey(mw, PROV)
setkey(wage_dist, PROV)


# 2) Add a stable row id to wage_dist to address rows directly
wage_dist[, rid := .I]
wage_dist[, WAGEBINPROV := .GRP, by = .(PROV, WAGE_BIN)]

#wage_dist1 <- wage_dist
#wage_dist1[, fedincrease := 0]

#wage_dist1 <- make_treatment_stacks(wage_dist1, k_start = -4, wmax = 16,
#                                    panel_id = "WAGEBINPROV",
 #                                   time_var = "QTR_NUM",
  #                                  state_id = "PROV",
   #                                 col_wageM25 = "WAGE_MID",
    #                                col_MW_realM25 = "MW_CURRENT_real",
     #                               col_count = "QUARTER_POP")

#summary(wage_dist$L4treat_p1)



# 3) Cross-join wage_dist × mw within province
X <- wage_dist[mw, on = .(PROV), allow.cartesian = TRUE]

# 4) Event time in quarters and tau in years (4 quarters per year)
#    tau = floor(event_qtr/4) maps q=0..3 -> 0; q=-4..-1 -> -1; q=4..7 -> 1; etc.
X[, event_distance_in_days := as.integer((Effective_Date - i.Effective_Date) )]
X[, tau       := floor(event_distance_in_days / 365.25)]

# 5) k-bin relative to the *new* MW at the hike: [MW_new + k, MW_new + k + 1)
X[, k := floor(WAGE_MID - MW_CURRENT_real) + 1]

# 6) Keep only tau and k ranges of interest
X <- X[tau >= -3 & tau <= 4 & k >= -4 & k <= 17, .(rid, tau, k)]
X <- unique(X, by = c("rid","tau","k"))  # de-duplicate to avoid redundant setting

# 7) Initialize all dummy columns to 0 with syntactic names
for (kk in -4:17) {
  for (tt in -3:4) {
    colname <- paste0(ifelse(tt == 0, "", ifelse(tt < 0, paste0("F", 4 * abs(tt)), paste0("L", 4 * abs(tt)))),
                      "treat_", ifelse(kk < 0, paste0("m", abs(kk)), paste0("p", abs(kk)))
    )
    set(wage_dist, j = colname, value = 0L)
  }
}

# 8) Set dummies to 1 wherever there is a matching (rid, tau, k)
for (kk in -4:17) {
  for (tt in -3:4) {
    # get the corresponding column name
    colname <- paste0(ifelse(tt == 0, "", ifelse(tt < 0, paste0("F", 4 * abs(tt)), paste0("L", 4 * abs(tt)))),
                      "treat_", ifelse(kk < 0, paste0("m", abs(kk)), paste0("p", abs(kk)))
    )
    
    idx <- X[(k == kk) & tau == tt, rid]
    if (length(idx)) {
      set(wage_dist, i = idx, j = colname, value = 1L)
    }
  }
}
















setDT(wage_dist)

# --- 1) Outcome: employment share E_sjt / N_st ---
# Assume your population column is already QUARTER_POP
wage_dist[, N_st := QUARTER_POP]
wage_dist[, share := EMPLOYMENT / N_st]

wage_dist1[, N_st := QUARTER_POP]

wage_dist1[, share := EMPLOYMENT / N_st]


# --- 2) Generate syntactic dummy names (I_{tau,k}) ---
for (kk in -4:17) {
  for (tt in -3:4) {
    colname <- paste0(ifelse(tt == 0, "", ifelse(tt < 0, paste0("F", 4 * abs(tt)), paste0("L", 4 * abs(tt)))),
                      "treat_", ifelse(kk < 0, paste0("m", abs(kk)), paste0("p", abs(kk)))
    )
    # initialize to 0
    set(wage_dist, j = colname, value = 0L)
  }
}

# --- 3) Fill dummies wherever event occurs ---
# Assume X has columns: rid, k, tau (matching rows in wage_dist by rid)
for (kk in -4:17) {
  for (tt in -3:4) {
    colname <- paste0(ifelse(tt == 0, "", ifelse(tt < 0, paste0("F", 4 * abs(tt)), paste0("L", 4 * abs(tt)))),
                      "treat_", ifelse(kk < 0, paste0("m", abs(kk)), paste0("p", abs(kk)))
    )
    idx <- X[(k == kk) & tau == tt, rid]
    if (length(idx)) {
      set(wage_dist, i = idx, j = colname, value = 1L)
    }
  }
}



# main bins: m4..m1, p0..p4
for (k in c(paste0("m", 4:1), paste0("p", 0:7))) mk_window(wage_dist, k, "window_")

# placebo right tail group 1: p5..p13
for (k in paste0("p", 8:13)) mk_window(wage_dist, k, "window_")

# placebo right tail group 2: p14..p17
for (k in paste0("p", 14:17)) mk_window(wage_dist, k, "window_")

formula_parts <- get_regression_formula_parts()


all_vars <- c(formula_parts$treatafter, formula_parts$treatbefore, formula_parts$window)

# Create the formula string
rhs_formula <- paste(all_vars, collapse = " + ")
fixed_effects_formula <- paste(formula_parts$fixed_effects, collapse = " + ")

final_formula <- as.formula(
  paste("share ~", rhs_formula, "|", "PROV ^ WAGE_BIN + WAGE_BIN ^ QTR_NUM")
)


# Print the formula to check (it will now be correct)
print(final_formula)

# 3. Use the corrected formula in your event study model
library(fixest)

# This should now run without error, assuming the variables exist in final_data
model_real_treatment <- feols(
  fml = final_formula,
  data = wage_dist,
  cluster = ~PROV
)

summary(model_real_treatment)





# Figure II from a model estimated with CDLZ-style names (treat_p#, treat_m#, L4., L8., L12., L16.)
# est: a fixest (or lm) model; names(coef(est)) must include e.g. "treat_p0", "L4.treat_p0", ...
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr) 

epop<- 1
# --- Example usage ----------------------------------------------------------
res <- build_figure2_from_stata_names(model_real_treatment, bins = c(-4:-1, 0:17))
res$plot
res$data  # contains k, est, se, ci_lo, ci_hi, cum_est







# --- 4) Keep all dummies except tau = -1 as baseline ---
all_I <- grep("^I_", names(wage_dist), value = TRUE)
I_info <- data.table(var = all_I)
I_info[, c("k","tau") := tstrsplit(sub("^I_", "", var), "_", fixed = TRUE)]
I_info[, `:=`(k = as.integer(gsub("m","-",k)), tau = as.integer(gsub("m","-",tau)))]
keep_vars <- I_info[tau != -1L, var]

# --- 5) Build formula with direct interaction FEs ---
rhs <- paste(keep_vars, collapse = " + ")
fml <- as.formula(paste0("share ~ ", rhs, " | PROV + Effective_Date"))
# --- 6) Estimate regression with SE clustered by state ---
est <- feols(
  fml = fml,
  data    = wage_dist,
  cluster = ~ PROV
)

summary(est)
coefs[]






# Packages you’ll need
library(fixest)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(purrr)

# ---- 1) Parse coefficients into (bin k, event-time tau) ----
# Coef names look like: "I_<bin>_<tau>", e.g., I_m4_0, I_10_3, I_0_m2, etc.
coef_vec <- coef(est)
vcv      <- vcov(est)  # clustered VCV as used in your summary

coef_df <- tibble(term = names(coef_vec), beta = as.numeric(coef_vec)) %>%
  filter(str_detect(term, "^I_")) %>%
  tidyr::extract(
    term, into = c("bin_tok","tau_tok"),
    regex = "^I_([^_]+)_([^_]+)$", remove = FALSE
  ) %>%
  mutate(
    k   = ifelse(str_starts(bin_tok, "m"),
                 -as.integer(str_remove(bin_tok, "^m")),
                 as.integer(bin_tok)),
    tau = ifelse(str_starts(tau_tok, "m"),
                 -as.integer(str_remove(tau_tok, "^m")),
                 as.integer(tau_tok))
  ) %>%
  select(term, beta, k, tau)


# Sanity check: do we have the five post periods 0..4 for (most) bins?
# table(coef_df$tau)

# ---- 2) Build the 5-year posttreatment average per bin ----
post_years <- 0:4

# Helper to compute L' beta and its SE for a given bin:
lincomb_for_bin <- function(bin_k) {
  terms_k <- coef_df %>%
    filter(k == bin_k, tau %in% post_years) %>%
    arrange(tau)
  
  # If any post-year coefficient is missing, drop (or handle as you prefer)
  if (nrow(terms_k) == 0) return(NULL)
  
  w <- rep(1/length(post_years), length(post_years))  # equal weights (average over τ=0..4)
  # If a tau is missing, align weights to available ones (optional):
  # w <- rep(1/nrow(terms_k), nrow(terms_k))
  
  # Point estimate: L' beta
  est_val <- sum(w * terms_k$beta)
  
  # Variance: L' V L
  idx <- match(terms_k$term, colnames(vcv))
  V_sub <- vcv[idx, idx, drop = FALSE]
  se_val <- sqrt(as.numeric(t(w) %*% V_sub %*% w))
  
  tibble(
    k = bin_k,
    est = est_val,
    se  = se_val,
    ci_lo = est_val - 1.96 * se_val,
    ci_hi = est_val + 1.96 * se_val,
    n_post = nrow(terms_k)
  )
}

bins <- sort(unique(coef_df$k))
fig_df <- map_dfr(bins, lincomb_for_bin) %>%
  arrange(k) %>%
  mutate(cum_est = cumsum(est))

# ---- 3) Plot: bars (blue) + error bars + dashed red running sum ----
ggplot(fig_df, aes(x = k, y = est)) +
  geom_col(fill = "#2E86D1", width = 0.9) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2, color = "#1B4F72") +
  geom_line(aes(y = cum_est, group = 1), linetype = "dashed", size = 0.9, color = "#C0392B") +
  geom_point(aes(y = cum_est), color = "#C0392B", size = 1.4) +
  labs(
    x = "Wage bin relative to the (new) minimum wage (k)",
    y = "Average employment change over post years τ ∈ {0,1,2,3,4}",
    title = "Figure II: Employment changes by wage bin and running cumulative sum",
    subtitle = "Bars: bin-level five-year posttreatment averages; dashed line: running sum across bins"
  ) +
  theme_minimal(base_size = 12)







# 1) Your files
csv_files <- dir_ls(getwd(), recurse = TRUE, regexp = "pub\\d{4}\\.csv$")

# 2) Columns you want (UPPERCASE, as used in your code)
WANTED <- c("PROV","SURVYEAR","SURVMNTH","FINALWT","HRLYEARN","LFSSTAT","COWMAIN", "MJH")

read_needed_upper <- function(f, wanted_upper = WANTED) {
  # Peek header
  hdr <- names(fread(f, nrows = 0, showProgress = FALSE))
  # Map UPPERCASE -> actual header in file
  map <- setNames(hdr, toupper(trimws(hdr)))
  
  # Which wanted columns exist in this file?
  present_actual <- map[wanted_upper]           # values are actual names in the file
  present_actual <- present_actual[!is.na(present_actual)]
  if (!length(present_actual)) {
    stop(sprintf("No wanted columns found in %s", f))
  }
  
  
  # Read only the present columns (by their actual names)
  dt <- fread(f, select = unname(present_actual),  showProgress = FALSE)
  

  # Ensure all wanted columns exist; create missing ones as NA
  missing <- setdiff(wanted_upper, names(dt))
  if (length(missing)) for (m in missing) dt[, (m) := NA]
  
  # Keep a stable column order
  setcolorder(dt, wanted_upper)
  dt[]
}

# 3) Read & merge
dt_list <- lapply(csv_files, read_needed_upper, wanted_upper = WANTED)
dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

# dt now has columns named in UPPERCASE:
# PROV, SURVYEAR, SURVMNTH, FINALWT, HRLYEARN, LFSSTAT, COWMAIN, MJH
