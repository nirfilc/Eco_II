# Ensure the necessary packages are loaded
library(dplyr)
library(haven)
# Load required libraries
library(fixest)
library(data.table)
library(ggplot2)

setwd("C://Users//t-nirfilc//OneDrive - Microsoft//Desktop//Eco II")
source("Eco_II//Replication//figure_ii_helpers.R")


# Load QCEW data
message("Loading QCEW data...")
qcew <- read_dta(file.path(getwd(), "data/qcew_state_overall_counts_2016.dta"))
dt <- read_dta(file.path(getwd(), "data/state_panels_with3quant1979.dta"))
qcew_data <-read_dta(file.path(getwd(), "data/qcew_multiplier.dta"))
qcew <- qcew %>%
  rename(statenum = statefips) %>%
  mutate(quarterdate = 4 * (year - 1960) + quarter - 3)  # Convert to quarterly date


final_data <- dt %>%
  
  # Stata: merge m:1 statenum quarterdate using `qcew', assert(3) nogenerate
  left_join(qcew_data, by = c("statenum", "quarterdate")) %>%
  
  # Assert check
  {
    if (any(is.na(.$multiplier))) {
      warning("Warning: Some rows in dt did not find a match in qcew_data.")
    }
    .
  } %>%
  
  # Stata: replace count = count*multiplier (and others)
  mutate(
    count = count * multiplier,
    overallcountpc = overallcountpc * multiplier,
    overallcountpcall = overallcountpcall * multiplier
  ) %>%
  
  # Stata: cap drop HSD* HSL* ...
  # THE FIX IS HERE: Use dplyr::select() instead of just select()
  dplyr::select(
    -starts_with("HSD"),
    -starts_with("HSL"),
    -starts_with("black"),
    -starts_with("female"),
    -starts_with("BH"),
    -starts_with("teen"),
    -starts_with("white"),
    -starts_with("gender"),
  )


rm(dt)
######## Create treatment vars for "placebo"
# --- Example usage ---
 final_dt <- make_treatment_stacks(final_data, k_start = 8, wmax = 17,
                             panel_id = "wagebinstate",
                             time_var = "quarterdate",
                             state_id = "statenum")
 
 
 # placebo right tail group 1: p5..p13
 for (k in paste0("p", 0:7)) mk_window(final_dt, k, "window_")
 
 # placebo right tail group 1: p5..p13
 for (k in paste0("p", 8:13)) mk_window(final_dt, k, "window_")
 
 # placebo right tail group 2: p14..p17
 for (k in paste0("p", 14:17)) mk_window(final_dt, k, "window_")
 
 
rm(final_data)


# --- inputs: change these names if your columns differ ---
panel_id   <- "wagebinstate"   # panel for leads (same unit as Stata's xtset panel)
time_var   <- "quarterdate"    # time index used for F. leads
y_col      <- "overallcountpcall"
gr_col     <- "overallcountgroup"
fed_col    <- "fedincrease"
year_col   <- "year.x"
clean_col  <- "cleansample"
weight_col <- "wtoverall1979" 

setkeyv(final_dt, c(panel_id, time_var))

# Leads for the next 1..4 quarters (Stata's F., F2., F3., F4.) within the panel
final_dt[, `:=`(
  gr_F1  = shift(get(gr_col),  1, type = "lead"),
  gr_F2  = shift(get(gr_col),  2, type = "lead"),
  gr_F3  = shift(get(gr_col),  3, type = "lead"),
  gr_F4  = shift(get(gr_col),  4, type = "lead"),
  fed_F1 = shift(get(fed_col), 1, type = "lead"),
  fed_F2 = shift(get(fed_col), 2, type = "lead"),
  fed_F3 = shift(get(fed_col), 3, type = "lead"),
  fed_F4 = shift(get(fed_col), 4, type = "lead")
), by = panel_id]

# Stata's IF clause:
# (F.gr>0 & F.gr!=. & F.fed!=1 & F.fed!=.)  OR  same for F2, F3, F4
keep_future <-
  (final_dt$gr_F1 > 0 & !is.na(final_dt$gr_F1) & final_dt$fed_F1 != 1 & !is.na(final_dt$fed_F1)) |
  (final_dt$gr_F2 > 0 & !is.na(final_dt$gr_F2) & final_dt$fed_F2 != 1 & !is.na(final_dt$fed_F2)) |
  (final_dt$gr_F3 > 0 & !is.na(final_dt$gr_F3) & final_dt$fed_F3 != 1 & !is.na(final_dt$fed_F3)) |
  (final_dt$gr_F4 > 0 & !is.na(final_dt$gr_F4) & final_dt$fed_F4 != 1 & !is.na(final_dt$fed_F4))

# Full filter: year >= 1979 & cleansample == 1 & the future-quarters condition
ok <- keep_future & (final_dt[[year_col]] >= 1979) & (final_dt[[clean_col]] == 1)

# Weighted mean of overallcountpcall (Stata's "sum ... [aw=weights]" then r(mean))
epop <- with(final_dt[ok],
             stats::weighted.mean(get(y_col), get(weight_col), na.rm = TRUE))


# --- How to Use This with Your Regression ---
# 1. Generate the list of variable names (no changes here)
formula_parts <- get_regression_formula_parts()
formula_parts$controlf




# 2. Assemble the formula for the "real" treatment regression
all_vars <- c(formula_parts$treatafter, formula_parts$treatbefore, formula_parts$control,
              formula_parts$controlf, formula_parts$window)

# Create the formula string
rhs_formula <- paste(all_vars, collapse = " + ")
fixed_effects_formula <- paste(formula_parts$fixed_effects, collapse = " + ")

final_formula <- as.formula(
  paste("overallcountpc ~", rhs_formula, "|", fixed_effects_formula)
)

# Print the formula to check (it will now be correct)
print(final_formula)

# 3. Use the corrected formula in your event study model
library(fixest)

# This should now run without error, assuming the variables exist in final_data
model_real_treatment <- feols(
  fml = final_formula,
  data = final_dt,
  weights = ~wtoverall1979,
  cluster = ~statenum
)

summary(model_real_treatment)





# Figure II from a model estimated with CDLZ-style names (treat_p#, treat_m#, L4., L8., L12., L16.)
# est: a fixest (or lm) model; names(coef(est)) must include e.g. "treat_p0", "L4.treat_p0", ...
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr) 

# --- Example usage ----------------------------------------------------------
res <- build_figure2_from_stata_names(model_real_treatment, bins = c(-4:-1, 0:17), post_quarters = (0))
res$plot
    res$data  # contains k, est, se, ci_lo, ci_hi, cum_est

summary(final_dt$treat_p0)
summary(final_dt$treat_m2)
summary(final_dt$treat_p10)


###### END FOR NOW #########

# Mass over time figures
eventmat_before <- makefigure_massovertime_before(model_before)
figure_massovertime <- makefigure_massovertime_after(model_after, eventmat_before)
graphexportpdf(figure_massovertime, file.path(figures_path, "Figure3.pdf"))


# est is your fitted model (fixest::feols)
ct <- as.data.frame(summary(model_real_treatment)$coeftable)
ct$term <- rownames(ct)
ct <- ct[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
write.csv(ct, "coefficients.csv", row.names = FALSE)
getw



fig_df <- map_dfr(bins, ~lincomb_bin_avg(model_real_treatment, .x, post_quarters = post_quarters)) %>%
  arrange(k) %>%
  mutate(cum_est = cumsum(replace_na(model_real_treatment, 0)))

# Plot
p <- ggplot(fig_df, aes(x = k, y = model_real_treatment)) +
  geom_col(width = 0.9, fill = "#2E86D1") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2, color = "#1B4F72") +
  geom_line(aes(y = cum_est, group = 1), linetype = "dashed", size = 0.9, color = "#C0392B") +
  geom_point(aes(y = cum_est), color = "#C0392B", size = 1.3) +
  labs(
    x = "Wage bin relative to the new minimum (k)",
    y = "Avg. employment change, post years τ ∈ {0..4}",
    title   = "Figure II (CDLZ naming): bin-level five-year averages",
    subtitle= "Bars: per-bin average of {treat_•, L4., L8., L12., L16.}; dashed: running cumulative sum"
  ) +
  theme_minimal(base_size = length(bins))

list(data = fig_df, plot = p)




need <- unlist(lapply(8:16, function(k)
  c(sprintf("treat_p%d", k),
    sprintf("L4treat_p%d", k),  sprintf("L8treat_p%d", k),
    sprintf("L12treat_p%d", k), sprintf("L16treat_p%d", k),
    sprintf("F8treat_p%d", k),  sprintf("F12treat_p%d", k))
))
colSums(final_dt[, need, with = FALSE] != 0, na.rm = TRUE)

