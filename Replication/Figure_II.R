# Ensure the necessary packages are loaded
library(dplyr)
library(haven)

# Assuming 'dt' and 'qcew_data' are your data frames

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
    -starts_with("teen")
  )




#' Generate Regression Formula Components
#'
#' This function replicates the logic of the Stata `treatmentcontrolwindows`
#' program when called with its default parameters. It returns a list
#' containing character vectors of variable names for the regression.
#'
#' @return A list with the following elements:
#'         - treatafter: Core event-study leads and lags.
#'         - treatbefore: Variables for testing pre-trends.
#'         - control: Control variables for state-level MW changes.
#'         - controlf: Control variables for federal-level MW changes.
#'         - window: Additional windowed control variables.
#'         - fixed_effects: The two-way fixed effects.
get_regression_formula_parts <- function() {
  
  # --- Set default parameters from the Stata script ---
  Tmax <- 16
  Tmin <- 12
  Wmax <- 4
  Wmin <- 4
  
  # --- 1. Build `$treatafter` ---
  # These are the primary coefficients for the event study plot
  treatafter_vars <- character(0)
  
  # Loop over event time (k)
  for (k in seq(0, Tmax, by = 4)) {
    time_prefix <- if (k == 0) "" else paste0("L", k)
    
    # Loop over wage bins (j)
    bins_m <- paste0("treat_m", Wmin:1)
    bins_p <- paste0("treat_p", 0:Wmax)
    
    treatafter_vars <- c(treatafter_vars, paste0(time_prefix, c(bins_m, bins_p)))
  }
  
  # --- 2. Build `$treatbefore` ---
  # These are for the pre-trend test
  treatbefore_vars <- character(0)
  
  # Loop over pre-event time (k)
  for (k in seq(-Tmin, -8, by = 4)) {
    time_prefix <- paste0("F", -k) # Note: F for "Forward" operator
    
    bins_m <- paste0("treat_m", Wmin:1)
    bins_p <- paste0("treat_p", 0:Wmax)
    
    treatbefore_vars <- c(treatbefore_vars, paste0(time_prefix, c(bins_m, bins_p)))
  }
  
  # --- 3. Build `$control` ---
  # Based on defaults: disagg="N", controlmethod="Absorb", contlead="Y"
  control_vars <- c("postcont_m", "precont_m", "earlycont_m",
                    "postcont_p", "precont_p", "earlycont_p")
  
  # --- 4. Build `$controlf` ---
  # Based on defaults: contlead="Y"
  controlf_vars <- c("postcontf_m", "precontf_m", "earlycontf_m",
                     "postcontf_p", "precontf_p", "earlycontf_p")
  
  # --- 5. Build `$window` ---
  window_bins_m <- paste0("window_m", Wmin:1)
  window_bins_p <- paste0("window_p", 0:Wmax)
  window_vars <- paste0("i.one#c.", c(window_bins_m, window_bins_p)) # Stata interaction syntax
  # For R, you'd likely just use the variable names directly if they are continuous
  # Or create the interactions manually if needed. For now, let's keep the core name.
  simple_window_vars <- c(window_bins_m, window_bins_p)
  
  # --- 6. Define Fixed Effects ---
  fixed_effects_vars <- c("wagebinstate", "wagequarterdate")
  
  # --- Return all parts in a named list ---
  return(
    list(
      treatafter = treatafter_vars,
      treatbefore = treatbefore_vars,
      control = control_vars,
      controlf = controlf_vars,
      window = simple_window_vars,
      fixed_effects = fixed_effects_vars
    )
  )
}

# --- How to Use This with Your Regression ---
# 1. Generate the list of variable names (no changes here)
formula_parts <- get_regression_formula_parts()

# 2. Assemble the formula for the "real" treatment regression
# THE FIX IS HERE: Remove "one" from the vector of variables.
all_vars <- formula_parts$treatafter

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
  data = final_data,
  weights = ~wtoverall1979,
  cluster = ~statenum
)

summary(model_real_treatment)

# install.packages("broom")
# install.packages("ggplot2")
# install.packages("stringr") # For string manipulation
library(broom)
library(ggplot2)
library(stringr)

# --- Final Step: Aggregate Coefficients and Create Figure II ---

# 1. Tidy the model output to get a clean data frame of coefficients
model_tidy <- tidy(model_real_treatment, conf.int = TRUE)

# 2. Extract the variance-covariance matrix for correct SE calculation
vcov_matrix <- vcov(model_real_treatment)

# 3. Identify the wage bins and time horizons from the coefficient names
#    Example term: L4treat_m2 -> time=4, bin_type="m", bin_num=2
results_df <- model_tidy %>%
  filter(str_detect(term, "treat_")) %>% # Keep only our treatment variables
  mutate(
    # Extract time horizon (0 for contemporaneous, 4 for L4, etc.)
    time = as.numeric(str_extract(term, "(?<=L)\\d+")),
    time = ifelse(is.na(time), 0, time),
    
    # Extract bin type (p or m)
    bin_type = str_extract(term, "(?<=treat_)[mp]"),
    
    # Extract bin number
    bin_num = as.numeric(str_extract(term, "\\d+$")),
    
    # Recreate the bin identifier, like "-2" for m2 or "+1" for p1
    wage_bin = ifelse(bin_type == "m", -bin_num, bin_num)
  )

# --- 4. Calculate the aggregate effect and standard error for each wage bin ---

# The warning message from dplyr suggests using reframe() for this type of
# complex summary, but for clarity, we can stick with summarise() and
# ensure our logic is sound.

aggregated_results <- results_df %>%
  group_by(wage_bin) %>%
  # The summarise block calculates the main point estimate and its standard error
  summarise(
    avg_effect = sum(estimate),
    
    # Get the coefficient names for the current group to subset the vcov matrix
    terms_in_group = list(term),
    vcov_subset = list(vcov(model_real_treatment)[terms_in_group[[1]], terms_in_group[[1]]]),
    
    # The variance of the sum is the sum of all elements in this vcov sub-matrix
    var_of_sum = sum(vcov_subset[[1]]),
    
    # SE of the sum = sqrt of its variance. SE of the average = SE of sum / N
    se_of_avg = sqrt(var_of_sum) / n(),
    
    .groups = 'drop' # Ungroup after summarising
  ) %>%
  
  # --- THE FIX IS HERE ---
  # First, arrange the data frame by wage_bin.
  # This is critical for the cumulative sum to be calculated in the correct order.
  arrange(wage_bin) %>%
  
  # Now, use mutate() to add the confidence intervals and the cumulative sum.
  mutate(
    conf.low = avg_effect - 1.96 * se_of_avg,
    conf.high = avg_effect + 1.96 * se_of_avg,
    cumulative_effect = cumsum(avg_effect) # This now works on the sorted data
  )

# Check the first few rows of the final aggregated data frame
print(head(aggregated_results))


# --- 5. Generate the final plot (Figure II) ---

# We need to manually set the x-axis labels to match the paper's style
x_axis_labels <- setNames(
  paste0("$", aggregated_results$wage_bin), # Labels like "$ -4", "$0", "$4"
  aggregated_results$wage_bin             # Positions on the axis
)
# Add the "+"" for the highest bin if it exists (e.g., bin 4 becomes "$4+")
if (max(aggregated_results$wage_bin) == 4) {
  x_axis_labels[as.character(max(aggregated_results$wage_bin))] <- "$4+"
}


figure_II_plot <- ggplot(aggregated_results, aes(x = factor(wage_bin))) +
  
  # Blue bars for the average effect in each bin
  geom_col(aes(y = avg_effect), fill = "#56B4E9", alpha = 0.8) +
  
  # Error bars for the 95% confidence interval
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "#0072B2") +
  
  # Red dashed line for the running sum of employment changes
  # We need to use aes(group = 1) to tell ggplot to draw a single line across all x-axis points
  geom_line(aes(y = cumulative_effect, group = 1), color = "#D55E00", linetype = "dashed", size = 1) +
  
  # Add a horizontal line at y=0 for reference
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  
  # Formatting to match the academic style of the paper
  scale_x_discrete(labels = x_axis_labels) +
  labs(
    title = "Impact of Minimum Wages on the Wage Distribution",
    subtitle = "Event Study Estimates of Employment Changes by Wage Bin",
    x = "Wage Bins in $ Relative to New Minimum Wage",
    y = "Change in Employment Share"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

# Display the plot
print(figure_II_plot)

# --- Final Step: Aggregate and Scale Coefficients (Corrected to Average) ---

# NOTE: You must calculate the correct 'E' normalization factor from your data.
E_normalization_factor <- 0.02 # Placeholder value

# 1. Tidy model output and get VCV matrix (no change)
model_tidy <- tidy(model_real_treatment, conf.int = TRUE)
vcov_matrix <- vcov(model_real_treatment)

# 2. Extract bin and time info (no change)
results_df <- model_tidy %>%
  filter(str_detect(term, "treat_")) %>%
  mutate(
    time = as.numeric(str_extract(term, "(?<=L)\\d+")),
    time = ifelse(is.na(time), 0, time),
    bin_type = str_extract(term, "(?<=treat_)[mp]"),
    bin_num = as.numeric(str_extract(term, "\\d+$")),
    wage_bin = ifelse(bin_type == "m", -bin_num, bin_num)
  )

# --- 3. Calculate the Final Average Effect and Its Standard Error ---
aggregated_results_final <- results_df %>%
  group_by(wage_bin) %>%
  summarise(
    # --- THIS IS THE FIX ---
    # We create the linear combination of coefficients for the average effect.
    # The combination is (1/5)*coef_t0 + (1/5)*coef_t4 + ...
    # The scaling factors are applied to the whole combination.
    
    # Get the names of the coefficients in the current group
    terms_in_group = list(term),
    
    # Calculate the average effect using the 'lincom' logic from Stata
    # Sum the estimates, then multiply by the denominator (0.2) and scaling factors
    avg_effect = (sum(estimate) * 0.2) * 4 * (1 / E_normalization_factor),
    
    # --- Calculate the Standard Error of this combination ---
    # Create a vector of weights for the linear combination (0.2 for each term)
    comb_weights = rep(0.2, n()),
    
    # Subset the variance-covariance matrix
    vcov_subset = vcov_matrix[terms_in_group[[1]], terms_in_group[[1]]],
    
    # Calculate the variance of the linear combination: w' * V * w
    # And then apply the scaling factors to the standard error
    se_of_avg = sqrt(t(comb_weights) %*% vcov_subset %*% comb_weights) * 4 * (1 / E_normalization_factor),
    
    .groups = 'drop'
  ) %>%
  arrange(wage_bin) %>%
  mutate(
    conf.low = avg_effect - 1.96 * se_of_avg,
    conf.high = avg_effect + 1.96 * se_of_avg,
    cumulative_effect = cumsum(avg_effect)
  )

# --- 4. Generate the Plot ---
# The plotting code is the same, just ensure it uses `avg_effect` from this final data frame.
# (Plotting code omitted for brevity, it remains unchanged from the previous correct version)


# --- 5. Generate the Final Plot ---
# (The plotting code remains the same as before, but uses this new data frame)

figure_II_final_plot <- ggplot(aggregated_results_final, aes(x = factor(wage_bin))) +
  geom_col(aes(y = avg_effect), fill = "#56B4E9", alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "#0072B2") +
  geom_line(aes(y = cumulative_effect, group = 1), color = "#D55E00", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Impact of Minimum Wages on the Wage Distribution",
    subtitle = "Average 5-Year Employment Changes by Wage Bin",
    x = "Wage Bins in $ Relative to New Minimum Wage",
    y = "Change in Employment Share"
  ) +
  theme_minimal(base_size = 14)

print(figure_II_final_plot)
