make_treatment_stacks <- function(dt,
                                  k_start = 8, wmax = 17,
                                  panel_id = "WAGEBINSTATE",
                                  time_var = "QUARTERDATE",
                                  state_id = "STATENUM",
                                  col_MW_real = "MW_real",
                                  col_MW      = "MW",
                                  col_wageM25 = "wageM25",
                                  col_MW_realM25 = "MW_realM25",
                                  col_count   = "overallcountgroup",
                                  col_fed     = "fedincrease") {
  
  dt <- as.data.table(dt)
  
  # Ensure proper ordering for shift() within groups
  setkeyv(dt, c(panel_id, time_var))
  
  
  # Helper to safely coerce logical to integer (0/1), with NA -> 0
  to_byte <- function(x) as.integer(replace(x, is.na(x), 0))
  
  # --- Build _treat_pk, treat_pk, and F/L stacks for k = 8..wmax ---
  for (k in k_start:wmax) {
    # 1) `_treat_pk` at the *event quarter* (the ifclause)
    #nm_tmp <- paste0("temptreat_", ifelse(k < 0, paste0("m", abs(k)), paste0("p", abs(k))))
    nm_tmp  <- sprintf("temptreat_p%d", k)
    dt[, (nm_tmp) := as.integer(
      ( get("DMW_real") >  0.25 ) &
        (!is.na(get("DMW_real"))) &
        ( get("DMW")      >  0    ) &
        ( get(col_wageM25) >= get(col_MW_realM25) + k ) &
        ( get(col_wageM25) <  get(col_MW_realM25) + k + 1 ) &
        ( get(col_count) > 0 ) &
        ( get(col_fed) != 1 )
    )]
    
    # 2) `treat_pk` = current + 3 lags of `_treat_pk` within the panel
    nm_treat <- sprintf("treat_p%d", k)
#    nm_treat < paste0("treat_", ifelse(k < 0, paste0("m", abs(k)), paste0("p", abs(k))))
    dt[, (nm_treat) := 
         get(nm_tmp) +
         shift(get(nm_tmp), 1L, type = "lag") +
         shift(get(nm_tmp), 2L, type = "lag") +
         shift(get(nm_tmp), 3L, type = "lag"),
       by = panel_id]
    # Replace NAs from the shifts with 0 (Stata: missing -> 0)
    dt[is.na(get(nm_treat)), (nm_treat) := 0L]
    
    # 3) Leads/Lags at {4,8,12,16} quarters on `treat_pk`, NA -> 0
    for (j in c(4L, 8L, 12L, 16L)) {
      nm_F <- sprintf("F%dtreat_p%d", j, k)
      #nm_F  <- paste0("F", j, "treat_", ifelse(k < 0, paste0("m", abs(k)), paste0("p", abs(k))))
      nm_L <- sprintf("L%dtreat_p%d", j, k)
      #nm_L  <- paste0("L", j, "treat_", ifelse(k < 0, paste0("m", abs(k)), paste0("p", abs(k))))
      dt[, (nm_F) := shift(get(nm_treat),  j, type = "lead"), by = panel_id]
      dt[, (nm_L) := shift(get(nm_treat),  j, type = "lag"),  by = panel_id]
      dt[is.na(get(nm_F)), (nm_F) := 0L]
      dt[is.na(get(nm_L)), (nm_L) := 0L]
    }
    
    # 4) Drop the temporary `_treat_pk` (as in Stata)
    dt[, (nm_tmp) := NULL]
  }
  
  dt[]
}


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
  Wmax <- 16
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






# --- Helpers ----------------------------------------------------------------
# Map an integer bin k to the base regressor name used in the model
# k >= 0 -> "treat_p{k}", k < 0 -> "treat_m{abs(k)}"
base_name_for_bin <- function(k) {
  if (k >= 0) sprintf("treat_p%d", k) else sprintf("treat_m%d", abs(k))
}

# Return the 5 post-year term names for a bin k: tau=0, +1y, +2y, +3y, +4y
terms_for_bin <- function(k, post_quarters = c(0, 4, 8, 12, 16)) {
  base <- base_name_for_bin(k)
  c(if (0 %in% post_quarters) base else NULL,
    sprintf("L%d.%s", setdiff(post_quarters, 0), base))
}

# Compute L' beta and SE for a given bin k (equal weights over available post years)
lincomb_bin_avg <- function(est, k, post_quarters = c(0, 4, 8, 12, 16), warn_drop = TRUE) {
  beta <- coef(est)
  vcv  <- vcov(est)
  trms <- terms_for_bin(k, post_quarters)
  
  # Keep only terms that actually exist in the model
  idx  <- match(trms, names(beta))
  ok   <- !is.na(idx)
  if (!any(ok)) {
    if (warn_drop) warning(sprintf("No post-year terms found for bin k=%s", k))
    return(tibble(k = k, est = NA_real_, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, n_post = 0L))
  }
  if (any(!ok) && warn_drop) {
    warning(sprintf("Dropping %d missing post-year terms for bin k=%s: %s",
                    sum(!ok), k, paste(trms[!ok], collapse = ", ")))
  }
  
  idx <- idx[ok]
  w   <- rep(1/length(idx), length(idx))             # equal-weight average over available post years
  b   <- as.numeric(beta[idx])
  V   <- as.matrix(vcv[idx, idx, drop = FALSE])
  
  est_val <- sum(w * b * 4 / epop)
  se_val  <- sqrt(as.numeric(t(w) %*% V %*% w))
  
  tibble(
    k = k, est = est_val, se = se_val,
    ci_lo = est_val - 1.96 * se_val,
    ci_hi = est_val + 1.96 * se_val,
    n_post = length(idx)
  )
}

# --- Main function ----------------------------------------------------------
build_figure2_from_stata_names <- function(est,
                                           bins = c(-4:-1, 0:17),        # adjust to your range
                                           post_quarters = c(0, 4, 8, 12, 16)) {
  
  fig_df <- map_dfr(bins, ~lincomb_bin_avg(est, .x, post_quarters = post_quarters)) %>%
    arrange(k) %>%
    mutate(cum_est = cumsum(replace_na(est, 0)))
  
  # Plot
  p <- ggplot(fig_df, aes(x = k, y = est)) +
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
}




library(data.table)

make_treatment_stacks_intticks <- function(dt,
                                           k_start = 8, wmax = 17,
                                           panel_id = "WAGEBINSTATE",
                                           time_var = "QUARTERDATE",
                                           state_id = "STATENUM",
                                           col_MW_real = "MW_real",
                                           col_MW      = "MW",
                                           col_wageM25 = "wageM25",
                                           col_MW_realM25 = "MW_realM25",
                                           col_count   = "overallcountgroup",
                                           col_fed     = "fedincrease",
                                           d_mw_real = "DMW_real",
                                           d_mw = "DMW") {
  
  dt <- as.data.table(dt)
  setkeyv(dt, c(panel_id, time_var))
  
  # First differences by state (event-quarter flag)
  d_mw_real <- "D__MW_real"; d_mw <- "D__MW"
  dt[order(get(state_id), get(time_var)),
     c(d_mw_real, d_mw) := .(
       get(col_MW_real) - shift(get(col_MW_real), 1L),
       get(col_MW)      - shift(get(col_MW),      1L)
     ),
     by = state_id]
  
  # Convert to integer ticks (25¢ units)
  dt[, w25  := as.integer(round(4 * get(col_wageM25)))]
  dt[, mw25 := as.integer(round(4 * get(col_MW_realM25)))]
  dt[, d25  := w25 - mw25]  # difference in ticks
  
  # Event-quarter clause (CDLZ treatdef3 + filters)
  evt <- (dt[[d_mw_real]] > 0.25) & !is.na(dt[[d_mw_real]]) & (dt[[d_mw]] > 0) &
    (dt[[col_count]] > 0) & (dt[[col_fed]] != 1)
  
  for (k in k_start:wmax) {
    nm_treat <- sprintf("treat_p%d", k)
    
    # In-tick membership: d25 ∈ {4k, 4k+1, 4k+2, 4k+3}
    hit <- evt & (dt$d25 >= 4L*k) & (dt$d25 <= 4L*k + 3L)
    
    # contemporaneous flag in event quarter
    dt[, (nm_treat) := as.integer(hit)]
    
    # τ = 0 "year" = current + 3 lags within the wage-bin×state panel
    dt[, (nm_treat) :=
         get(nm_treat) +
         shift(get(nm_treat), 1L, type = "lag") +
         shift(get(nm_treat), 2L, type = "lag") +
         shift(get(nm_treat), 3L, type = "lag"),
       by = panel_id]
    dt[is.na(get(nm_treat)), (nm_treat) := 0L]
    
    # Leads/lags at 4,8,12,16 quarters (NA -> 0)
    for (j in c(4L, 8L, 12L, 16L)) {
      dt[, (sprintf("F%dtreat_p%d", j, k)) := shift(get(nm_treat), j, type = "lead"), by = panel_id]
      dt[, (sprintf("L%dtreat_p%d", j, k)) := shift(get(nm_treat), j, type = "lag"),  by = panel_id]
      dt[is.na(get(sprintf("F%dtreat_p%d", j, k))), (sprintf("F%dtreat_p%d", j, k)) := 0L]
      dt[is.na(get(sprintf("L%dtreat_p%d", j, k))), (sprintf("L%dtreat_p%d", j, k)) := 0L]
    }
    
    # simple diagnostic print
    cat(sprintf("k=%2d  hits(evt & bin): %d\n", k, sum(hit, na.rm = TRUE)))
  }
  
  # clean up helper columns if you want:
  # dt[, c("w25","mw25","d25") := NULL]
  
  dt[]
}


# ********************************************************************************
# ******************    Figure Creation Functions    ***************************
# ********************************************************************************

#' Create mass over time figure (before period)
#' Equivalent to Stata's makefigure_massovertime_before program
#' @param model_obj Model object
#' @param tmin Minimum time periods backward (default 12)
#' @param tmax Maximum time periods forward (default 16)
makefigure_massovertime_before <- function(model_obj, tmin = 12, tmax = 16) {
  
  # Extract coefficients and variance matrix
  coefs <- coef(model_obj)
  vcov_mat <- vcov(model_obj)
  
  eventmat <- NULL
  
  # Loop through before periods
  for (t in seq(-tmin, -4, 4)) {
    nt <- -t
    
    # Calculate above mass using actual treatment coefficients
    # Get treatment variables for this period
    above_vars <- paste0("F", nt, "treat_p", 0:4)
    below_vars <- paste0("F", nt, "treat_m", 1:4)
    
    # Calculate above mass effect
    ab_coefs <- coefs[intersect(above_vars, names(coefs))]
    ab <- ifelse(length(ab_coefs) > 0, sum(ab_coefs, na.rm = TRUE), 0)
    
    # Calculate standard error for above mass
    ab_indices <- match(intersect(above_vars, names(coefs)), names(coefs))
    ase <- ifelse(length(ab_indices) > 0, sqrt(sum(vcov_mat[ab_indices, ab_indices], na.rm = TRUE)), 0)
    
    # Calculate below mass effect
    bb_coefs <- coefs[intersect(below_vars, names(coefs))]
    bb <- ifelse(length(bb_coefs) > 0, sum(bb_coefs, na.rm = TRUE), 0)
    
    # Calculate standard error for below mass
    bb_indices <- match(intersect(below_vars, names(coefs)), names(coefs))
    bse <- ifelse(length(bb_indices) > 0, sqrt(sum(vcov_mat[bb_indices, bb_indices], na.rm = TRUE)), 0)
    
    # Build event matrix
    row <- c(t, ab, ab - 1.96*ase, ab + 1.96*ase, bb, bb - 1.96*bse, bb + 1.96*bse)
    if (is.null(eventmat)) {
      eventmat <- matrix(row, nrow = 1)
    } else {
      eventmat <- rbind(eventmat, row)
    }
  }
  
  return(eventmat)
}

#' Create mass over time figure (after period)
#' Equivalent to Stata's makefigure_massovertime_after program
#' @param model_obj Model object
#' @param eventmat_before Event matrix from before period
#' @param tmin Minimum time periods backward (default 12)
#' @param tmax Maximum time periods forward (default 16)
makefigure_massovertime_after <- function(model_obj, eventmat_before, tmin = 12, tmax = 16) {
  
  # Extract coefficients and variance matrix
  coefs <- coef(model_obj)
  vcov_mat <- vcov(model_obj)
  
  eventmat <- eventmat_before
  
  # Loop through after periods
  for (t in seq(0, tmax, 4)) {
    # Get treatment variables for this period
    if (t == 0) {
      above_vars <- paste0("treat_p", 0:4)
      below_vars <- paste0("treat_m", 1:4)
    } else {
      above_vars <- paste0("L", t, "treat_p", 0:4)
      below_vars <- paste0("L", t, "treat_m", 1:4)
    }
    
    # Calculate above mass effect
    ab_coefs <- coefs[intersect(above_vars, names(coefs))]
    ab <- ifelse(length(ab_coefs) > 0, sum(ab_coefs, na.rm = TRUE), 0)
    
    # Calculate standard error for above mass
    ab_indices <- match(intersect(above_vars, names(coefs)), names(coefs))
    ase <- ifelse(length(ab_indices) > 0, sqrt(sum(vcov_mat[ab_indices, ab_indices], na.rm = TRUE)), 0)
    
    # Calculate below mass effect
    bb_coefs <- coefs[intersect(below_vars, names(coefs))]
    bb <- ifelse(length(bb_coefs) > 0, sum(bb_coefs, na.rm = TRUE), 0)
    
    # Calculate standard error for below mass
    bb_indices <- match(intersect(below_vars, names(coefs)), names(coefs))
    bse <- ifelse(length(bb_indices) > 0, sqrt(sum(vcov_mat[bb_indices, bb_indices], na.rm = TRUE)), 0)
    
    row <- c(t, ab, ab - 1.96*ase, ab + 1.96*ase, bb, bb - 1.96*bse, bb + 1.96*bse)
    eventmat <- rbind(eventmat, row)
  }
  
  # Convert to data frame for plotting
  colnames(eventmat) <- c("time", "estA", "lowA", "highA", "estB", "lowB", "highB")
  df <- as.data.frame(eventmat)
  
  # Create ggplot with improved appearance
  p <- ggplot(df, aes(x = time)) +
    geom_line(aes(y = estA), color = "blue", size = 1) +
    geom_ribbon(aes(ymin = lowA, ymax = highA), alpha = 0.3, fill = "blue") +
    geom_line(aes(y = estB), color = "red", size = 1) +
    geom_ribbon(aes(ymin = lowB, ymax = highB), alpha = 0.3, fill = "red") +
    geom_point(aes(y = estA), color = "blue", size = 2) +
    geom_point(aes(y = estB), color = "red", size = 2) +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    labs(x = "Quarters relative to minimum wage change",
         y = "Employment effect",
         title = "Employment Effects Around Minimum Wage Changes") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# helper to build one window column safely (indicator = any of the post dummies is 1)
mk_window <- function(final_dt, k, prefix = "window_") {
  post_terms <- paste0(c("", "L4", "L8", "L12", "L16"), "treat_", k)
  
  pre_terms <- paste0(c("F4", "F8", "F12"), "treat_", k)
  newname <- paste0(prefix, k)
  
  final_dt[, (newname) :=  get(pre_terms[1]) + get(pre_terms[2]) + get(pre_terms[3]) +
             get(post_terms[1]) + get(post_terms[2]) + get(post_terms[3]) + get(post_terms[4]) + get(post_terms[5])]
}

