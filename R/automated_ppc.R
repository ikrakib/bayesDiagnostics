#' Automated Posterior Predictive Checks
#'
#' Automatically computes a suite of posterior predictive checks to diagnose
#' model fit. It compares observed data against posterior predictive samples
#' across multiple statistics (mean, sd, min, max, skewness, kurtosis).
#'
#' @param model A fitted brmsfit object.
#' @param observed_data Numeric vector of observed data.
#' @param n_samples Integer. Number of posterior draws to use (default: 1000).
#' @param p_value_threshold Numeric. Threshold for flagging extreme p-values (default: 0.05).
#'
#' @return A list of class `automated_ppc` containing diagnostics and flags.
#' @export
automated_ppc <- function(model, observed_data, n_samples = 1000, p_value_threshold = 0.05) {
  checkmate::assert_class(model, "brmsfit")
  checkmate::assert_numeric(observed_data)
  checkmate::assert_number(p_value_threshold, lower = 0, upper = 0.5)

  # 0. Initialize flags (FIXED)
  flags <- character()

  # 1. Generate Samples
  pp_samples <- brms::posterior_predict(model, ndraws = n_samples)

  # 2. Define Stats
  stats_funcs <- list(
    Mean = mean,
    SD = sd,
    Min = min,
    Max = max,
    Skewness = function(x) {
      if (sd(x) == 0) return(0)
      mean((x - mean(x))^3) / sd(x)^3
    },
    Kurtosis = function(x) {
      if (sd(x) == 0) return(0)
      (mean((x - mean(x))^4) / sd(x)^4) - 3
    }
  )

  # 3. Compute Observed
  obs_stats <- vapply(stats_funcs, function(f) f(observed_data), numeric(1))

  # 4. Compute Replicated
  # pp_samples: rows = draws, cols = data points
  rep_stats <- apply(pp_samples, 1, function(row) {
    vapply(stats_funcs, function(f) f(row), numeric(1))
  })

  if (is.matrix(rep_stats)) rep_stats <- t(rep_stats)

  # 5. P-values: proportion of replicated stats >= observed stat
  # Keep column names to match obs_stats order
  if (ncol(rep_stats) != length(obs_stats)) {
    stop("Mismatch between number of statistics in replicated samples and observed stats.")
  }

  p_values <- colMeans(rep_stats >= matrix(obs_stats, nrow = nrow(rep_stats), ncol = ncol(rep_stats), byrow = TRUE))
  names(p_values) <- names(obs_stats)

  # 6. Flags: compare to thresholds
  lower_cut <- p_value_threshold # e.g., 0.05
  upper_cut <- 1 - p_value_threshold # e.g., 0.95

  for (stat_name in names(p_values)) {
    p <- p_values[[stat_name]]
    if (is.na(p)) next

    if (p <= lower_cut) {
      # Low p: model underestimates the statistic
      flags <- c(flags, paste0("Model underestimates ", stat_name, " (p = ", signif(p, 3), ")"))
    } else if (p >= upper_cut) {
      # High p: model overestimates the statistic
      flags <- c(flags, paste0("Model overestimates ", stat_name, " (p = ", signif(p, 3), ")"))
    }
  }

  result <- list(
    diagnostics = data.frame(
      Statistic = names(obs_stats),
      Observed = as.numeric(obs_stats),
      P_Value = as.numeric(p_values),
      Flagged = (p_values <= lower_cut | p_values >= upper_cut),
      stringsAsFactors = FALSE
    ),
    flags = if (length(flags) > 0) flags else "No major discrepancies detected.",
    n_samples = n_samples
  )

  class(result) <- "automated_ppc"
  return(result)
}

#' @export
print.automated_ppc <- function(x, ...) {
  cat("\n=== Automated PPC Report ===\n")
  if (length(x$flags) == 1 && x$flags == "No major discrepancies detected.") {
    cat(" [OK] No major discrepancies detected.\n")
  } else {
    for (flag in x$flags) cat(" [!] ", flag, "\n", sep = "")
  }
  print(x$diagnostics, row.names = FALSE)
}
