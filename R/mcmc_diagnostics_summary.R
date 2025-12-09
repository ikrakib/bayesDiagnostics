#' MCMC Diagnostics Summary
#'
#' Provides a comprehensive summary of MCMC convergence diagnostics,
#' including R-hat, Effective Sample Size (ESS), and NUTS-specific
#' issues like divergent transitions and tree depth saturation.
#'
#' @param model A fitted brmsfit object
#' @param rhat_threshold Numeric. Threshold for R-hat warning (default: 1.01)
#' @param ess_threshold Numeric. Threshold for ESS warning (default: 400)
#'
#' @return A list of class `mcmc_diagnostics` containing:
#'   - `rhat_issues`: Parameters with high R-hat
#'   - `ess_issues`: Parameters with low ESS
#'   - `divergences`: Number of divergent transitions
#'   - `tree_depth`: Number of iterations hitting max tree depth
#'   - `summary_table`: Tibble of all diagnostics per parameter
#'   - `converged`: Logical summary of overall convergence
#'
#' @export
mcmc_diagnostics_summary <- function(model, rhat_threshold = 1.01, ess_threshold = 400) {

  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit'")
  }

  # Extract diagnostics using posterior package
  draws <- posterior::as_draws_array(model)

  # Calculate summaries
  summ <- posterior::summarise_draws(
    draws,
    "rhat",
    "ess_bulk",
    "ess_tail"
  )

  # Identify issues
  rhat_issues <- summ[summ$rhat > rhat_threshold, ]
  ess_bulk_issues <- summ[summ$ess_bulk < ess_threshold, ]
  ess_tail_issues <- summ[summ$ess_tail < ess_threshold, ]

  # NUTS specific diagnostics (Divergences & Tree Depth)
  # We use rstan::get_sampler_params safely
  sampler_params <- rstan::get_sampler_params(model$fit, inc_warmup = FALSE)

  total_divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  total_max_treedepth <- sum(sapply(sampler_params, function(x) sum(x[, "treedepth__"] >= 10)))

  # Determine overall status
  converged <- (nrow(rhat_issues) == 0) &&
    (nrow(ess_bulk_issues) == 0) &&
    (nrow(ess_tail_issues) == 0) &&
    (total_divergences == 0)

  result <- structure(
    list(
      rhat_issues = rhat_issues,
      ess_bulk_issues = ess_bulk_issues,
      ess_tail_issues = ess_tail_issues,
      divergences = total_divergences,
      tree_depth = total_max_treedepth,
      summary_table = summ,
      converged = converged,
      thresholds = list(rhat = rhat_threshold, ess = ess_threshold)
    ),
    class = "mcmc_diagnostics"
  )

  return(result)
}

#' @export
print.mcmc_diagnostics <- function(x, ...) {
  cat("\nMCMC Diagnostics Summary\n")
  cat("========================\n")

  if (x$converged) {
    cat("\n[OK] Model has converged! No major issues detected.\n")
  } else {
    cat("\n[FAIL] Convergence issues detected.\n")
  }

  if (nrow(x$rhat_issues) > 0) {
    cat("\n[WARN] High R-hat (> ", x$thresholds$rhat, "): ", nrow(x$rhat_issues), " parameters.\n", sep="")
  }

  if (x$divergences > 0) {
    cat("\n[!] ", x$divergences, " Divergent transitions detected!\n")
  }

  invisible(x)
}
