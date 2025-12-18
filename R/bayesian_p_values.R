#' Calculate Bayesian P-Values
#'
#' A flexible utility to calculate Bayesian p-values for any custom test statistic.
#'
#' @param yrep Matrix. Posterior predictive samples (rows = samples, cols = observations).
#' @param y Vector. Observed data.
#' @param statistic Function. The test statistic to compute (e.g., mean, max).
#'
#' @return A list with the observed stat, replicated stats, and the p-value.
#' @export
bayesian_p_values <- function(yrep, y, statistic) {
  checkmate::assert_matrix(yrep)
  checkmate::assert_numeric(y)
  checkmate::assert_function(statistic)

  # 1. Calc Observed Stat
  stat_obs <- statistic(y)

  # 2. Calc Replicated Stats
  stat_rep <- apply(yrep, 1, statistic)

  # 3. P-Value
  p_val <- mean(stat_rep >= stat_obs)

  return(list(
    observed_value = stat_obs,
    replicated_values = stat_rep,
    p_value = p_val
  ))
}
