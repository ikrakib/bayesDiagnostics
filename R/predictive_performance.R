#' Predictive Performance Evaluation
#'
#' Comprehensive evaluation of Bayesian model predictive performance using
#' multiple metrics: RMSE, MAE, Coverage, and proper scoring rules.
#'
#' @param model A brmsfit object
#' @param newdata Optional data frame for out-of-sample predictions.
#'   If NULL, uses model's original data.
#' @param observed_y Numeric vector of observed response values.
#'   Must match nrow(newdata) or length(newdata).
#' @param metrics Character vector of metrics to compute.
#'   Options: "rmse" (root mean square error), "mae" (mean absolute error),
#'   "coverage" (credible interval coverage), "crps" (continuous ranked prob score),
#'   "all" (default - all metrics)
#' @param credible_level Numeric. Credible interval level for coverage (default: 0.95)
#' @param n_draws Integer. Number of posterior draws to use (NULL = all)
#'
#' @return A list of class `predictive_performance` containing:
#'   \item{point_metrics}{Data frame with RMSE, MAE, and correlation}
#'   \item{interval_metrics}{Data frame with coverage and interval width}
#'   \item{proper_scores}{Data frame with CRPS and log-score}
#'   \item{prediction_summary}{Data frame with mean, lower CI, upper CI for each observation}
#'   \item{metrics_requested}{Character vector of requested metrics}
#'   \item{model_formula}{Formula from the fitted model}
#'   \item{sample_size}{Number of observations}
#'
#' @details
#' Predictive performance metrics evaluate how well posterior predictions align with data:
#'
#' Point metrics:
#' - RMSE: Square root of mean squared prediction error
#' - MAE: Mean absolute error
#' - Correlation: Pearson correlation of predictions vs observed
#'
#' Interval metrics:
#' - Coverage: Proportion of observations within credible interval
#' - Width: Average width of credible intervals
#'
#' Proper scoring rules:
#' - CRPS: Continuous Ranked Probability Score (lower is better)
#'   Computed using empirical cumulative distribution function
#'
#' @examples
#' \donttest{
#' library(brms)
#'
#' data <- data.frame(y = rnorm(100, mean = 5), x = rnorm(100))
#' model <- brm(y ~ x, data = data, chains = 1, iter = 1000, refresh = 0)
#'
#' perf <- predictive_performance(model, observed_y = data$y, metrics = "all")
#' print(perf)
#' }
#'
#' @importFrom brms posterior_predict posterior_epred
#' @importFrom stats quantile cor
#' @export
predictive_performance <- function(model, newdata = NULL, observed_y,
                                   metrics = "all", credible_level = 0.95,
                                   n_draws = NULL) {

  checkmate::assert_class(model, "brmsfit")
  checkmate::assert_numeric(observed_y, min.len = 2)
  checkmate::assert_number(credible_level, lower = 0, upper = 1)

  # Validate metrics
  valid_metrics <- c("rmse", "mae", "coverage", "crps")
  if (metrics == "all") {
    metrics <- valid_metrics
  } else {
    metrics <- match.arg(metrics, valid_metrics, several.ok = TRUE)
  }

  # Extract model data if newdata not provided
  if (is.null(newdata)) {
    new_data <- model$data
  } else {
    new_data <- newdata
  }

  # Validate dimensions - use new_data, not newdata
  if (nrow(new_data) != length(observed_y)) {
    stop("new_data and observed_y must have matching dimensions")
  }

  n_obs <- length(observed_y)

  # Get posterior predictions (draws x observations)
  yrep <- brms::posterior_predict(model, newdata = new_data)

  # Subsample if requested
  if (!is.null(n_draws) && nrow(yrep) > n_draws) {
    idx <- sample(seq_len(nrow(yrep)), n_draws)
    yrep <- yrep[idx, , drop = FALSE]
  }

  # Get posterior expected values (draws x observations)
  mu <- brms::posterior_epred(model, newdata = new_data)

  if (!is.null(n_draws) && nrow(mu) > n_draws) {
    idx <- sample(seq_len(nrow(mu)), n_draws)
    mu <- mu[idx, , drop = FALSE]
  }

  # Initialize result storage
  results <- list()

  # 1. RMSE and MAE (point predictions)
  if ("rmse" %in% metrics || "mae" %in% metrics) {
    pred_mean <- colMeans(yrep)

    rmse <- sqrt(mean((pred_mean - observed_y)^2))
    mae <- mean(abs(pred_mean - observed_y))
    corr <- cor(pred_mean, observed_y)

    results$point_metrics <- data.frame(
      Metric = c("RMSE", "MAE", "Correlation"),
      Value = c(rmse, mae, corr),
      stringsAsFactors = FALSE
    )
  }

  # 2. Coverage (credible interval calibration)
  if ("coverage" %in% metrics) {
    alpha <- 1 - credible_level
    lower_q <- alpha / 2
    upper_q <- 1 - alpha / 2

    lower_ci <- apply(yrep, 2, quantile, probs = lower_q)
    upper_ci <- apply(yrep, 2, quantile, probs = upper_q)

    coverage <- mean((observed_y >= lower_ci) & (observed_y <= upper_ci))
    avg_width <- mean(upper_ci - lower_ci)

    results$interval_metrics <- data.frame(
      Metric = c("Coverage", "Avg_CI_Width", "Credible_Level"),
      Value = c(coverage, avg_width, credible_level),
      stringsAsFactors = FALSE
    )
  }

  # 3. CRPS (Continuous Ranked Probability Score) - Empirical CDF approach
  if ("crps" %in% metrics) {
    crps_vals <- numeric(n_obs)

    for (i in seq_len(n_obs)) {
      y_val <- observed_y[i]
      samples <- sort(yrep[, i])  # Sort samples for empirical CDF
      n_samp <- length(samples)

      # Empirical CDF-based CRPS calculation
      # CRPS = (1/n) * sum|y - x_i| * (2 * i/n - 1)
      crps <- 0
      for (j in seq_len(n_samp)) {
        # Empirical CDF: j/n is the CDF value at x_j
        # Use (j-0.5)/n for better approximation (mid-point)
        ecdf_val <- (j - 0.5) / n_samp
        crps <- crps + abs(y_val - samples[j]) * (2 * ecdf_val - 1)
      }

      crps_vals[i] <- crps / n_samp
    }

    mean_crps <- mean(crps_vals, na.rm = TRUE)

    results$proper_scores <- data.frame(
      Metric = "CRPS",
      Value = mean_crps,
      SE = sd(crps_vals, na.rm = TRUE) / sqrt(n_obs),
      stringsAsFactors = FALSE
    )
  }

  # Prediction summary
  pred_summary <- data.frame(
    Observation = seq_len(n_obs),
    Observed = observed_y,
    Predicted_Mean = colMeans(yrep),
    Predicted_SD = apply(yrep, 2, sd),
    stringsAsFactors = FALSE
  )

  # Add credible intervals if coverage was computed
  if ("coverage" %in% metrics) {
    alpha <- 1 - credible_level
    lower_q <- alpha / 2
    upper_q <- 1 - alpha / 2

    pred_summary$Lower_CI <- apply(yrep, 2, quantile, probs = lower_q)
    pred_summary$Upper_CI <- apply(yrep, 2, quantile, probs = upper_q)
  }

  results$prediction_summary <- pred_summary
  results$metrics_requested <- metrics
  results$model_formula <- model$formula
  results$sample_size <- n_obs
  results$credible_level <- credible_level

  class(results) <- "predictive_performance"
  return(results)
}

#' @export
print.predictive_performance <- function(x, ...) {
  cat("\n=== Predictive Performance Evaluation ===\n\n")

  cat(sprintf("Sample size: %d observations\n", x$sample_size))
  cat(sprintf("Credible level: %.1f%%\n", x$credible_level * 100))
  cat(sprintf("Metrics computed: %s\n\n", paste(x$metrics_requested, collapse = ", ")))

  if (!is.null(x$point_metrics)) {
    cat("POINT METRICS:\n")
    print(x$point_metrics, row.names = FALSE)
    cat("\n")
  }

  if (!is.null(x$interval_metrics)) {
    cat("INTERVAL METRICS:\n")
    print(x$interval_metrics, row.names = FALSE)
    cat("\n")
  }

  if (!is.null(x$proper_scores)) {
    cat("PROPER SCORING RULES:\n")
    print(x$proper_scores, row.names = FALSE)
    cat("\n")
  }

  cat("PREDICTION SUMMARY (first 10):\n")
  print(head(x$prediction_summary, 10), row.names = FALSE)
}

#' @export
plot.predictive_performance <- function(x, ...) {

  pred_df <- x$prediction_summary

  # Plot 1: Observed vs Predicted
  p1 <- ggplot2::ggplot(pred_df, ggplot2::aes(x = Observed, y = Predicted_Mean)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Observed vs Predicted",
                  x = "Observed",
                  y = "Predicted Mean") +
    ggplot2::theme_minimal()

  # Plot 2: Residuals
  pred_df$Residual <- pred_df$Observed - pred_df$Predicted_Mean

  p2 <- ggplot2::ggplot(pred_df, ggplot2::aes(x = Predicted_Mean, y = Residual)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Residuals vs Predicted",
                  x = "Predicted Mean",
                  y = "Residual") +
    ggplot2::theme_minimal()

  # Plot 3: Prediction intervals (first 50 obs)
  n_show <- min(50, nrow(pred_df))
  pred_show <- pred_df[seq_len(n_show), ]

  if ("Lower_CI" %in% names(pred_show)) {
    p3 <- ggplot2::ggplot(pred_show,
                          ggplot2::aes(x = Observation, y = Predicted_Mean)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower_CI, ymax = Upper_CI),
                             width = 0.1, alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(y = Observed), color = "red", shape = 3) +
      ggplot2::labs(title = "Credible Intervals (first 50)",
                    x = "Observation",
                    y = "Value") +
      ggplot2::theme_minimal()
  } else {
    p3 <- ggplot2::ggplot(pred_show, ggplot2::aes(x = Observation)) +
      ggplot2::geom_point(ggplot2::aes(y = Predicted_Mean), color = "blue") +
      ggplot2::geom_point(ggplot2::aes(y = Observed), color = "red", shape = 3) +
      ggplot2::labs(title = "Predictions (first 50)",
                    x = "Observation",
                    y = "Value") +
      ggplot2::theme_minimal()
  }

  print(p1)
  print(p2)
  print(p3)
}

# Global variables
utils::globalVariables(c("Observed", "Predicted_Mean", "Residual", "Lower_CI", "Upper_CI", "Observation"))
