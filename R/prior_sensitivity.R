#' Prior Sensitivity Analysis
#'
#' Conducts comprehensive prior sensitivity analysis to assess how robust
#' posterior inferences are to alternative prior specifications.
#'
#' @param model A fitted Bayesian model (brmsfit, stanfit, or compatible)
#' @param parameters Character vector of parameter names to analyze
#' @param prior_grid List of prior specifications to compare (named list)
#' @param comparison_metric One of "KL", "Wasserstein", or "overlap"
#' @param plot Logical. Whether to generate plots (default: TRUE)
#' @param n_draws Number of posterior draws to use (default: 2000)
#' @param x Object of class `prior_sensitivity` (for print/plot methods).
#' @param ... Additional arguments passed to plotting functions
#'
#' @return An object of class \code{prior_sensitivity} containing:
#' \item{sensitivity_metrics}{Data frame with sensitivity metrics}
#' \item{posteriors}{List of posterior distributions for each prior}
#' \item{comparison_metric}{Metric used for comparison}
#' \item{parameters}{Parameters analyzed}
#' \item{model}{The original fitted model}
#'
#' @details
#' Prior sensitivity analysis assesses how much posterior inferences depend
#' on the choice of prior distribution. Small sensitivity metrics indicate
#' that conclusions are robust to prior specification.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' fit <- brm(mpg ~ hp + wt, data = mtcars)
#'
#' result <- prior_sensitivity(
#'   model = fit,
#'   parameters = c("b_hp", "b_wt"),
#'   prior_grid = list(
#'     weak = set_prior("normal(0, 10)", class = "b"),
#'     strong = set_prior("normal(0, 1)", class = "b")
#'   ),
#'   comparison_metric = "KL"
#' )
#'
#' print(result)
#' plot(result)
#' }
#'
#' @export
prior_sensitivity <- function(model,
                              parameters,
                              prior_grid,
                              comparison_metric = "KL",
                              plot = TRUE,
                              n_draws = 2000,
                              ...) {

  # Input validation
  if (!inherits(model, "brmsfit") && !inherits(model, "stanfit")) {
    stop("Model must be a brmsfit or stanfit object")
  }

  checkmate::assert_character(parameters, min.len = 1)
  checkmate::assert_list(prior_grid, min.len = 1, names = "named")
  checkmate::assert_choice(comparison_metric, c("KL", "Wasserstein", "overlap"))
  checkmate::assert_logical(plot, len = 1)
  checkmate::assert_int(n_draws, lower = 100)

  # Extract original posterior
  original_posterior <- extract_posterior(model, parameters, n_draws)

  # Refit with alternative priors
  refitted_posteriors <- lapply(prior_grid, function(priors) {
    refit_with_prior(model, priors, n_draws)
  })

  # Calculate sensitivity metrics
  sensitivity_metrics <- calculate_sensitivity_metrics(
    original_posterior,
    refitted_posteriors,
    parameters,
    comparison_metric
  )

  # Create result object
  result <- structure(
    list(
      sensitivity_metrics = sensitivity_metrics,
      posteriors = c(list(original = original_posterior), refitted_posteriors),
      comparison_metric = comparison_metric,
      parameters = parameters,
      model = model
    ),
    class = "prior_sensitivity"
  )

  # Generate plots if requested
  if (plot) {
    result$plots <- create_sensitivity_plots(result, ...)
  }

  return(result)
}

#' @rdname prior_sensitivity
#' @export
print.prior_sensitivity <- function(x, ...) {
  cat("\nPrior Sensitivity Analysis\n")
  cat("===========================\n\n")
  cat("Parameters analyzed:", paste(x$parameters, collapse = ", "), "\n")
  cat("Comparison metric:", x$comparison_metric, "\n")
  cat("Number of alternative priors:", nrow(x$sensitivity_metrics) / length(x$parameters), "\n\n")
  cat("Sensitivity Metrics:\n")
  print(x$sensitivity_metrics)
  invisible(x)
}

#' @rdname prior_sensitivity
#' @export
plot.prior_sensitivity <- function(x, ...) {
  if (!is.null(x$plots)) {
    print(x$plots)
  } else {
    x$plots <- create_sensitivity_plots(x, ...)
    print(x$plots)
  }
  invisible(x)
}

# ============ HELPER FUNCTIONS ============

#' Extract Posterior Draws
#' @keywords internal
extract_posterior <- function(model, parameters, n_draws) {
  if (inherits(model, "brmsfit")) {
    # FIX: Use posterior::subset_draws() instead of dplyr::select()
    # This avoids the warning about dropping draws_df class metadata
    # posterior::subset_draws() is designed for draws objects and preserves class
    draws_df <- posterior::as_draws_df(model)

    # Subset to only requested parameters using posterior functions
    # This preserves the draws_df class without warnings
    subset_draws_obj <- posterior::subset_draws(draws_df, variable = parameters)

    # Convert to regular data frame and limit to n_draws
    as.data.frame(subset_draws_obj) |> head(n_draws)
  } else {
    stop("Model class not supported")
  }
}

#' Refit Model with Alternative Prior
#' @keywords internal
refit_with_prior <- function(model, new_prior, n_draws) {
  if (inherits(model, "brmsfit")) {
    refitted <- update(
      model,
      prior = new_prior,
      refresh = 0,
      verbose = FALSE,
      chains = 2,
      iter = 2000,
      warmup = 1000,
      silent = 2
    )

    # FIX: Use posterior::subset_draws() to avoid metadata warnings
    draws_df <- posterior::as_draws_df(refitted)

    # Get only the parameters we need
    subset_obj <- posterior::subset_draws(draws_df, variable = colnames(draws_df)[!grepl("^\\.", colnames(draws_df))])

    as.data.frame(subset_obj) |> head(n_draws)
  } else {
    stop("Model class not supported")
  }
}

#' Calculate Sensitivity Metrics
#' @keywords internal
calculate_sensitivity_metrics <- function(original,
                                          refitted_list,
                                          parameters,
                                          metric) {
  metrics_list <- list()

  for (i in seq_along(refitted_list)) {
    prior_name <- names(refitted_list)[i]

    for (param in parameters) {
      if (param %in% names(original) && param %in% names(refitted_list[[i]])) {
        original_draws <- original[[param]]
        refitted_draws <- refitted_list[[i]][[param]]

        if (metric == "KL") {
          metric_value <- calculate_kl_divergence(original_draws, refitted_draws)
        } else if (metric == "Wasserstein") {
          metric_value <- calculate_wasserstein_distance(original_draws, refitted_draws)
        } else if (metric == "overlap") {
          metric_value <- calculate_overlap_coefficient(original_draws, refitted_draws)
        }

        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          prior = prior_name,
          parameter = param,
          metric_value = metric_value,
          metric_type = metric,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  do.call(rbind, metrics_list)
}

#' KL Divergence Calculation
#' @keywords internal
calculate_kl_divergence <- function(x, y) {
  dx <- density(x, from = min(c(x, y)), to = max(c(x, y)))
  dy <- density(y, from = min(c(x, y)), to = max(c(x, y)))

  p <- dx$y / sum(dx$y)
  q <- dy$y / sum(dy$y)

  p <- pmax(p, 1e-10)
  q <- pmax(q, 1e-10)

  sum(p * log(p / q))
}

#' Wasserstein Distance Calculation
#' @keywords internal
calculate_wasserstein_distance <- function(x, y) {
  quantiles <- seq(0, 1, by = 0.01)
  qx <- quantile(x, probs = quantiles)
  qy <- quantile(y, probs = quantiles)
  mean(abs(qx - qy))
}

#' Overlap Coefficient Calculation
#' @keywords internal
calculate_overlap_coefficient <- function(x, y) {
  dx <- density(x)
  dy <- density(y)

  common_x <- seq(min(c(dx$x, dy$x)), max(c(dx$x, dy$x)), length.out = 1000)
  dx_interp <- approx(dx$x, dx$y, xout = common_x)$y
  dy_interp <- approx(dy$x, dy$y, xout = common_x)$y

  sum(pmin(dx_interp, dy_interp), na.rm = TRUE) /
    sum(pmax(dx_interp, dy_interp), na.rm = TRUE)
}

#' Create Sensitivity Plots
#' @keywords internal
create_sensitivity_plots <- function(result, ...) {
  plot_data <- data.frame()

  for (param in result$parameters) {
    for (prior_name in names(result$posteriors)) {
      if (param %in% names(result$posteriors[[prior_name]])) {
        draws <- result$posteriors[[prior_name]][[param]]
        plot_data <- rbind(
          plot_data,
          data.frame(
            parameter = param,
            prior = prior_name,
            value = draws,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = value, color = prior, fill = prior)) +
    ggplot2::geom_density(alpha = 0.3) +
    ggplot2::facet_wrap(~parameter, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Prior Sensitivity Analysis",
      subtitle = paste("Metric:", result$comparison_metric),
      x = "Parameter Value",
      y = "Density"
    )
}

# Global variables
utils::globalVariables(c(
  "prior", "parameter", "metric_value", "metric_type",
  "value", "."
))
