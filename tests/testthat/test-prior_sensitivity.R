#' Prior Sensitivity Analysis
#'
#' Conducts comprehensive prior sensitivity analysis to assess how robust
#' posterior inferences are to alternative prior specifications.
#'
#' @param model A fitted Bayesian model (brmsfit or compatible)
#' @param parameters Character vector of parameter names to analyze
#' @param prior_grid List of prior specifications to compare (named list)
#' @param comparison_metric One of "KL", "Wasserstein", or "overlap"
#' @param plot Logical. Whether to generate plots (default: TRUE)
#' @param n_draws Number of posterior draws to use (default: 2000)
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
#' @export
prior_sensitivity <- function(model,
                              parameters,
                              prior_grid,
                              comparison_metric = "KL",
                              plot = TRUE,
                              n_draws = 2000,
                              ...) {

  # Input validation - FIXED: Accept "brmsfit" class
  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit' (fitted with brms)")
  }

  checkmate::assert_character(parameters, min.len = 1)
  checkmate::assert_list(prior_grid, min.len = 1, names = "named")
  checkmate::assert_choice(comparison_metric, c("KL", "Wasserstein", "overlap"))
  checkmate::assert_logical(plot)
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
extract_posterior <- function(model, parameters, n_draws) {
  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit'")
  }

  # Extract posterior draws
  posterior_draws <- posterior::as_draws_df(model)

  # Select parameters and limit to n_draws
  result <- posterior_draws %>%
    dplyr::select(dplyr::all_of(parameters)) %>%
    head(n_draws)

  return(result)
}

#' Refit Model with Alternative Prior
refit_with_prior <- function(model, new_prior, n_draws) {
  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit'")
  }

  # Refit with new prior
  refitted <- brms::update(
    model,
    prior = new_prior,
    refresh = 0,
    verbose = FALSE,
    chains = 1,
    iter = 1000
  )

  # Extract draws
  posterior_draws <- posterior::as_draws_df(refitted)
  result <- posterior_draws %>% head(n_draws)

  return(result)
}

#' Calculate Sensitivity Metrics
calculate_sensitivity_metrics <- function(original,
                                          refitted_list,
                                          parameters,
                                          metric) {

  metrics_list <- list()

  for (i in seq_along(refitted_list)) {
    prior_name <- names(refitted_list)[i]

    for (param in parameters) {
      # Extract draws safely
      original_draws <- original[[param]]
      refitted_draws <- refitted_list[[i]][[param]]

      if (is.null(original_draws) || is.null(refitted_draws)) {
        warning(paste("Parameter", param, "not found in posteriors"))
        next
      }

      # Calculate metric
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

  if (length(metrics_list) == 0) {
    warning("No metrics calculated. Check parameter names.")
    return(data.frame())
  }

  do.call(rbind, metrics_list)
}

#' KL Divergence Calculation
calculate_kl_divergence <- function(x, y) {
  tryCatch({
    # Estimate densities
    x_range <- range(c(x, y))
    dx <- density(x, from = x_range, to = x_range)
    dy <- density(y, from = x_range, to = x_range)

    # KL divergence (approximate)
    p <- dx$y / sum(dx$y)
    q <- dy$y / sum(dy$y)

    # Avoid log(0)
    p <- pmax(p, 1e-10)
    q <- pmax(q, 1e-10)

    kl_div <- sum(p * log(p / q))
    return(kl_div)
  }, error = function(e) {
    warning("KL divergence calculation failed: ", e$message)
    return(NA_real_)
  })
}

#' Wasserstein Distance Calculation
calculate_wasserstein_distance <- function(x, y) {
  tryCatch({
    # Approximate via quantiles
    quantiles <- seq(0, 1, by = 0.01)
    qx <- quantile(x, probs = quantiles, na.rm = TRUE)
    qy <- quantile(y, probs = quantiles, na.rm = TRUE)

    wasserstein <- mean(abs(qx - qy), na.rm = TRUE)
    return(wasserstein)
  }, error = function(e) {
    warning("Wasserstein calculation failed: ", e$message)
    return(NA_real_)
  })
}

#' Overlap Coefficient Calculation
calculate_overlap_coefficient <- function(x, y) {
  tryCatch({
    # Overlap between two distributions
    x_range <- range(c(x, y))
    dx <- density(x, from = x_range, to = x_range)
    dy <- density(y, from = x_range, to = x_range)

    # Interpolate to common grid
    common_x <- seq(x_range, x_range, length.out = 1000)
    dx_interp <- approx(dx$x, dx$y, xout = common_x)$y
    dy_interp <- approx(dy$x, dy$y, xout = common_x)$y

    # Calculate overlap
    overlap <- sum(pmin(dx_interp, dy_interp), na.rm = TRUE) /
      sum(pmax(dx_interp, dy_interp), na.rm = TRUE)

    return(overlap)
  }, error = function(e) {
    warning("Overlap calculation failed: ", e$message)
    return(NA_real_)
  })
}

#' Create Sensitivity Plots
create_sensitivity_plots <- function(result, ...) {
  tryCatch({
    # Create density plot comparing posteriors
    plot_data <- data.frame()

    for (param in result$parameters) {
      for (prior_name in names(result$posteriors)) {
        draws <- result$posteriors[[prior_name]][[param]]
        if (!is.null(draws)) {
          plot_data <- rbind(
            plot_data,
            data.frame(
              parameter = param,
              prior = prior_name,
              value = draws
            )
          )
        }
      }
    }

    if (nrow(plot_data) == 0) {
      warning("No data for plotting")
      return(NULL)
    }

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, color = prior, fill = prior)) +
      ggplot2::geom_density(alpha = 0.3) +
      ggplot2::facet_wrap(~parameter, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Prior Sensitivity Analysis",
        subtitle = paste("Metric:", result$comparison_metric),
        x = "Parameter Value",
        y = "Density"
      )

    return(p)
  }, error = function(e) {
    warning("Plot creation failed: ", e$message)
    return(NULL)
  })
}

# Global variables
utils::globalVariables(c(
  "prior", "parameter", "metric_value", "metric_type",
  "value", "."
))
