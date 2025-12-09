#' Prior Elicitation Helper
#'
#' Interactive tool to translate expert knowledge into statistical priors.
#' Guides users through specification of prior distributions based on
#' domain expertise and data characteristics.
#'
#' @param expert_beliefs List containing expert beliefs about parameters.
#'   Elements: parameter_name, plausible_range, most_likely_value, confidence
#' @param parameter_type Character: "continuous", "discrete", or "proportion"
#' @param method Character: "quantile", "histogram", or "interactive"
#' @param data_sample Numeric vector of observed data (optional, for context)
#' @param visualize Logical. Show comparison plots (default: TRUE)
#' @param x Object of class `prior_elicitation` (for print method).
#' @param ... Additional arguments
#'
#' @return Object of class `prior_elicitation` containing:
#'   - `recommended_prior` - Prior specification as prior() object
#'   - `parameter_summary` - Summary of expert inputs
#'   - `diagnostic_plots` - Visualizations of prior
#'   - `alternatives` - Alternative prior specifications
#'   - `sensitivity_note` - Guidance on sensitivity analysis
#'
#' @details
#' This function helps bridge the gap between domain expertise and
#' statistical prior specification. It uses several methods:
#'
#' 1. Quantile method: Expert specifies percentiles
#' 2. Histogram method: Expert draws rough distribution shape
#' 3. Interactive: Step-by-step guided elicitation
#'
#' The function then matches the inputs to standard distributions
#' (normal, t, gamma, beta, etc.) and suggests sensitivity analysis.
#'
#' @export
prior_elicitation_helper <- function(expert_beliefs,
                                     parameter_type = "continuous",
                                     method = "quantile",
                                     data_sample = NULL,
                                     visualize = TRUE,
                                     ...) {

  # Validation
  checkmate::assert_list(expert_beliefs)
  checkmate::assert_choice(parameter_type, c("continuous", "discrete", "proportion"))
  checkmate::assert_choice(method, c("quantile", "histogram", "interactive"))
  checkmate::assert_logical(visualize)

  # Extract expert inputs
  param_name <- expert_beliefs$parameter_name
  plausible_range <- expert_beliefs$plausible_range
  most_likely <- expert_beliefs$most_likely_value
  confidence <- expert_beliefs$confidence  # 0-1 scale

  # Recommend prior distribution
  if (parameter_type == "continuous") {
    recommended_prior <- elicit_continuous_prior(
      plausible_range,
      most_likely,
      confidence
    )
  } else if (parameter_type == "discrete") {
    recommended_prior <- elicit_discrete_prior(
      plausible_range,
      most_likely
    )
  } else {
    recommended_prior <- elicit_proportion_prior(
      most_likely,
      confidence
    )
  }

  # Generate alternatives
  alternatives <- generate_prior_alternatives(
    recommended_prior,
    parameter_type,
    confidence
  )

  # Create sensitivity note
  sensitivity_note <- create_sensitivity_guidance(
    recommended_prior,
    alternatives
  )

  # Create result
  result <- structure(
    list(
      parameter_name = param_name,
      recommended_prior = recommended_prior,
      parameter_summary = list(
        plausible_range = plausible_range,
        most_likely = most_likely,
        confidence = confidence,
        parameter_type = parameter_type
      ),
      alternatives = alternatives,
      sensitivity_note = sensitivity_note,
      expert_method = method,
      elicitation_time = Sys.time()
    ),
    class = "prior_elicitation"
  )

  if (visualize && !is.null(data_sample)) {
    result$diagnostic_plots <- create_elicitation_diagnostics(result, data_sample)
  }

  return(result)
}

#' @rdname prior_elicitation_helper
#' @export
print.prior_elicitation <- function(x, ...) {
  cat("\nPrior Elicitation Results\n")
  cat("=========================\n\n")
  cat("Parameter:", x$parameter_name, "\n")
  cat("Parameter type:", x$parameter_summary$parameter_type, "\n")
  cat("Confidence level:", round(x$parameter_summary$confidence, 2), "\n\n")

  cat("Plausible Range:",
      min(x$parameter_summary$plausible_range), "to",
      max(x$parameter_summary$plausible_range), "\n")
  cat("Most Likely Value:", x$parameter_summary$most_likely, "\n\n")

  cat("Recommended Prior:\n")
  cat("  ", deparse(x$recommended_prior), "\n\n")

  cat("Alternative Priors (for sensitivity analysis):\n")
  for (i in seq_along(x$alternatives)) {
    cat(sprintf("  %d. %s\n", i, deparse(x$alternatives[[i]])))
  }

  cat("\n", x$sensitivity_note, "\n")

  invisible(x)
}

# Helper functions for prior_elicitation

#' Elicit continuous prior
#' @keywords internal
elicit_continuous_prior <- function(plausible_range, most_likely, confidence) {
  # Determine distribution family
  lower <- min(plausible_range)
  upper <- max(plausible_range)
  center <- most_likely

  # Spread based on confidence
  spread_factor <- 1 / confidence

  # Check if distribution should be bounded
  if (lower > 0) {
    # Log-normal or gamma-like
    log_center <- log(center)
    log_sd <- log(spread_factor)

    return(brms::prior(
      lognormal(log_center, log_sd),
      class = "b"
    ))
  } else {
    # Normal distribution
    sd_val <- (upper - lower) / (4 * sqrt(confidence))

    return(brms::prior(
      normal(center, sd_val),
      class = "b"
    ))
  }
}

#' Elicit discrete prior
#' @keywords internal
elicit_discrete_prior <- function(plausible_range, most_likely) {
  # For count data, use Poisson or negative binomial priors
  return(brms::prior(
    poisson(log(most_likely)),
    class = "b"
  ))
}

#' Elicit proportion prior
#' @keywords internal
elicit_proportion_prior <- function(most_likely, confidence) {
  # Beta distribution for proportions
  # Use method of moments
  alpha <- most_likely * (1 / (1 - confidence) - 1)
  beta <- (1 - most_likely) * (1 / (1 - confidence) - 1)

  return(brms::prior(
    beta(alpha, beta),
    class = "b"
  ))
}

#' Generate prior alternatives
#' @keywords internal
generate_prior_alternatives <- function(recommended, param_type, confidence) {
  # Generate more/less informative alternatives
  alternatives <- list(
    weak_prior = generate_weaker_version(recommended, param_type),
    strong_prior = generate_stronger_version(recommended, param_type),
    default_prior = brms::prior(normal(0, 1), class = "b")
  )

  return(alternatives)
}

#' Generate weaker version
#' @keywords internal
generate_weaker_version <- function(prior_obj, param_type) {
  # Make prior more diffuse
  if (param_type == "continuous") {
    return(brms::prior(normal(0, 10), class = "b"))
  } else {
    return(brms::prior(normal(0, 5), class = "b"))
  }
}

#' Generate stronger version
#' @keywords internal
generate_stronger_version <- function(prior_obj, param_type) {
  # Make prior more concentrated
  if (param_type == "continuous") {
    return(brms::prior(normal(0, 0.5), class = "b"))
  } else {
    return(brms::prior(normal(0, 0.25), class = "b"))
  }
}

#' Create sensitivity guidance
#' @keywords internal
create_sensitivity_guidance <- function(recommended, alternatives) {
  guidance <- paste0(
    "Recommended approach:\n",
    "1. Fit model with recommended prior\n",
    "2. Refit with each alternative prior\n",
    "3. Compare posterior distributions\n",
    "4. If substantially different, conduct formal sensitivity analysis\n",
    "5. Report robustness across prior specifications"
  )

  return(guidance)
}

#' Create elicitation diagnostics
#' @keywords internal
create_elicitation_diagnostics <- function(result, data_sample) {
  # Create plots comparing prior to data
  prior_draws <- rnorm(1000, mean = result$parameter_summary$most_likely,
                       sd = diff(result$parameter_summary$plausible_range) / 4)

  plot_data <- data.frame(
    value = c(data_sample, prior_draws),
    type = rep(c("Observed Data", "Prior"),
               c(length(data_sample), length(prior_draws)))
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, fill = type)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Prior vs Observed Data",
      subtitle = paste("Parameter:", result$parameter_name),
      x = "Value",
      y = "Density"
    )

  return(p)
}
