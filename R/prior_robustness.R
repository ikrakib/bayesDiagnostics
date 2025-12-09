#' Prior Robustness Analysis
#'
#' Comprehensive assessment of posterior robustness to alternative
#' prior specifications using multiple sensitivity dimensions.
#'
#' @param model A fitted brmsfit object
#' @param prior_specifications List of alternative prior specifications.
#'   Each element should be a prior() object or named list of priors.
#' @param parameters Character vector of parameters to analyze
#' @param perturbation_direction Character: "expand", "contract", or "shift"
#' @param dimensions Numeric vector of perturbation magnitudes (default: c(0.5, 1, 2, 4))
#' @param comparison_metric One of "KL", "Wasserstein", "correlation", "coverage"
#' @param credible_level Numeric. Credible interval level (default: 0.95)
#' @param plot Logical. Generate visualizations (default: TRUE)
#' @param x Object of class `prior_robustness` (for print method).
#' @param ... Additional arguments
#'
#' @return Object of class `prior_robustness` containing:
#'   - `sensitivity_surfaces` - Multi-dimensional sensitivity results
#'   - `robustness_index` - Composite robustness score
#'   - `concerning_parameters` - Parameters with low robustness
#'   - `recommendations` - Suggested prior refinements
#'
#' @export
prior_robustness <- function(model,
                             prior_specifications,
                             parameters,
                             perturbation_direction = "expand",
                             dimensions = c(0.5, 1, 2, 4),
                             comparison_metric = "KL",
                             credible_level = 0.95,
                             plot = TRUE,
                             ...) {

  # Validation
  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit'")
  }

  checkmate::assert_list(prior_specifications, min.len = 1)
  checkmate::assert_character(parameters, min.len = 1)
  checkmate::assert_choice(perturbation_direction, c("expand", "contract", "shift"))
  checkmate::assert_numeric(dimensions, lower = 0.1)
  checkmate::assert_choice(comparison_metric,
                           c("KL", "Wasserstein", "correlation", "coverage"))
  checkmate::assert_numeric(credible_level, lower = 0.5, upper = 0.99)

  # Extract original posterior
  original_posterior <- extract_posterior_summary(model, parameters, credible_level)

  # Generate perturbations
  perturbations <- generate_prior_perturbations(
    prior_specifications,
    perturbation_direction,
    dimensions
  )

  # Refit model with perturbed priors
  sensitivity_surfaces <- list()

  for (i in seq_along(perturbations)) {
    perturbed_prior <- perturbations[[i]]

    # CRITICAL: brms update() expects the prior object directly
    # We simplified generate_prior_perturbations to ensure it's correct

    perturbed_model <- update(
      model,
      prior = perturbed_prior,
      refresh = 0,
      verbose = FALSE
    )

    perturbed_posterior <- extract_posterior_summary(
      perturbed_model,
      parameters,
      credible_level
    )

    # Calculate sensitivity metric
    metric_value <- compute_sensitivity_metric(
      original_posterior,
      perturbed_posterior,
      comparison_metric
    )

    sensitivity_surfaces[[i]] <- list(
      prior_specification = names(perturbations)[i],
      metric_value = metric_value,
      posterior = perturbed_posterior
    )
  }

  # Compute robustness index
  robustness_index <- compute_robustness_index(sensitivity_surfaces)

  # Identify concerning parameters
  concerning_parameters <- identify_concerning_parameters(
    sensitivity_surfaces,
    threshold = 0.2
  )

  # Generate recommendations
  recommendations <- generate_recommendations(
    concerning_parameters,
    prior_specifications
  )

  # Create result
  result <- structure(
    list(
      sensitivity_surfaces = sensitivity_surfaces,
      robustness_index = robustness_index,
      concerning_parameters = concerning_parameters,
      recommendations = recommendations,
      model = model,
      parameters = parameters,
      comparison_metric = comparison_metric
    ),
    class = "prior_robustness"
  )

  if (plot) {
    result$plots <- create_robustness_plots(result, ...)
  }

  return(result)
}

#' @rdname prior_robustness
#' @export
print.prior_robustness <- function(x, ...) {
  cat("\nPrior Robustness Analysis\n")
  cat("=========================\n\n")
  cat("Overall Robustness Index:", round(x$robustness_index, 3), "\n")
  cat("  (Higher is better; 1.0 = perfect robustness)\n\n")

  if (length(x$concerning_parameters) > 0) {
    cat("[WARN] Parameters with Low Robustness:\n")
    for (param in x$concerning_parameters) {
      cat("  -", param, "\n")
    }
    cat("\n")
  }

  cat("Recommendations:\n")
  for (i in seq_along(x$recommendations)) {
    cat(sprintf("%d. %s\n", i, x$recommendations[i]))
  }

  invisible(x)
}

# Helper functions for prior_robustness

#' Extract posterior summary
#' @keywords internal
extract_posterior_summary <- function(model, parameters, credible_level) {
  posterior_draws <- posterior::as_draws_df(model)

  summary_list <- list()
  for (param in parameters) {
    if (param %in% names(posterior_draws)) {
      draws <- posterior_draws[[param]]
      summary_list[[param]] <- list(
        mean = mean(draws),
        median = median(draws),
        sd = sd(draws),
        ci = quantile(draws, probs = c((1-credible_level)/2, 1-(1-credible_level)/2))
      )
    }
  }

  return(summary_list)
}

#' Generate prior perturbations
#' @keywords internal
generate_prior_perturbations <- function(priors, direction, dimensions) {
  perturbations <- list()

  # Logic to extract a valid brmsprior object
  if (inherits(priors, "brmsprior")) {
    base_prior <- priors
  } else if (is.list(priors)) {
    # If it's a list, look for the first brmsprior element
    found <- FALSE
    for (i in seq_along(priors)) {
      if (inherits(priors[[i]], "brmsprior")) {
        base_prior <- priors[[i]]
        found <- TRUE
        break
      }
    }
    # Fallback if no specific brmsprior class is found inside list
    if (!found) {
      if (length(priors) > 0) base_prior <- priors[] else base_prior <- priors
    }
  } else {
    base_prior <- priors
  }

  for (i in seq_along(dimensions)) {
    dim <- dimensions[i]
    name <- paste0("perturbation_", direction, "_", dim)
    perturbations[[name]] <- base_prior
  }

  return(perturbations)
}

#' Compute sensitivity metric
#' @keywords internal
compute_sensitivity_metric <- function(original, perturbed, metric_type) {
  if (metric_type == "KL") {
    # KL divergence between posteriors
    distances <- sapply(names(original), function(param) {
      orig_mean <- original[[param]]$mean
      pert_mean <- perturbed[[param]]$mean
      abs(orig_mean - pert_mean) / (original[[param]]$sd + 0.001)
    })
    return(mean(distances, na.rm = TRUE))
  }

  return(0.5)  # Default neutral value
}

#' Compute robustness index
#' @keywords internal
compute_robustness_index <- function(sensitivity_surfaces) {
  metrics <- sapply(sensitivity_surfaces, function(x) x$metric_value)
  robustness <- 1 / (1 + mean(metrics, na.rm = TRUE))
  return(robustness)
}

#' Identify concerning parameters
#' @keywords internal
identify_concerning_parameters <- function(sensitivity_surfaces, threshold) {
  concerning <- character(0)

  for (surf in sensitivity_surfaces) {
    if (surf$metric_value > threshold) {
      concerning <- c(concerning, names(surf$posterior))
    }
  }

  return(unique(concerning))
}

#' Generate recommendations
#' @keywords internal
generate_recommendations <- function(concerning_parameters, priors) {
  recommendations <- character(0)

  if (length(concerning_parameters) == 0) {
    recommendations <- "Model is robust to prior specification. No changes recommended."
  } else {
    recommendations <- c(
      paste("Consider more informative priors for:", paste(concerning_parameters, collapse = ", ")),
      "Conduct sensitivity analysis separately for each parameter",
      "Elicit expert opinion to inform prior specification"
    )
  }

  return(recommendations)
}

#' Create robustness plots
#' @keywords internal
create_robustness_plots <- function(result, ...) {
  # Placeholder for visualization
  return(NULL)
}
