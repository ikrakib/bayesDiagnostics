#' Effective Sample Size Diagnostics
#'
#' Comprehensive diagnostics for effective sample size (ESS) in MCMC chains,
#' including bulk ESS, tail ESS, and per-chain analysis.
#'
#' @param model A fitted Bayesian model (brmsfit, stanfit, or compatible)
#' @param parameters Character vector of parameter names to analyze (default: all)
#' @param min_ess Numeric. Minimum acceptable ESS (default: 400)
#' @param tail_quantiles Numeric vector. Quantiles for tail ESS (default: c(0.025, 0.975))
#' @param by_chain Logical. Whether to compute ESS per chain (default: TRUE)
#' @param plot Logical. Whether to generate diagnostic plots (default: TRUE)
#' @param x Object of class `ess_diagnostics` (for print/plot methods).
#' @param ... Additional arguments passed to plotting functions
#'
#' @return An object of class \code{ess_diagnostics} containing:
#' \item{ess_summary}{Summary statistics for ESS across parameters}
#' \item{bulk_ess}{Bulk ESS for each parameter}
#' \item{tail_ess}{Tail ESS for each parameter}
#' \item{by_chain_ess}{Per-chain ESS if \code{by_chain = TRUE}}
#' \item{problematic_params}{Parameters with ESS below threshold}
#' \item{recommendations}{Specific recommendations for improving ESS}
#'
#' @details
#' Effective Sample Size (ESS) measures the number of independent samples
#' in MCMC chains after accounting for autocorrelation. This function provides:
#'
#' \itemize{
#'   \item \strong{Bulk ESS}: ESS for central posterior mass (mean, median)
#'   \item \strong{Tail ESS}: ESS for extreme quantiles (credible intervals)
#'   \item \strong{Per-chain ESS}: Identifies which chains have low ESS
#' }
#'
#' Low ESS indicates high autocorrelation and may require:
#' \itemize{
#'   \item Longer chains (more iterations)
#'   \item Better parameterization
#'   \item Stronger priors
#'   \item Different sampler settings
#' }
#'
#' @examples
#' \dontrun{
#' library(brms)
#' fit <- brm(mpg ~ hp + wt, data = mtcars)
#'
#' # Comprehensive ESS diagnostics
#' ess_diag <- effective_sample_size_diagnostics(
#'   model = fit,
#'   min_ess = 400,
#'   by_chain = TRUE
#' )
#'
#' print(ess_diag)
#' plot(ess_diag)
#' }
#'
#' @export
effective_sample_size_diagnostics <- function(model,
                                              parameters = NULL,
                                              min_ess = 400,
                                              tail_quantiles = c(0.025, 0.975),
                                              by_chain = TRUE,
                                              plot = TRUE,
                                              ...) {

  # Input validation
  checkmate::assert_multi_class(model, c("brmsfit", "stanfit"))
  checkmate::assert_character(parameters, null.ok = TRUE)
  checkmate::assert_number(min_ess, lower = 100)
  checkmate::assert_numeric(tail_quantiles, lower = 0, upper = 1, len = 2)
  checkmate::assert_logical(by_chain)
  checkmate::assert_logical(plot)

  # Get all parameters if not specified
  if (is.null(parameters)) {
    parameters <- extract_all_parameters(model)
  }

  # Extract ESS statistics
  ess_stats <- extract_ess_statistics(model, parameters)

  # Calculate bulk and tail ESS
  bulk_ess <- ess_stats[, c("variable", "ess_bulk")]
  tail_ess <- ess_stats[, c("variable", "ess_tail")]

  # Per-chain analysis
  by_chain_ess <- NULL
  if (by_chain) {
    by_chain_ess <- calculate_per_chain_ess(model, parameters)
  }

  # Identify problematic parameters
  problematic <- identify_problematic_ess(ess_stats, min_ess)

  # Generate recommendations
  recommendations <- generate_ess_recommendations(problematic, ess_stats)

  # Summary statistics
  ess_summary <- summarize_ess(ess_stats, min_ess)

  # Create result object
  result <- structure(
    list(
      ess_summary = ess_summary,
      bulk_ess = bulk_ess,
      tail_ess = tail_ess,
      by_chain_ess = by_chain_ess,
      problematic_params = problematic,
      recommendations = recommendations,
      model = model,
      min_ess = min_ess
    ),
    class = "ess_diagnostics"
  )

  # Generate plots if requested
  if (plot) {
    result$plots <- create_ess_plots(result, ...)
  }

  return(result)
}

#' @rdname effective_sample_size_diagnostics
#' @export
print.ess_diagnostics <- function(x, ...) {
  cat("\nEffective Sample Size Diagnostics\n")
  cat("==================================\n\n")

  cat("Total parameters analyzed:", nrow(x$bulk_ess), "\n")
  cat("Minimum ESS threshold:", x$min_ess, "\n\n")

  cat("ESS Summary:\n")
  print(x$ess_summary)

  if (nrow(x$problematic_params) > 0) {
    cat("\n Problematic Parameters (", nrow(x$problematic_params), "):\n", sep = "")
    print(x$problematic_params[, c("variable", "ess_bulk", "ess_tail")])

    cat("\nRecommendations:\n")
    for (rec in x$recommendations) {
      cat("  ", rec, "\n")
    }
  } else {
    cat("\n All parameters have adequate ESS\n")
  }

  invisible(x)
}

#' @rdname effective_sample_size_diagnostics
#' @export
plot.ess_diagnostics <- function(x, ...) {
  if (!is.null(x$plots)) {
    print(x$plots)
  } else {
    x$plots <- create_ess_plots(x, ...)
    print(x$plots)
  }
  invisible(x)
}

# ============ HELPER FUNCTIONS ============

#' Extract All Parameters
#' @keywords internal
extract_all_parameters <- function(model) {
  if (inherits(model, "brmsfit")) {
    summ <- posterior::summarise_draws(model)
    return(summ$variable)
  }
  return(character(0))
}

#' Extract ESS Statistics
#' @keywords internal
extract_ess_statistics <- function(model, parameters) {
  if (inherits(model, "brmsfit")) {
    summ <- posterior::summarise_draws(model)
    summ <- summ[summ$variable %in% parameters, ]
    return(summ)
  }
  return(data.frame())
}

#' Calculate Per-Chain ESS
#' @keywords internal
calculate_per_chain_ess <- function(model, parameters) {
  if (!inherits(model, "brmsfit")) return(NULL)

  # Extract draws as array (iterations x chains x parameters)
  draws_array <- posterior::as_draws_array(model)

  chain_ess_list <- list()

  for (param in parameters) {
    if (param %in% dimnames(draws_array)[[3]]) {
      for (chain_id in 1:dim(draws_array)[2]) {
        chain_draws <- draws_array[, chain_id, param]
        ess_val <- posterior::ess_basic(chain_draws)

        chain_ess_list[[length(chain_ess_list) + 1]] <- data.frame(
          parameter = param,
          chain = chain_id,
          ess = ess_val
        )
      }
    }
  }

  if (length(chain_ess_list) > 0) {
    return(do.call(rbind, chain_ess_list))
  }

  return(NULL)
}

#' Identify Problematic ESS
#' @keywords internal
identify_problematic_ess <- function(ess_stats, min_ess) {
  problematic <- ess_stats[
    ess_stats$ess_bulk < min_ess | ess_stats$ess_tail < min_ess,
  ]

  if (nrow(problematic) > 0) {
    problematic$issue <- ifelse(
      problematic$ess_bulk < min_ess & problematic$ess_tail < min_ess,
      "Both bulk and tail ESS low",
      ifelse(problematic$ess_bulk < min_ess, "Bulk ESS low", "Tail ESS low")
    )
  }

  return(problematic)
}

#' Generate ESS Recommendations
#' @keywords internal
generate_ess_recommendations <- function(problematic, ess_stats) {
  if (nrow(problematic) == 0) {
    return(character(0))
  }

  recommendations <- character()

  # General recommendations
  n_low_bulk <- sum(problematic$ess_bulk < ess_stats$ess_bulk[1])
  n_low_tail <- sum(problematic$ess_tail < ess_stats$ess_tail[1])

  if (n_low_bulk > nrow(ess_stats) * 0.5) {
    recommendations <- c(recommendations,
                         "Consider increasing the number of iterations (many parameters have low bulk ESS)")
  }

  if (n_low_tail > 0) {
    recommendations <- c(recommendations,
                         sprintf("%d parameter(s) have low tail ESS - consider checking prior specifications",
                                 n_low_tail))
  }

  if (nrow(problematic) < 5) {
    recommendations <- c(recommendations,
                         paste("Focus on improving ESS for:",
                               paste(problematic$variable, collapse = ", ")))
  }

  return(recommendations)
}

#' Summarize ESS
#' @keywords internal
summarize_ess <- function(ess_stats, min_ess) {
  summary <- data.frame(
    metric = c("Bulk ESS", "Tail ESS"),
    min = c(min(ess_stats$ess_bulk, na.rm = TRUE),
            min(ess_stats$ess_tail, na.rm = TRUE)),
    median = c(median(ess_stats$ess_bulk, na.rm = TRUE),
               median(ess_stats$ess_tail, na.rm = TRUE)),
    max = c(max(ess_stats$ess_bulk, na.rm = TRUE),
            max(ess_stats$ess_tail, na.rm = TRUE)),
    n_below_threshold = c(
      sum(ess_stats$ess_bulk < min_ess, na.rm = TRUE),
      sum(ess_stats$ess_tail < min_ess, na.rm = TRUE)
    )
  )

  return(summary)
}

#' Create ESS Plots
#' @keywords internal
create_ess_plots <- function(result, ...) {
  # Plot 1: Bulk vs Tail ESS
  plot_data <- data.frame(
    parameter = result$bulk_ess$variable,
    bulk = result$bulk_ess$ess_bulk,
    tail = result$tail_ess$ess_tail
  )

  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = bulk, y = tail)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = result$min_ess, color = "red", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = result$min_ess, color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Bulk vs Tail ESS", x = "Bulk ESS", y = "Tail ESS")

  # Plot 2: ESS by parameter
  plot_data_long <- tidyr::pivot_longer(
    plot_data,
    cols = c("bulk", "tail"),
    names_to = "ess_type",
    values_to = "ess"
  )

  p2 <- ggplot2::ggplot(plot_data_long,
                        ggplot2::aes(x = parameter, y = ess, fill = ess_type)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_hline(yintercept = result$min_ess, color = "red", linetype = "dashed") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "ESS by Parameter", x = "Parameter", y = "ESS")

  # Combine plots
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}

# Global variables
utils::globalVariables(c(
  "parameter", "bulk", "tail", "ess_type", "ess",
  "ess_bulk", "ess_tail", "variable", "issue"
))
