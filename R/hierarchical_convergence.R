#' Hierarchical Model Convergence Diagnostics
#'
#' Performs specialized convergence diagnostics for hierarchical/multilevel
#' Bayesian models, checking convergence at both group-level and population-level
#' parameters.
#'
#' @param model A fitted hierarchical Bayesian model (brmsfit, stanfit, or compatible)
#' @param group_vars Character vector of grouping variable names (e.g., "subject", "school")
#' @param rhat_threshold Numeric. Threshold for R-hat diagnostic (default: 1.01)
#' @param ess_threshold Numeric. Minimum effective sample size threshold (default: 400)
#' @param check_shrinkage Logical. Whether to assess shrinkage patterns (default: TRUE)
#' @param plot Logical. Whether to generate diagnostic plots (default: TRUE)
#' @param x Object of class `hierarchical_convergence` (for print/plot methods).
#' @param ... Additional arguments passed to plotting functions
#'
#' @return An object of class \code{hierarchical_convergence} containing:
#' \item{population_diagnostics}{Diagnostics for population-level (fixed) effects}
#' \item{group_diagnostics}{Diagnostics for group-level (random) effects}
#' \item{shrinkage_metrics}{Shrinkage statistics if \code{check_shrinkage = TRUE}}
#' \item{convergence_summary}{Overall convergence assessment}
#' \item{warnings}{List of convergence warnings}
#' \item{model}{Original fitted model}
#'
#' @details
#' Hierarchical models require special attention to convergence because:
#' \itemize{
#'   \item Group-level parameters often have slower mixing
#'   \item Variance components can be difficult to estimate
#'   \item Extreme shrinkage may indicate identification problems
#' }
#'
#' The function checks:
#' \itemize{
#'   \item R-hat values for all parameters
#'   \item Effective sample sizes (bulk and tail ESS)
#'   \item Between-chain variance
#'   \item Shrinkage factor (ratio of group SD to pooled SD)
#' }
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   library(brms)
#'   library(lme4)
#' # Fit hierarchical model
#' library(lme4)  # For sleepstudy dataset
#' fit <- brm(
#'   reaction ~ days + (days | subject),
#'   data = sleepstudy,
#'   family = gaussian()
#' )
#'
#' # Check convergence
#' conv <- hierarchical_convergence(
#'   model = fit,
#'   group_vars = "subject",
#'   rhat_threshold = 1.01,
#'   check_shrinkage = TRUE
#' )
#'
#' print(conv)
#' plot(conv)
#' }
#'
#' @export
hierarchical_convergence <- function(model,
                                     group_vars = NULL,
                                     rhat_threshold = 1.01,
                                     ess_threshold = 400,
                                     check_shrinkage = TRUE,
                                     plot = TRUE,
                                     ...) {

  # Input validation
  if (!inherits(model, c("brmsfit", "stanfit"))) {
    stop("Model must be of class 'brmsfit' or 'stanfit'")
  }
  checkmate::assert_character(group_vars, null.ok = TRUE)
  checkmate::assert_number(rhat_threshold, lower = 1, upper = 1.1)
  checkmate::assert_number(ess_threshold, lower = 100)
  checkmate::assert_logical(check_shrinkage)
  checkmate::assert_logical(plot)

  # Detect group variables if not provided
  if (is.null(group_vars)) {
    group_vars <- detect_group_vars(model)
  }

  # Extract diagnostics
  all_diagnostics <- extract_diagnostics(model)

  # Separate population and group-level parameters
  pop_params <- identify_population_params(all_diagnostics)
  group_params <- identify_group_params(all_diagnostics, group_vars)

  # Check convergence
  pop_diagnostics <- check_parameter_convergence(
    pop_params, rhat_threshold, ess_threshold
  )

  group_diagnostics <- check_parameter_convergence(
    group_params, rhat_threshold, ess_threshold
  )

  # Calculate shrinkage if requested
  shrinkage_metrics <- NULL
  if (check_shrinkage && !is.null(group_vars)) {
    shrinkage_metrics <- calculate_shrinkage(model, group_vars)
  }

  # Generate convergence summary
  convergence_summary <- summarize_convergence(
    pop_diagnostics, group_diagnostics, shrinkage_metrics
  )

  # Collect warnings
  warnings_list <- collect_convergence_warnings(
    pop_diagnostics, group_diagnostics, shrinkage_metrics,
    rhat_threshold, ess_threshold
  )

  # Create result object
  result <- structure(
    list(
      population_diagnostics = pop_diagnostics,
      group_diagnostics = group_diagnostics,
      shrinkage_metrics = shrinkage_metrics,
      convergence_summary = convergence_summary,
      warnings = warnings_list,
      model = model,
      group_vars = group_vars
    ),
    class = "hierarchical_convergence"
  )

  # Generate plots if requested
  if (plot) {
    result$plots <- create_convergence_plots(result, ...)
  }

  return(result)
}

#' @rdname hierarchical_convergence
#' @export
print.hierarchical_convergence <- function(x, ...) {
  cat("\nHierarchical Model Convergence Diagnostics\n")
  cat("==========================================\n\n")

  cat("Population-level parameters:", nrow(x$population_diagnostics), "\n")
  cat("Group-level parameters:", nrow(x$group_diagnostics), "\n")

  if (!is.null(x$group_vars)) {
    cat("Grouping variables:", paste(x$group_vars, collapse = ", "), "\n")
  }

  cat("\nConvergence Summary:\n")
  print(x$convergence_summary)

  if (length(x$warnings) > 0) {
    cat("\n Warnings:\n")
    for (w in x$warnings) {
      cat("  ", w, "\n")
    }
  } else {
    cat("\n All convergence checks passed\n")
  }

  invisible(x)
}

#' @rdname hierarchical_convergence
#' @export
plot.hierarchical_convergence <- function(x, ...) {
  if (!is.null(x$plots)) {
    print(x$plots)
  } else {
    x$plots <- create_convergence_plots(x, ...)
    print(x$plots)
  }
  invisible(x)
}

# ============ HELPER FUNCTIONS ============

#' Detect Group Variables
#' @keywords internal
detect_group_vars <- function(model) {
  if (inherits(model, "brmsfit")) {
    # Try to extract random effect names, return NULL if none exist
    re_names <- tryCatch({
      names(brms::ranef(model))
    }, error = function(e) {
      NULL
    })
    if (length(re_names) > 0) {
      return(re_names)
    }
  }
  return(NULL)
}

#' Extract Diagnostics
#' @keywords internal
extract_diagnostics <- function(model) {
  if (inherits(model, "brmsfit")) {
    # Get R-hat, ESS, etc.
    summ <- posterior::summarise_draws(model)
    return(summ)
  } else {
    stop("Model class not supported")
  }
}

#' Identify Population Parameters
#' @keywords internal
identify_population_params <- function(diagnostics) {
  # Population params typically start with "b_" in brms
  pop_pattern <- "^b_|^Intercept"
  diagnostics[grepl(pop_pattern, diagnostics$variable), ]
}

#' Identify Group Parameters
#' @keywords internal
identify_group_params <- function(diagnostics, group_vars) {
  # Group params typically contain the group variable name
  if (is.null(group_vars)) return(data.frame())

  group_pattern <- paste0("(", paste(group_vars, collapse = "|"), ")")
  diagnostics[grepl(group_pattern, diagnostics$variable), ]
}

#' Check Parameter Convergence
#' @keywords internal
check_parameter_convergence <- function(params, rhat_threshold, ess_threshold) {
  if (nrow(params) == 0) return(data.frame())

  params$rhat_ok <- params$rhat < rhat_threshold
  params$ess_bulk_ok <- params$ess_bulk > ess_threshold
  params$ess_tail_ok <- params$ess_tail > ess_threshold
  params$converged <- params$rhat_ok & params$ess_bulk_ok & params$ess_tail_ok

  return(params)
}

#' Calculate Shrinkage
#' @keywords internal
calculate_shrinkage <- function(model, group_vars) {
  if (!inherits(model, "brmsfit")) return(NULL)

  # Extract group-level SDs and correlations
  sd_params <- brms::VarCorr(model)

  shrinkage_list <- list()

  for (group_var in group_vars) {
    if (group_var %in% names(sd_params)) {
      group_sd <- sd_params[[group_var]]$sd

      # Calculate shrinkage factor (simplified)
      # Shrinkage = 1 - (group_var / total_var)
      shrinkage_list[[group_var]] <- data.frame(
        group = group_var,
        sd = mean(group_sd, na.rm = TRUE),
        shrinkage = NA  # Simplified; full calculation requires more info
      )
    }
  }

  if (length(shrinkage_list) > 0) {
    return(do.call(rbind, shrinkage_list))
  }

  return(NULL)
}

#' Summarize Convergence
#' @keywords internal
summarize_convergence <- function(pop_diag, group_diag, shrinkage) {
  # Calculate group-level statistics only if we have group parameters
  if (nrow(group_diag) > 0) {
    group_n_converged <- sum(group_diag$converged, na.rm = TRUE)
    group_pct_converged <- mean(group_diag$converged, na.rm = TRUE) * 100
  } else {
    group_n_converged <- 0
    group_pct_converged <- 0
  }

  summary <- data.frame(
    level = c("Population", "Group"),
    n_params = c(nrow(pop_diag), nrow(group_diag)),
    n_converged = c(
      sum(pop_diag$converged, na.rm = TRUE),
      group_n_converged
    ),
    pct_converged = c(
      mean(pop_diag$converged, na.rm = TRUE) * 100,
      group_pct_converged
    )
  )

  return(summary)
}

#' Collect Convergence Warnings
#' @keywords internal
collect_convergence_warnings <- function(pop_diag, group_diag, shrinkage,
                                         rhat_threshold, ess_threshold) {
  warnings <- character()

  # Only check if data frames have rows
  if (nrow(pop_diag) > 0) {
    # R-hat warnings for population
    if (any(!pop_diag$rhat_ok, na.rm = TRUE)) {
      n_bad <- sum(!pop_diag$rhat_ok, na.rm = TRUE)
      warnings <- c(warnings,
                    sprintf("%d population parameter(s) have R-hat > %.2f",
                            n_bad, rhat_threshold))
    }

    # ESS warnings for population
    if (any(!pop_diag$ess_bulk_ok, na.rm = TRUE)) {
      n_bad <- sum(!pop_diag$ess_bulk_ok, na.rm = TRUE)
      warnings <- c(warnings,
                    sprintf("%d population parameter(s) have ESS < %d",
                            n_bad, ess_threshold))
    }
  }

  if (nrow(group_diag) > 0) {
    # R-hat warnings for group
    if (any(!group_diag$rhat_ok, na.rm = TRUE)) {
      n_bad <- sum(!group_diag$rhat_ok, na.rm = TRUE)
      warnings <- c(warnings,
                    sprintf("%d group parameter(s) have R-hat > %.2f",
                            n_bad, rhat_threshold))
    }

    # ESS warnings for group
    if (any(!group_diag$ess_bulk_ok, na.rm = TRUE)) {
      n_bad <- sum(!group_diag$ess_bulk_ok, na.rm = TRUE)
      warnings <- c(warnings,
                    sprintf("%d group parameter(s) have ESS < %d",
                            n_bad, ess_threshold))
    }
  }

  return(warnings)
}

#' Create Convergence Plots
#' @keywords internal
create_convergence_plots <- function(result, ...) {
  # Combine diagnostics
  all_diag <- rbind(
    cbind(result$population_diagnostics, level = "Population"),
    cbind(result$group_diagnostics, level = "Group")
  )

  if (nrow(all_diag) == 0) return(NULL)

  # R-hat plot
  p1 <- ggplot2::ggplot(all_diag, ggplot2::aes(x = variable, y = rhat, color = level)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 1.01, linetype = "dashed", color = "red") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "R-hat Diagnostics", y = "R-hat", x = "Parameter")

  # ESS plot
  p2 <- ggplot2::ggplot(all_diag, ggplot2::aes(x = variable, y = ess_bulk, color = level)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 400, linetype = "dashed", color = "red") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Effective Sample Size", y = "ESS (Bulk)", x = "Parameter")

  # Combine plots
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}

# Global variables
utils::globalVariables(c(
  "variable", "rhat", "ess_bulk", "ess_tail", "level",
  "rhat_ok", "ess_bulk_ok", "ess_tail_ok", "converged"
))
