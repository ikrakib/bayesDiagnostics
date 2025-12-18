#' Comprehensive Model Comparison Suite
#'
#' Compares multiple Bayesian models using information criteria (LOO, WAIC, Bayes R2)
#' and generates comparison tables with rankings and visualizations.
#'
#' @param ... Multiple brmsfit objects to compare
#' @param criterion Character vector of criteria to use.
#'   Options: "loo" (default), "waic", "bayes_r2", "all"
#' @param plot Logical. Generate comparison plots? (default: TRUE)
#' @param detailed Logical. Return detailed statistics? (default: TRUE)
#'
#' @return A list of class `model_comparison` containing:
#'   \item{comparison_table}{Data frame with model rankings and IC values}
#'   \item{ic_differences}{Data frame with IC differences and weights}
#'   \item{model_names}{Character vector of model names}
#'   \item{plots}{List of ggplot objects (if plot = TRUE)}
#'   \item{criterion_used}{Character vector of criteria used}
#'
#' @importFrom brms loo waic bayes_R2
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_viridis_c facet_wrap labs theme_minimal element_text
#' @export
model_comparison_suite <- function(..., criterion = "loo",
                                   plot = TRUE, detailed = TRUE) {

  # Collect models and validate
  models <- list(...)
  n_models <- length(models)

  if (n_models < 2) {
    stop("At least 2 models must be provided for comparison.")
  }

  # Validate all inputs are brmsfit objects
  for (i in seq_len(n_models)) {
    checkmate::assert_class(models[[i]], "brmsfit",
                            .var.name = paste0("Argument ", i))
  }

  # Get model names
  model_names <- names(models)
  if (is.null(model_names)) {
    model_names <- paste0("Model_", seq_len(n_models))
  }

  # Validate and expand criterion
  valid_criteria <- c("loo", "waic", "bayes_r2")
  if ("all" %in% criterion) {
    criterion <- valid_criteria
  } else {
    criterion <- match.arg(criterion, valid_criteria, several.ok = TRUE)
  }

  # Initialize result storage
  ic_results <- list()

  # Compute LOO with moment matching for robust handling of influential observations
  if ("loo" %in% criterion) {
    loo_list <- list()
    for (i in seq_len(n_models)) {
      tryCatch({
        loo_list[[i]] <- brms::loo(models[[i]], moment_match = TRUE)
      }, error = function(e) {
        warning(sprintf("Failed to compute LOO for %s: %s", model_names[i], e$message))
        loo_list[[i]] <<- NA
      })
    }
    ic_results$loo <- loo_list
  }

  # Compute WAIC
  if ("waic" %in% criterion) {
    waic_list <- list()
    for (i in seq_len(n_models)) {
      tryCatch({
        waic_list[[i]] <- brms::waic(models[[i]])
      }, error = function(e) {
        warning(sprintf("Failed to compute WAIC for %s: %s", model_names[i], e$message))
        waic_list[[i]] <<- NA
      })
    }
    ic_results$waic <- waic_list
  }

  # Compute Bayes R²
  if ("bayes_r2" %in% criterion) {
    bayes_r2_list <- list()
    for (i in seq_len(n_models)) {
      tryCatch({
        r2_vals <- brms::bayes_R2(models[[i]])
        bayes_r2_list[[i]] <- list(
          estimate = mean(r2_vals[, "Estimate"]),
          se = sd(r2_vals[, "Estimate"])
        )
      }, error = function(e) {
        warning(sprintf("Failed to compute Bayes R2 for %s: %s", model_names[i], e$message))
        bayes_r2_list[[i]] <<- NA
      })
    }
    ic_results$bayes_r2 <- bayes_r2_list
  }

  # Build comparison table
  comparison_df <- data.frame(
    Model = model_names,
    stringsAsFactors = FALSE
  )

  # Add LOO results if available
  if (!is.null(ic_results$loo)) {
    loo_elpd <- rep(NA, n_models)
    loo_se <- rep(NA, n_models)
    for (i in seq_len(n_models)) {
      if (is.list(ic_results$loo[[i]])) {
        loo_elpd[i] <- ic_results$loo[[i]]$estimates["elpd_loo", "Estimate"]
        loo_se[i] <- ic_results$loo[[i]]$estimates["elpd_loo", "SE"]
      }
    }
    comparison_df$LOO_ELPD <- loo_elpd
    comparison_df$LOO_SE <- loo_se
  }

  # Add WAIC results if available
  if (!is.null(ic_results$waic)) {
    waic_val <- rep(NA, n_models)
    waic_se <- rep(NA, n_models)
    for (i in seq_len(n_models)) {
      if (is.list(ic_results$waic[[i]])) {
        waic_val[i] <- ic_results$waic[[i]]$estimates["waic", "Estimate"]
        waic_se[i] <- ic_results$waic[[i]]$estimates["waic", "SE"]
      }
    }
    comparison_df$WAIC <- waic_val
    comparison_df$WAIC_SE <- waic_se
  }

  # Add Bayes R² results if available
  if (!is.null(ic_results$bayes_r2)) {
    r2_mean <- rep(NA, n_models)
    r2_se <- rep(NA, n_models)
    for (i in seq_len(n_models)) {
      if (is.list(ic_results$bayes_r2[[i]])) {
        r2_mean[i] <- ic_results$bayes_r2[[i]]$estimate
        r2_se[i] <- ic_results$bayes_r2[[i]]$se
      }
    }
    comparison_df$Bayes_R2 <- r2_mean
    comparison_df$Bayes_R2_SE <- r2_se
  }

  # Compute IC differences and weights
  ic_diff_list <- NULL

  if (!is.null(ic_results$loo) && "LOO_ELPD" %in% names(comparison_df)) {
    valid_loo <- !is.na(comparison_df$LOO_ELPD)
    if (any(valid_loo)) {
      best_loo <- max(comparison_df$LOO_ELPD[valid_loo], na.rm = TRUE)
      loo_diff <- rep(NA_real_, nrow(comparison_df))
      loo_diff[valid_loo] <- best_loo - comparison_df$LOO_ELPD[valid_loo]

      loo_weights <- rep(NA_real_, nrow(comparison_df))
      if (sum(valid_loo) >= 2) {
        valid_weights <- exp(-0.5 * loo_diff[valid_loo])
        loo_weights[valid_loo] <- valid_weights / sum(valid_weights)
      }

      ic_diff_list <- data.frame(
        Model = model_names,
        IC_Type = "LOO",
        Delta_IC = loo_diff,
        Model_Weight = loo_weights,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!is.null(ic_results$waic) && "WAIC" %in% names(comparison_df)) {
    valid_waic <- !is.na(comparison_df$WAIC)
    if (any(valid_waic)) {
      best_waic <- min(comparison_df$WAIC[valid_waic], na.rm = TRUE)
      waic_diff <- rep(NA_real_, nrow(comparison_df))
      waic_diff[valid_waic] <- comparison_df$WAIC[valid_waic] - best_waic

      waic_weights <- rep(NA_real_, nrow(comparison_df))
      if (sum(valid_waic) >= 2) {
        valid_weights <- exp(-0.5 * waic_diff[valid_waic])
        waic_weights[valid_waic] <- valid_weights / sum(valid_weights)
      }

      waic_diff_df <- data.frame(
        Model = model_names,
        IC_Type = "WAIC",
        Delta_IC = waic_diff,
        Model_Weight = waic_weights,
        stringsAsFactors = FALSE
      )

      if (is.null(ic_diff_list)) {
        ic_diff_list <- waic_diff_df
      } else {
        ic_diff_list <- rbind(ic_diff_list, waic_diff_df)
      }
    }
  }

  # Generate plots
  plot_list <- NULL
  if (plot && !is.null(ic_diff_list)) {
    plot_list <- list()

    if ("LOO" %in% ic_diff_list$IC_Type) {
      loo_data <- ic_diff_list[ic_diff_list$IC_Type == "LOO", ]
      plot_list$loo <- ggplot2::ggplot(loo_data,
                                       ggplot2::aes(x = stats::reorder(Model, Delta_IC),
                                                    y = Delta_IC,
                                                    fill = Model_Weight)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::labs(title = "LOO Model Comparison",
                      x = "Model",
                      y = "Delta ELPD (relative to best)",
                      fill = "Model Weight") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }

    if ("WAIC" %in% ic_diff_list$IC_Type) {
      waic_data <- ic_diff_list[ic_diff_list$IC_Type == "WAIC", ]
      plot_list$waic <- ggplot2::ggplot(waic_data,
                                        ggplot2::aes(x = stats::reorder(Model, Delta_IC),
                                                     y = Delta_IC,
                                                     fill = Model_Weight)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::labs(title = "WAIC Model Comparison",
                      x = "Model",
                      y = "Delta WAIC (relative to best)",
                      fill = "Model Weight") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  }

  # Build result object
  result <- list(
    comparison_table = comparison_df,
    ic_differences = ic_diff_list,
    model_names = model_names,
    plots = plot_list,
    criterion_used = criterion,
    detailed = detailed
  )

  class(result) <- "model_comparison"
  return(result)
}

#' @export
print.model_comparison <- function(x, ...) {
  cat("\n=== Model Comparison Results ===\n\n")

  cat("Models compared:", length(x$model_names), "\n")
  cat("Criteria used:", paste(x$criterion_used, collapse = ", "), "\n\n")

  cat("COMPARISON TABLE:\n")
  print(x$comparison_table, row.names = FALSE)

  if (!is.null(x$ic_differences)) {
    cat("\n\nIC DIFFERENCES & MODEL WEIGHTS:\n")
    print(x$ic_differences, row.names = FALSE)
  }
}

#' @export
plot.model_comparison <- function(x, ...) {
  if (is.null(x$plots)) {
    message("No plots available. Set plot = TRUE when calling model_comparison_suite().")
    return(invisible(NULL))
  }

  for (plot_name in names(x$plots)) {
    print(x$plots[[plot_name]])
  }
}
