#' PPC Cross-Validation (LOO-PIT)
#'
#' Performs Leave-One-Out (LOO) Probability Integral Transform (PIT) checks.
#' A uniform distribution of PIT values indicates a well-calibrated model.
#'
#' @param model A fitted brmsfit object.
#' @param observed_y Numeric vector of response variable.
#' @param n_draws Integer. Number of posterior draws to use for calculation.
#'   If NULL, uses all draws (recommended for accuracy).
#'
#' @return A list containing PIT values and a diagnostic plot object.
#' @importFrom loo loo
#' @importFrom brms posterior_predict add_criterion
#' @importFrom ggplot2 ggplot aes geom_histogram geom_hline labs theme_minimal after_stat
#' @importFrom matrixStats logSumExp
#' @export
ppc_crossvalidation <- function(model, observed_y, n_draws = NULL) {
  checkmate::assert_class(model, "brmsfit")

  # 1. Compute LOO object if not present
  # Use save_psis = TRUE to ensure importance weights are stored
  if (is.null(model$criteria$loo)) {
    model <- brms::add_criterion(model, "loo", save_psis = TRUE)
  }

  # 2. Extract LOO object and PSIS weights with save_psis = TRUE
  loo_obj <- brms::loo(model, save_psis = TRUE)
  psis_obj <- loo_obj$psis_object

  # 3. Attempt to extract log weights from PSIS object
  log_weights <- NULL

  # Try method 1: Direct extraction from list
  if (is.list(psis_obj) && "log_weights" %in% names(psis_obj)) {
    log_weights <- psis_obj$log_weights
  }
  # Try method 2: PSIS object is itself the matrix
  else if (is.matrix(psis_obj)) {
    log_weights <- psis_obj
  }
  # Try method 3: Extract log_weights from PSIS object
  else {
    tryCatch({
      log_weights <- attr(psis_obj, "log_weights")
    }, error = function(e) {
      # Silent fallback - will use unweighted PIT
    })
  }

  # If we successfully extracted log_weights, use weighted PIT
  use_weights <- !is.null(log_weights)

  if (use_weights) {
    # Ensure it is a matrix
    if (!is.matrix(log_weights)) {
      log_weights <- as.matrix(log_weights)
    }

    # Normalize weights for each observation (column-wise)
    imp_weights <- apply(log_weights, 2, function(lw) {
      exp(lw - matrixStats::logSumExp(lw))
    })

    # Transpose: we want S x N (draws x observations)
    imp_weights <- t(imp_weights)
  }

  # Get posterior predictions
  yrep <- brms::posterior_predict(model)

  # Validate and align dimensions if using weights
  if (use_weights) {
    if (nrow(yrep) != nrow(imp_weights)) {
      if (nrow(yrep) == ncol(imp_weights)) {
        imp_weights <- t(imp_weights)
      } else {
        # If dimension mismatch, fall back to unweighted
        use_weights <- FALSE
      }
    }
  }

  # Subsampling if requested
  if (!is.null(n_draws) && n_draws < nrow(yrep)) {
    idx <- sample(seq_len(nrow(yrep)), n_draws)
    yrep <- yrep[idx, , drop = FALSE]

    if (use_weights) {
      imp_weights <- imp_weights[idx, , drop = FALSE]
      # Renormalize weights for each observation
      col_sums <- colSums(imp_weights)
      col_sums[col_sums == 0] <- 1
      imp_weights <- sweep(imp_weights, 2, col_sums, "/")
    }
  }

  # 4. Calculate PIT Values
  pit_values <- numeric(length(observed_y))
  n_obs <- min(length(observed_y), ncol(yrep))

  if (use_weights) {
    # Weighted PIT
    for (i in seq_len(n_obs)) {
      pit_values[i] <- sum(imp_weights[, i] * (yrep[, i] <= observed_y[i]))
    }
  } else {
    # Unweighted PIT (uniform weights = simple empirical CDF)
    for (i in seq_len(n_obs)) {
      pit_values[i] <- mean(yrep[, i] <= observed_y[i])
    }
  }

  # 5. Create diagnostic plot
  plot_data <- data.frame(PIT = pit_values)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PIT)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 20, fill = "lightblue", color = "black") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "LOO-PIT Cross-Validation Check",
                  subtitle = "Ideally should look Uniform (flat line at 1.0)",
                  x = "Probability Integral Transform (PIT)",
                  y = "Density") +
    ggplot2::theme_minimal()

  result <- list(
    pit_values = pit_values,
    loo_result = loo_obj,
    plot = p,
    weighted = use_weights
  )
  class(result) <- "ppc_crossvalidation"
  return(result)
}

#' @export
plot.ppc_crossvalidation <- function(x, ...) {
  print(x$plot)
}

#' @export
print.ppc_crossvalidation <- function(x, ...) {
  cat("\n=== LOO Cross-Validation Check ===\n")
  print(x$loo_result)
  method <- if (x$weighted) "weighted LOO-PIT" else "unweighted empirical PIT"
  cat("\n[NOTE] PIT computed using", method, "\n")
  cat("[NOTE] Use plot() to inspect calibration.\n")
}
