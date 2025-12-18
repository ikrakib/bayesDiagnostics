#' Bayesian Factor Comparison Between Models
#'
#' Computes and compares Bayes Factors between two or more Bayesian models
#' using both marginal likelihood approximation and bridge sampling methods.
#'
#' @param ... Named or unnamed brmsfit objects to compare
#' @param method Character. Method for computing marginal likelihood.
#'   Options: "bridge_sampling" (default), "waic"
#' @param repetitions Integer. Number of bridge sampling repetitions (default: 5)
#' @param silent Logical. Suppress messages? (default: TRUE)
#'
#' @return A list of class `bayes_factor_comparison` containing:
#' \item{bayes_factor}{Bayes Factor for 2-model comparison}
#' \item{log_bf}{Log Bayes Factor}
#' \item{interpretation}{Interpretation of BF strength}
#' \item{marginal_likelihoods}{Data frame with model-level MLs}
#' \item{model_names}{Character vector of model names}
#' \item{pairwise_comparisons}{Data frame of pairwise BFs if 3+ models}
#' @export
bayes_factor_comparison <- function(...,
                                    method = "bridge_sampling",
                                    repetitions = 5,
                                    silent = TRUE) {
  models <- list(...)
  n_models <- length(models)

  if (n_models < 2) {
    stop("At least 2 models required for Bayes Factor comparison")
  }

  # Validate inputs
  for (i in seq_len(n_models)) {
    checkmate::assert_class(models[[i]], "brmsfit", .var.name = paste0("model", i))
  }

  method <- match.arg(method, c("bridge_sampling", "waic"))

  # Handle model names robustly
  model_names <- names(models)
  if (is.null(model_names)) model_names <- rep("", n_models)
  missing_names <- model_names == "" | is.na(model_names)
  if (any(missing_names)) {
    model_names[missing_names] <- paste0("Model", which(missing_names))
  }

  ml_results <- vector("list", n_models)

  if (method == "bridge_sampling") {
    for (i in seq_len(n_models)) {
      tryCatch({
        bs_result <- bridgesampling::bridge_sampler(models[[i]],
                                                    silent = silent,
                                                    repetitions = repetitions)
        ml_results[[i]] <- list(
          log_marginal_likelihood = bs_result$logml,
          se = if (!is.null(bs_result$method_SE) && length(bs_result$method_SE) > 0) bs_result$method_SE else NA_real_
        )
      }, error = function(e) {
        warning(sprintf("Bridge sampling failed for %s: %s. Attempting WAIC as fallback.", model_names[i], e$message))
        ml_results[[i]] <<- NA
      })
    }
  } else if (method == "waic") {
    for (i in seq_len(n_models)) {
      tryCatch({
        waic_val <- suppressWarnings(brms::waic(models[[i]]))
        ml_results[[i]] <- list(
          log_marginal_likelihood = -as.numeric(waic_val$estimates["waic", "Estimate"]) / 2,
          se = as.numeric(waic_val$estimates["waic", "SE"]) / 2
        )
      }, error = function(e) {
        warning(sprintf("WAIC computation failed for %s: %s", model_names[i], e$message))
        ml_results[[i]] <<- NA
      })
    }
  }

  # Build ml_df safely
  ml_df <- data.frame(
    Model = model_names,
    Log_Marginal_Likelihood = rep(NA_real_, n_models),
    SE = rep(NA_real_, n_models),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_models)) {
    item <- ml_results[[i]]

    if (is.list(item) && !all(is.na(item))) {
      if (!is.null(item$log_marginal_likelihood) &&
          length(item$log_marginal_likelihood) > 0 &&
          !is.na(item$log_marginal_likelihood[1])) {
        ml_df$Log_Marginal_Likelihood[i] <- as.numeric(item$log_marginal_likelihood[1])
      } else {
        ml_df$Log_Marginal_Likelihood[i] <- NA_real_
      }

      se_val <- if (!is.null(item$se)) item$se else NA_real_

      if (is.null(se_val) || length(se_val) == 0 || all(is.na(se_val))) {
        ml_df$SE[i] <- NA_real_
      } else {
        ml_df$SE[i] <- as.numeric(se_val[1])
      }
    } else {
      ml_df$Log_Marginal_Likelihood[i] <- NA_real_
      ml_df$SE[i] <- NA_real_
    }
  }

  pairwise_bf <- NULL
  bayes_factor <- NULL
  log_bf <- NULL
  interpretation <- NULL

  if (n_models == 2) {
    if (any(is.na(ml_df$Log_Marginal_Likelihood[1:2]))) {
      warning("One or more marginal likelihoods are NA. Bayes Factor may be NA.")
    }

    log_bf <- ml_df$Log_Marginal_Likelihood[1] - ml_df$Log_Marginal_Likelihood[2]
    bayes_factor <- exp(log_bf)
    interpretation <- interpret_bayes_factor(bayes_factor)

  } else {
    pairwise_bf <- data.frame(
      Model_A = character(0),
      Model_B = character(0),
      Log_BF_AB = numeric(0),
      BF_AB = numeric(0),
      Interpretation = character(0),
      stringsAsFactors = FALSE
    )

    idx <- 1
    for (i in 1:(n_models - 1)) {
      for (j in (i + 1):n_models) {
        log_bf_ij <- ml_df$Log_Marginal_Likelihood[i] - ml_df$Log_Marginal_Likelihood[j]
        bf_ij <- exp(log_bf_ij)
        interp <- interpret_bayes_factor(bf_ij)

        pairwise_bf[idx, ] <- list(
          Model_A = model_names[i],
          Model_B = model_names[j],
          Log_BF_AB = as.numeric(log_bf_ij),
          BF_AB = as.numeric(bf_ij),
          Interpretation = interp
        )

        idx <- idx + 1
      }
    }

    log_bf <- ml_df$Log_Marginal_Likelihood[1] - ml_df$Log_Marginal_Likelihood[2]
    bayes_factor <- exp(log_bf)
    interpretation <- interpret_bayes_factor(bayes_factor)
  }

  result <- list(
    bayes_factor = bayes_factor,
    log_bf = log_bf,
    interpretation = interpretation,
    marginal_likelihoods = ml_df,
    model_names = model_names,
    method_used = method,
    models_compared = n_models,
    pairwise_comparisons = pairwise_bf
  )

  class(result) <- "bayes_factor_comparison"
  result
}

#' Interpret Bayes Factor Strength
#' @keywords internal
interpret_bayes_factor <- function(bf) {
  if (is.na(bf)) return("BF not available")
  if (bf < 1) {
    "Weak evidence for model 2"
  } else if (bf < 3) {
    "Weak evidence for model 1"
  } else if (bf < 10) {
    "Moderate evidence for model 1"
  } else if (bf < 30) {
    "Strong evidence for model 1"
  } else if (bf < 100) {
    "Very strong evidence for model 1"
  } else {
    "Decisive evidence for model 1"
  }
}

#' Print Bayes Factor Comparison Results
#'
#' @param x A bayes_factor_comparison object
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns the input object
#' @export
print.bayes_factor_comparison <- function(x, ...) {
  cat("
=== Bayes Factor Comparison ===
")
  cat("Models compared:", x$models_compared, "
")
  cat("Method used:", x$method_used, "

")

  cat("Marginal Likelihoods:
")
  print(x$marginal_likelihoods, row.names = FALSE)
  cat("
")

  if (!is.null(x$bayes_factor)) {
    cat("Bayes Factor (Model 1 vs Model 2):
")
    cat(" BF =", round(x$bayes_factor, 4), "
")
    cat(" log(BF) =", round(x$log_bf, 4), "
")
    cat(" Interpretation:", x$interpretation, "

")
  }

  if (!is.null(x$pairwise_comparisons)) {
    cat("Pairwise Comparisons:
")
    print(x$pairwise_comparisons, row.names = FALSE)
    cat("
")
  }

  invisible(x)
}

#' Plot Bayes Factor Comparison Results
#'
#' @param x A bayes_factor_comparison object
#' @param ... Additional arguments passed to ggplot2 functions
#' @return A ggplot2 plot object
#' @export
plot.bayes_factor_comparison <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Please install it.")
  }

  ml <- x$marginal_likelihoods

  if (x$models_compared == 2) {
    bf <- x$bayes_factor
    log_bf <- x$log_bf
    se1 <- ml$SE[1]
    se2 <- ml$SE[2]

    if (!is.na(se1) && !is.na(se2)) {
      se_logbf <- sqrt(se1^2 + se2^2)
      ci_log <- log_bf + c(-1, 1) * 1.96 * se_logbf
      ci <- exp(ci_log)

      df <- data.frame(
        Model = paste0(ml$Model[1], " vs ", ml$Model[2]),
        BF = bf,
        lower = ci[1],
        upper = ci[2]
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = BF)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2) +
        ggplot2::labs(title = "Bayes Factor (Model 1 vs Model 2)",
                      subtitle = sprintf("BF = %.3g (95%% CI: %.3g -- %.3g)", bf, df$lower, df$upper),
                      y = "Bayes Factor") +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip()

    } else {
      df <- data.frame(Model = paste0(ml$Model[1], " vs ", ml$Model[2]), BF = bf)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = BF)) +
        ggplot2::geom_col() +
        ggplot2::labs(title = "Bayes Factor (Model 1 vs Model 2)", y = "Bayes Factor") +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip()
    }

    print(p)
    invisible(p)

  } else {
    if (is.null(x$pairwise_comparisons)) {
      stop("Pairwise comparisons not available for >2 models.")
    }

    df <- x$pairwise_comparisons
    df$logBF <- log(df$BF_AB)

    p1 <- ggplot2::ggplot(df, ggplot2::aes(x = interaction(Model_A, Model_B, sep = " vs "), y = logBF)) +
      ggplot2::geom_col() +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(title = "Pairwise log-Bayes Factors (positive = evidence for A over B)",
                    x = "Comparison (A vs B)", y = "log(BF)") +
      ggplot2::theme_minimal() +
      ggplot2::coord_flip()

    try({
      mat <- matrix(NA_real_, nrow = length(x$model_names), ncol = length(x$model_names),
                    dimnames = list(x$model_names, x$model_names))

      for (i in seq_len(nrow(df))) {
        a <- df$Model_A[i]; b <- df$Model_B[i]; v <- df$Log_BF_AB[i]
        mat[a, b] <- v
        mat[b, a] <- -v
      }

      heat <- as.data.frame(as.table(mat))
      names(heat) <- c("Model_A", "Model_B", "LogBF")

      p2 <- ggplot2::ggplot(heat, ggplot2::aes(x = Model_A, y = Model_B, fill = LogBF)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", na.value = "grey90") +
        ggplot2::labs(title = "Pairwise log-BF heatmap", x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      print(p1)
      print(p2)
      invisible(list(bar = p1, heat = p2))
    }, silent = TRUE)

    if (!exists("p2")) {
      print(p1)
      invisible(p1)
    }
  }
}

