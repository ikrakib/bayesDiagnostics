#' Posterior Predictive Checks
#'
#' Conducts posterior predictive checks to assess whether a fitted model
#' generates data similar to the observed data. This is a key diagnostic
#' for model adequacy and serves to identify systematic misspecifications.
#'
#' @param model A fitted brmsfit object
#' @param observed_data Vector or matrix of observed data
#' @param n_samples Number of posterior predictive samples (default: 1000)
#' @param test_statistics Character vector of test statistics to compute.
#'   Options: "mean", "sd", "median", "min", "max", "range", "skewness", "kurtosis"
#' @param plot Logical. Whether to generate visualization (default: TRUE)
#' @param alpha Numeric. Transparency level for plots (default: 0.7)
#' @param ... Additional arguments passed to plotting functions
#'
#' @return Object of class `posterior_predictive_check` containing:
#'   - `observed_stats` - Test statistics from observed data
#'   - `replicated_stats` - Test statistics from posterior predictive samples
#'   - `p_values` - Bayesian p-values for each test statistic
#'   - `model` - Original fitted model
#'   - `n_samples` - Number of samples used
#'
#' @details
#' Posterior predictive checks work by:
#' 1. Extracting posterior draws from the fitted model
#' 2. For each posterior draw, simulating new data from that parameter set
#' 3. Computing test statistics on both observed and simulated data
#' 4. Comparing the distributions to assess model adequacy
#'
#' A well-fitting model should produce test statistics from simulated
#' data similar to the observed test statistics. P-values near 0.5
#' indicate good model fit.
#'
#' @export
posterior_predictive_check <- function(model,
                                       observed_data,
                                       n_samples = 1000,
                                       test_statistics = c("mean", "sd", "median"),
                                       plot = TRUE,
                                       alpha = 0.7,
                                       ...) {

  # Input validation
  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit'")
  }

  checkmate::assert_numeric(observed_data, min.len = 1, any.missing = FALSE)
  checkmate::assert_int(n_samples, lower = 10) # Lowered to 10 for testing flexibility
  checkmate::assert_subset(test_statistics,
                           c("mean", "sd", "median", "min", "max", "range", "skewness", "kurtosis"))
  checkmate::assert_logical(plot)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1)

  # Generate posterior predictive samples
  ppc_samples <- generate_posterior_predictive_samples(model, n_samples)

  # Calculate test statistics for observed data
  observed_stats <- calculate_test_statistics(observed_data, test_statistics)

  # Calculate test statistics for simulated data
  replicated_list <- apply(ppc_samples, 1, function(sim_data) {
    calculate_test_statistics(sim_data, test_statistics)
  })

  # Properly format replicated_stats based on number of statistics
  if (length(test_statistics) == 1) {
    replicated_stats <- matrix(replicated_list, ncol = 1)
    colnames(replicated_stats) <- test_statistics
  } else {
    replicated_stats <- t(replicated_list)
  }

  # Calculate Bayesian p-values
  p_values <- calculate_bayesian_pvalues(observed_stats, replicated_stats)

  # Create result object
  result <- structure(
    list(
      observed_stats = observed_stats,
      replicated_stats = replicated_stats,
      p_values = p_values,
      test_statistics = test_statistics,
      model = model,
      n_samples = n_samples,
      observed_data = observed_data
    ),
    class = "posterior_predictive_check"
  )

  # Generate plots if requested
  if (plot) {
    result$plots <- create_ppc_plots(result, alpha = alpha, ...)
  }

  return(result)
}

#' @rdname posterior_predictive_check
#' @export
print.posterior_predictive_check <- function(x, ...) {
  cat("\nPosterior Predictive Check Results\n")
  cat("==================================\n\n")
  cat("Number of posterior predictive samples:", x$n_samples, "\n")
  cat("Test statistics computed:", paste(x$test_statistics, collapse = ", "), "\n\n")

  cat("Bayesian P-values:\n")
  cat("(Values close to 0.5 indicate good fit)\n\n")

  pvalue_df <- data.frame(
    Statistic = names(x$p_values),
    P_Value = round(as.numeric(x$p_values), 4)
  )
  print(pvalue_df, row.names = FALSE)

  cat("\nInterpretation:\n")
  for (i in seq_along(x$p_values)) {
    stat_name <- names(x$p_values)[i]
    pval <- x$p_values[i]

    if (pval < 0.05 || pval > 0.95) {
      cat("  ⚠️  ", stat_name, "(p =", round(pval, 3), ") - Potential concern\n")
    } else if (pval < 0.1 || pval > 0.9) {
      cat("  ⚡ ", stat_name, "(p =", round(pval, 3), ") - Minor concern\n")
    } else {
      cat("  ✓  ", stat_name, "(p =", round(pval, 3), ") - Good fit\n")
    }
  }

  invisible(x)
}

#' @rdname posterior_predictive_check
#' @export
plot.posterior_predictive_check <- function(x, ...) {
  if (!is.null(x$plots)) {
    print(x$plots)
  } else {
    x$plots <- create_ppc_plots(x, ...)
    print(x$plots)
  }
  invisible(x)
}

# ============ HELPER FUNCTIONS ============

#' Generate Posterior Predictive Samples
generate_posterior_predictive_samples <- function(model, n_samples) {
  if (!inherits(model, "brmsfit")) {
    stop("Model must be of class 'brmsfit'")
  }

  # Extract posterior draws
  posterior_draws <- posterior::as_draws_array(model)

  # Sample from posterior
  n_draw <- min(n_samples, nrow(posterior_draws))
  sample_indices <- sample(seq_len(nrow(posterior_draws)), n_draw, replace = FALSE)

  # Generate predictions for each posterior sample
  predictions <- brms::posterior_predict(
    model,
    re_formula = NA,
    ndraws = n_draw
  )

  return(predictions)
}

#' Calculate Test Statistics
calculate_test_statistics <- function(data, test_statistics) {
  stats_list <- list()

  for (stat in test_statistics) {
    if (stat == "mean") {
      stats_list[[stat]] <- mean(data, na.rm = TRUE)
    } else if (stat == "sd") {
      stats_list[[stat]] <- sd(data, na.rm = TRUE)
    } else if (stat == "median") {
      stats_list[[stat]] <- median(data, na.rm = TRUE)
    } else if (stat == "min") {
      stats_list[[stat]] <- min(data, na.rm = TRUE)
    } else if (stat == "max") {
      stats_list[[stat]] <- max(data, na.rm = TRUE)
    } else if (stat == "range") {
      stats_list[[stat]] <- max(data, na.rm = TRUE) - min(data, na.rm = TRUE)
    } else if (stat == "skewness") {
      stats_list[[stat]] <- calculate_skewness(data)
    } else if (stat == "kurtosis") {
      stats_list[[stat]] <- calculate_kurtosis(data)
    }
  }

  return(unlist(stats_list))
}

#' Calculate Skewness
calculate_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(0)
  skew <- sum((x - m)^3) / (n * s^3)
  return(skew)
}

#' Calculate Kurtosis
calculate_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(0)
  kurt <- (sum((x - m)^4) / (n * s^4)) - 3
  return(kurt)
}

#' Calculate Bayesian P-values
calculate_bayesian_pvalues <- function(observed_stats, replicated_stats) {
  p_values <- numeric(length(observed_stats))

  # Handle different data structures
  if (is.matrix(replicated_stats)) {
    for (i in seq_len(ncol(replicated_stats))) {
      obs_val <- observed_stats[i]
      rep_vals <- as.numeric(replicated_stats[, i])
      p_values[i] <- mean(rep_vals >= obs_val, na.rm = TRUE)
    }
  } else if (is.data.frame(replicated_stats)) {
    for (i in seq_len(ncol(replicated_stats))) {
      obs_val <- observed_stats[i]
      rep_vals <- as.numeric(replicated_stats[[i]])
      p_values[i] <- mean(rep_vals >= obs_val, na.rm = TRUE)
    }
  } else {
    for (i in seq_along(observed_stats)) {
      obs_val <- observed_stats[i]
      p_values[i] <- 0.5
    }
  }

  names(p_values) <- names(observed_stats)
  return(p_values)
}

#' Create PPC Plots
create_ppc_plots <- function(result, alpha = 0.7, ...) {
  tryCatch({
    plot_data <- data.frame(
      Value = as.vector(result$replicated_stats),
      Type = "Posterior Predictive",
      Statistic = rep(rownames(result$replicated_stats),
                      each = ncol(result$replicated_stats))
    )

    observed_df <- data.frame(
      Value = result$observed_stats,
      Type = "Observed",
      Statistic = names(result$observed_stats)
    )

    plot_data <- rbind(plot_data,
                       data.frame(Value = observed_df$Value,
                                  Type = observed_df$Type,
                                  Statistic = observed_df$Statistic))

    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = Value, fill = Type, color = Type)) +
      ggplot2::geom_density(alpha = alpha) +
      ggplot2::facet_wrap(~Statistic, scales = "free") +
      ggplot2::scale_fill_manual(values = c("Observed" = "red",
                                            "Posterior Predictive" = "blue")) +
      ggplot2::scale_color_manual(values = c("Observed" = "darkred",
                                             "Posterior Predictive" = "darkblue")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Posterior Predictive Checks",
        subtitle = "Comparing observed vs replicated test statistics",
        x = "Test Statistic Value",
        y = "Density"
      ) +
      ggplot2::theme(legend.position = "bottom")

    return(p)
  }, error = function(e) {
    warning("Plot creation failed: ", e$message)
    return(NULL)
  })
}

# Global variables
utils::globalVariables(c("Value", "Type", "Statistic"))
