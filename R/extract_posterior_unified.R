#' Extract Posterior Draws (Unified Interface)
#'
#' Provides a unified interface for extracting posterior draws from multiple
#' Bayesian modeling packages (brms, rstanarm, cmdstanr, etc.).
#'
#' @param model A fitted Bayesian model object
#' @param parameters Character vector of parameter names to extract (default: all)
#' @param format Output format: "draws_df", "draws_matrix", "draws_array", or "list"
#' @param n_draws Number of draws to extract (default: all available)
#' @param include_warmup Logical. Include warmup/burn-in draws (default: FALSE)
#' @param chains Numeric vector of chain IDs to extract (default: all chains)
#' @param ... Additional arguments for specific model types
#'
#' @return Posterior draws in the requested format:
#' \itemize{
#'   \item \strong{draws_df}: Data frame with one row per draw
#'   \item \strong{draws_matrix}: Matrix with draws in rows, parameters in columns
#'   \item \strong{draws_array}: 3D array (iterations x chains x parameters)
#'   \item \strong{list}: Named list of parameter vectors
#' }
#'
#' @details
#' This function provides a consistent interface across different Bayesian
#' modeling packages, handling their different internal formats automatically.
#'
#' Supported model types:
#' \itemize{
#'   \item \code{brmsfit} (brms)
#'   \item \code{stanfit} (rstan)
#'   \item \code{stanreg} (rstanarm)
#'   \item \code{CmdStanMCMC} (cmdstanr)
#'   \item \code{mcmc.list} (coda)
#' }
#'
#' @examples
#' \donttest{
#' library(brms)
#' fit <- brm(mpg ~ hp + wt, data = mtcars)
#'
#' # Extract as data frame
#' draws_df <- extract_posterior_unified(fit, format = "draws_df")
#'
#' # Extract specific parameters
#' slopes <- extract_posterior_unified(
#'   fit,
#'   parameters = c("b_hp", "b_wt"),
#'   format = "draws_matrix"
#' )
#'
#' # Extract first 1000 draws from chain 1
#' subset_draws <- extract_posterior_unified(
#'   fit,
#'   n_draws = 1000,
#'   chains = 1
#' )
#' }
#'
#' @export
extract_posterior_unified <- function(model,
                                      parameters = NULL,
                                      format = c("draws_df", "draws_matrix",
                                                 "draws_array", "list"),
                                      n_draws = NULL,
                                      include_warmup = FALSE,
                                      chains = NULL,
                                      ...) {

  # Input validation
  format <- match.arg(format)
  checkmate::assert_character(parameters, null.ok = TRUE)
  checkmate::assert_int(n_draws, lower = 1, null.ok = TRUE)
  checkmate::assert_logical(include_warmup)
  checkmate::assert_integerish(chains, lower = 1, null.ok = TRUE)

  # Detect model type and extract draws
  model_class <- class(model)[1]

  draws <- switch(
    model_class,
    "brmsfit" = extract_from_brms(model, parameters, include_warmup, chains),
    "stanfit" = extract_from_stanfit(model, parameters, include_warmup, chains),
    "stanreg" = extract_from_stanreg(model, parameters, include_warmup, chains),
    "CmdStanMCMC" = extract_from_cmdstan(model, parameters, include_warmup, chains),
    "mcmc.list" = extract_from_mcmc_list(model, parameters, chains),
    stop("Unsupported model class: ", model_class)
  )

  # Subset to requested number of draws
  if (!is.null(n_draws)) {
    draws <- subset_draws(draws, n_draws)
  }

  # Convert to requested format
  draws <- convert_format(draws, format)

  return(draws)
}

# ============ EXTRACTION FUNCTIONS ============

#' Extract from brmsfit
#' @keywords internal
extract_from_brms <- function(model, parameters, include_warmup, chains) {
  draws <- posterior::as_draws_df(model, inc_warmup = include_warmup)

  if (!is.null(chains)) {
    draws <- draws[draws$.chain %in% chains, ]
  }

  if (!is.null(parameters)) {
    # Keep parameters + metadata columns
    meta_cols <- c(".chain", ".iteration", ".draw")
    keep_cols <- c(meta_cols[meta_cols %in% names(draws)], parameters)
    draws <- draws[, keep_cols, drop = FALSE]
  }

  return(draws)
}

#' Extract from stanfit
#' @keywords internal
extract_from_stanfit <- function(model, parameters, include_warmup, chains) {
  draws <- posterior::as_draws_df(model, inc_warmup = include_warmup)

  if (!is.null(chains)) {
    draws <- draws[draws$.chain %in% chains, ]
  }

  if (!is.null(parameters)) {
    meta_cols <- c(".chain", ".iteration", ".draw")
    keep_cols <- c(meta_cols[meta_cols %in% names(draws)], parameters)
    draws <- draws[, keep_cols, drop = FALSE]
  }

  return(draws)
}

#' Extract from stanreg
#' @keywords internal
extract_from_stanreg <- function(model, parameters, include_warmup, chains) {
  # rstanarm models
  draws <- posterior::as_draws_df(model$stanfit, inc_warmup = include_warmup)

  if (!is.null(chains)) {
    draws <- draws[draws$.chain %in% chains, ]
  }

  if (!is.null(parameters)) {
    meta_cols <- c(".chain", ".iteration", ".draw")
    keep_cols <- c(meta_cols[meta_cols %in% names(draws)], parameters)
    draws <- draws[, keep_cols, drop = FALSE]
  }

  return(draws)
}

#' Extract from CmdStanMCMC
#' @keywords internal
extract_from_cmdstan <- function(model, parameters, include_warmup, chains) {
  draws <- model$draws(inc_warmup = include_warmup, format = "draws_df")

  if (!is.null(chains)) {
    draws <- draws[draws$.chain %in% chains, ]
  }

  if (!is.null(parameters)) {
    meta_cols <- c(".chain", ".iteration", ".draw")
    keep_cols <- c(meta_cols[meta_cols %in% names(draws)], parameters)
    draws <- draws[, keep_cols, drop = FALSE]
  }

  return(draws)
}

#' Extract from mcmc.list
#' @keywords internal
extract_from_mcmc_list <- function(model, parameters, chains) {
  # Convert coda mcmc.list to draws_df
  draws <- posterior::as_draws_df(model)

  if (!is.null(chains)) {
    draws <- draws[draws$.chain %in% chains, ]
  }

  if (!is.null(parameters)) {
    meta_cols <- c(".chain", ".iteration", ".draw")
    keep_cols <- c(meta_cols[meta_cols %in% names(draws)], parameters)
    draws <- draws[, keep_cols, drop = FALSE]
  }

  return(draws)
}

# ============ UTILITY FUNCTIONS ============

#' Subset Draws
#' @keywords internal
subset_draws <- function(draws, n_draws) {
  if (inherits(draws, "draws_df")) {
    n_available <- nrow(draws)
    if (n_draws < n_available) {
      draws <- draws[1:n_draws, ]
    }
  }
  return(draws)
}

#' Convert Format
#' @keywords internal
convert_format <- function(draws, format) {
  if (format == "draws_df") {
    return(posterior::as_draws_df(draws))
  } else if (format == "draws_matrix") {
    return(posterior::as_draws_matrix(draws))
  } else if (format == "draws_array") {
    return(posterior::as_draws_array(draws))
  } else if (format == "list") {
    df <- posterior::as_draws_df(draws)
    # Remove metadata columns - convert to regular df first to avoid warning
    meta_cols <- c(".chain", ".iteration", ".draw")
    df <- as.data.frame(df)  # Convert to regular data frame
    df <- df[, !names(df) %in% meta_cols, drop = FALSE]
    return(as.list(df))
  }

  return(draws)
}

# Global variables
utils::globalVariables(c(".chain", ".iteration", ".draw"))
