#' bayesDiagnostics: Bayesian Diagnostic Tools
#'
#' Comprehensive tools for diagnosing Bayesian models, including
#' posterior predictive checks, prior sensitivity analysis, and
#' MCMC diagnostics.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats sd median quantile density rnorm approx poisson update
#' @importFrom utils head
#' @importFrom dplyr all_of
#' @importFrom rlang .data
#' @importFrom brms prior
## usethis namespace: end
NULL

# Handle prior specification symbols that might look like missing globals
utils::globalVariables(c("normal", "lognormal", "beta", "student_t", "cauchy", "gamma", "type"))
