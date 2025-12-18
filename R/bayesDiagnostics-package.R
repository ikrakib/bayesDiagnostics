#' bayesDiagnostics Package
#'
#' Provides comprehensive tools for Bayesian model diagnostics and comparison.
#'
#' @docType package
#' @name bayesDiagnostics
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats approx density median poisson rnorm sd update
#' @importFrom utils head capture.output
## usethis namespace: end
NULL

# Declare global variables for dplyr/ggplot2 NSE and brms priors
utils::globalVariables(c(
  # dplyr/ggplot2 variables
  "Model", "Delta_IC", "Model_Weight",
  "BF", "lower", "upper",
  "Model_A", "Model_B", "logBF", "LogBF",
  "PIT", ".", "type",
  # brms prior functions (from brms namespace, not base R)
  "normal", "lognormal"
))
