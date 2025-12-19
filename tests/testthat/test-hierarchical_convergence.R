library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Note: setup-global-fixtures.R doesn't have hierarchical model yet
# So we keep local fixture for this one
.fixture_env <- new.env(parent = emptyenv())

message("Setting up hierarchical test model...")
set.seed(808)
.fixture_env$data <- data.frame(y = rnorm(30), group = rep(1:3, each = 10), x = rnorm(30))

tryCatch({
  .fixture_env$fit <- brms::brm(
    y ~ x + (1 | group),
    data = .fixture_env$data,
    chains = 1,
    iter = 200,
    warmup = 100,
    refresh = 0,
    silent = 2,
    seed = 808
  )
  .fixture_env$fit_success <- TRUE
}, error = function(e) {
  .fixture_env$fit_success <- FALSE
  .fixture_env$fit_error <- e$message
})

get_fixture <- function() {
  if (!isTRUE(.fixture_env$fit_success)) {
    skip(paste("Model fitting failed:", .fixture_env$fit_error))
  }
  .fixture_env
}

test_that("hierarchical_convergence: checks hierarchical model convergence", {
  fixture <- get_fixture()
  result <- bayesDiagnostics::hierarchical_convergence(model = fixture$fit, group_vars = "group", plot = FALSE)
  expect_s3_class(result, "hierarchical_convergence")
  expect_true(!is.null(result$population_diagnostics))
})

test_that("hierarchical_convergence: contains group diagnostics", {
  fixture <- get_fixture()
  result <- bayesDiagnostics::hierarchical_convergence(model = fixture$fit, group_vars = "group", plot = FALSE)
  expect_true(!is.null(result$group_diagnostics))
})
