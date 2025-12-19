library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Use global fixture
test_that("mcmc_diagnostics_summary: returns correct class", {
  fixture <- get_simple_fixture()
  result <- bayesDiagnostics::mcmc_diagnostics_summary(fixture$fit)
  expect_s3_class(result, "mcmc_diagnostics")
  expect_true(is.list(result))
})

test_that("mcmc_diagnostics_summary: print method works", {
  fixture <- get_simple_fixture()
  result <- bayesDiagnostics::mcmc_diagnostics_summary(fixture$fit)
  expect_output(print(result), "Diagnostics")
})

test_that("mcmc_diagnostics_summary: contains diagnostic information", {
  fixture <- get_simple_fixture()
  result <- bayesDiagnostics::mcmc_diagnostics_summary(fixture$fit)
  expect_true(length(names(result)) > 0)
})

