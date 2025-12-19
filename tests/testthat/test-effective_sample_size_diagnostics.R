library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Use global fixture
test_that("effective_sample_size_diagnostics: returns correct class", {
  fixture <- get_simple_fixture()
  result <- bayesDiagnostics::effective_sample_size_diagnostics(model = fixture$fit, plot = FALSE)
  expect_s3_class(result, "ess_diagnostics")
  expect_true(is.list(result))
})

test_that("effective_sample_size_diagnostics: print method works", {
  fixture <- get_simple_fixture()
  result <- bayesDiagnostics::effective_sample_size_diagnostics(model = fixture$fit, plot = FALSE)
  expect_output(print(result), "Effective Sample Size")
})
