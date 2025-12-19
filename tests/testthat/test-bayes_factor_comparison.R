library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Use global fixtures
test_that("bayes_factor_comparison: compares two models", {
  fixture1 <- get_simple_fixture()
  fixture2 <- get_complex_fixture()
  result <- bayesDiagnostics::bayes_factor_comparison(fixture1$fit, fixture2$fit, method = "waic", silent = TRUE)
  expect_s3_class(result, "bayes_factor_comparison")
  expect_true(all(c("bayes_factor", "log_bf") %in% names(result)))
})

test_that("bayes_factor_comparison: print method works", {
  fixture1 <- get_simple_fixture()
  fixture2 <- get_complex_fixture()
  result <- bayesDiagnostics::bayes_factor_comparison(fixture1$fit, fixture2$fit, method = "waic", silent = TRUE)
  expect_output(print(result), "Bayes Factor")
})

test_that("bayes_factor_comparison: returns numeric bayes factor", {
  fixture1 <- get_simple_fixture()
  fixture2 <- get_complex_fixture()
  result <- bayesDiagnostics::bayes_factor_comparison(fixture1$fit, fixture2$fit, method = "waic", silent = TRUE)
  expect_true(is.numeric(result$bayes_factor))
})
