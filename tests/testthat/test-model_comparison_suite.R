library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("loo")

# Use global fixtures
test_that("model_comparison_suite: compares models with LOO", {
  fixture1 <- get_simple_fixture()
  fixture2 <- get_complex_fixture()
  comp <- bayesDiagnostics::model_comparison_suite(fixture1$fit, fixture2$fit, criterion = "loo", plot = FALSE)
  expect_s3_class(comp, "model_comparison")
  expect_true(is.data.frame(comp$comparison_table))
})

test_that("model_comparison_suite: print method works", {
  fixture1 <- get_simple_fixture()
  fixture2 <- get_complex_fixture()
  comp <- bayesDiagnostics::model_comparison_suite(fixture1$fit, fixture2$fit, criterion = "loo", plot = FALSE)
  expect_output(print(comp), "Model")
})
