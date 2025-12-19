library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Use global fixture from setup-global-fixtures.R
test_that("automated_ppc: returns correct class", {
  fixture <- get_simple_fixture()
  res <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50)
  expect_s3_class(res, "automated_ppc")
  expect_true("diagnostics" %in% names(res))
  expect_true("flags" %in% names(res))
  expect_equal(res$n_samples, 50)
})

test_that("automated_ppc: diagnostics structure is correct", {
  fixture <- get_simple_fixture()
  res <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50)
  expect_true(is.data.frame(res$diagnostics))
  expect_equal(nrow(res$diagnostics), 6)
  expect_true(all(c("Statistic", "Observed", "P_Value", "Flagged") %in% colnames(res$diagnostics)))
})

test_that("automated_ppc: p-values valid", {
  fixture <- get_simple_fixture()
  res <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50)
  expect_true(all(res$diagnostics$P_Value >= 0 & res$diagnostics$P_Value <= 1))
})

test_that("automated_ppc: print method works", {
  fixture <- get_simple_fixture()
  res <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50)
  expect_output(print(res), "Automated PPC Report")
})

test_that("automated_ppc: works with different thresholds", {
  fixture <- get_simple_fixture()
  res1 <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50, p_value_threshold = 0.05)
  res2 <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50, p_value_threshold = 0.01)
  expect_s3_class(res1, "automated_ppc")
  expect_s3_class(res2, "automated_ppc")
})

test_that("automated_ppc: works with different sample sizes", {
  fixture <- get_simple_fixture()
  res_small <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 20)
  res_large <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 100)
  expect_equal(res_small$n_samples, 20)
  expect_equal(res_large$n_samples, 100)
})

test_that("automated_ppc: flags are correct type", {
  fixture <- get_simple_fixture()
  res <- bayesDiagnostics::automated_ppc(fixture$fit, fixture$data$y, n_samples = 50)
  expect_true(is.character(res$flags))
  expect_true(length(res$flags) > 0)
})
