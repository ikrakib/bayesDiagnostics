library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Use global fixture
test_that("extract_posterior_unified: extracts as data frame", {
  fixture <- get_simple_fixture()
  draws <- bayesDiagnostics::extract_posterior_unified(fixture$fit, format = "draws_df")
  expect_true(is.data.frame(draws))
  expect_true(nrow(draws) > 0)
})

test_that("extract_posterior_unified: extracts as matrix", {
  fixture <- get_simple_fixture()
  draws <- bayesDiagnostics::extract_posterior_unified(fixture$fit, format = "draws_matrix")
  expect_true(is.matrix(draws))
  expect_true(nrow(draws) > 0)
})

test_that("extract_posterior_unified: correct number of draws", {
  fixture <- get_simple_fixture()
  draws <- bayesDiagnostics::extract_posterior_unified(fixture$fit, format = "draws_df")
  expect_equal(nrow(draws), 100)
})
