library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

test_that("predictive_performance: basic functionality", {
  fixture <- get_simple_fixture()

  result <- suppressWarnings(
    bayesDiagnostics::predictive_performance(
      model = fixture$fit,
      observed_y = fixture$data$y
    )
  )

  expect_true(!is.null(result))
})

test_that("predictive_performance: returns valid output", {
  fixture <- get_simple_fixture()

  result <- suppressWarnings(
    bayesDiagnostics::predictive_performance(
      model = fixture$fit,
      observed_y = fixture$data$y
    )
  )

  expect_true(is.list(result) || is.numeric(result))
})

test_that("predictive_performance: works with fixture", {
  fixture <- get_simple_fixture()

  result <- suppressWarnings(
    bayesDiagnostics::predictive_performance(
      model = fixture$fit,
      observed_y = fixture$data$y
    )
  )

  expect_true(!is.null(result))
})
