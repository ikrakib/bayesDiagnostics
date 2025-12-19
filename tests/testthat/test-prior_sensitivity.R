library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

test_that("prior_sensitivity: works with list input", {
  fixture <- get_simple_fixture()

  # FIX: Check what parameters actually exist in the model
  # The fixture model uses y ~ x, so the parameter is just the intercept and slope
  # Not "b_x" - check the actual posterior

  priors <- list(
    p1 = brms::set_prior("normal(0, 5)", class = "b"),
    p2 = brms::set_prior("normal(0, 10)", class = "b")
  )

  # Get actual parameter names from fixture
  posterior_names <- names(as.data.frame(posterior::as_draws_df(fixture$fit)))
  actual_param <- posterior_names[grepl("^b_", posterior_names)][1]

  if (is.na(actual_param)) {
    skip("No b_ parameters in fixture model")
  }

  result <- suppressWarnings(
    bayesDiagnostics::prior_sensitivity(
      model = fixture$fit,
      parameters = actual_param,  # Use actual parameter from model
      prior_grid = priors,
      comparison_metric = "KL"
    )
  )

  expect_true(is.list(result))
})

test_that("prior_sensitivity: handles single parameter", {
  fixture <- get_simple_fixture()

  priors <- list(
    narrow = brms::set_prior("normal(0, 3)", class = "b"),
    wide = brms::set_prior("normal(0, 20)", class = "b")
  )

  # Get actual parameter
  posterior_names <- names(as.data.frame(posterior::as_draws_df(fixture$fit)))
  actual_param <- posterior_names[grepl("^b_", posterior_names)][1]

  if (is.na(actual_param)) {
    skip("No b_ parameters in fixture model")
  }

  result <- suppressWarnings(
    bayesDiagnostics::prior_sensitivity(
      model = fixture$fit,
      parameters = actual_param,
      prior_grid = priors,
      comparison_metric = "KL"
    )
  )

  expect_true(is.list(result))
})

test_that("prior_sensitivity: returns result", {
  fixture <- get_simple_fixture()

  priors <- list(
    p1 = brms::set_prior("normal(0, 5)", class = "b"),
    p2 = brms::set_prior("normal(0, 10)", class = "b")
  )

  # Get actual parameter
  posterior_names <- names(as.data.frame(posterior::as_draws_df(fixture$fit)))
  actual_param <- posterior_names[grepl("^b_", posterior_names)][1]

  if (is.na(actual_param)) {
    skip("No b_ parameters in fixture model")
  }

  result <- suppressWarnings(
    bayesDiagnostics::prior_sensitivity(
      model = fixture$fit,
      parameters = actual_param,
      prior_grid = priors,
      comparison_metric = "KL"
    )
  )

  expect_true(!is.null(result) || is.list(result))
})
