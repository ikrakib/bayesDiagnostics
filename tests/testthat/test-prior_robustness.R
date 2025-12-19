library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

test_that("prior_robustness: analyzes with priors list", {
  fixture <- get_simple_fixture()

  alt_priors <- list(
    robust = brms::set_prior("normal(0, 10)", class = "b")
  )

  # Get actual parameter from model
  posterior_names <- names(as.data.frame(posterior::as_draws_df(fixture$fit)))
  actual_param <- posterior_names[grepl("^b_", posterior_names)][1]

  if (is.na(actual_param)) {
    skip("No b_ parameters in fixture model")
  }

  result <- suppressWarnings(
    bayesDiagnostics::prior_robustness(
      model = fixture$fit,
      prior_specifications = alt_priors,
      parameters = actual_param,  # Use actual parameter
      perturbation_direction = "expand",
      dimensions = 1,
      plot = FALSE
    )
  )

  expect_s3_class(result, "prior_robustness")
})

test_that("prior_robustness: returns valid results", {
  fixture <- get_simple_fixture()

  alt_priors <- list(
    p1 = brms::set_prior("normal(0, 5)", class = "b"),
    p2 = brms::set_prior("normal(0, 15)", class = "b")
  )

  posterior_names <- names(as.data.frame(posterior::as_draws_df(fixture$fit)))
  actual_param <- posterior_names[grepl("^b_", posterior_names)][1]

  if (is.na(actual_param)) {
    skip("No b_ parameters in fixture model")
  }

  result <- suppressWarnings(
    bayesDiagnostics::prior_robustness(
      model = fixture$fit,
      prior_specifications = alt_priors,
      parameters = actual_param,
      dimensions = 1,
      plot = FALSE
    )
  )

  expect_true(is.list(result))
})

test_that("prior_robustness: basic functionality", {
  fixture <- get_simple_fixture()

  alt_priors <- list(
    robust = brms::set_prior("normal(0, 10)", class = "b")
  )

  posterior_names <- names(as.data.frame(posterior::as_draws_df(fixture$fit)))
  actual_param <- posterior_names[grepl("^b_", posterior_names)][1]

  if (is.na(actual_param)) {
    skip("No b_ parameters in fixture model")
  }

  result <- suppressWarnings(
    bayesDiagnostics::prior_robustness(
      model = fixture$fit,
      prior_specifications = alt_priors,
      parameters = actual_param,
      plot = FALSE
    )
  )

  expect_true(!is.null(result))
})
