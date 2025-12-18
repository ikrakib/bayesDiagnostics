library(testthat)
library(bayesDiagnostics)
library(brms)
library(ggplot2)

test_that("FIXTURE: Model setup and all tests", {

  # Generate test data
  set.seed(123)
  test_data <- data.frame(
    y = rnorm(100, 2 * 1:100, 1),
    x = 1:100
  )

  # Create model with proper convergence settings
  sensitivity_test_model <- brm(
    y ~ x,
    data = test_data,
    chains = 2,
    iter = 2000,
    warmup = 1000,
    refresh = 0,
    silent = 2,
    seed = 123
  )

  test_that("basic structure and validation", {
    alt_priors <- list(
      weak = brms::set_prior("normal(0, 10)", class = "b"),
      strong = brms::set_prior("normal(0, 0.1)", class = "b")
    )

    result <- prior_sensitivity(
      model = sensitivity_test_model,
      parameters = "b_x",
      prior_grid = alt_priors,
      n_draws = 100,
      plot = FALSE
    )

    expect_s3_class(result, "prior_sensitivity")
    expect_named(result, c(
      "sensitivity_metrics", "posteriors", "comparison_metric",
      "parameters", "model"
    ))
    expect_s3_class(result$sensitivity_metrics, "data.frame")
    expect_type(result$posteriors, "list")
    expect_equal(result$comparison_metric, "KL")
    expect_equal(result$parameters, "b_x")
  })

  test_that("handles multiple parameters", {
    alt_priors <- list(
      alt1 = brms::set_prior("normal(0, 5)", class = "b")
    )

    params <- c("b_Intercept", "b_x")

    result <- prior_sensitivity(
      model = sensitivity_test_model,
      parameters = params,
      prior_grid = alt_priors,
      n_draws = 100,
      plot = FALSE
    )

    expect_equal(result$parameters, params)
    expect_equal(
      nrow(result$sensitivity_metrics),
      length(alt_priors) * length(params)
    )
  })

  test_that("supports different metrics", {
    alt_priors <- list(
      test = brms::set_prior("normal(0, 5)", class = "b")
    )

    res_w <- prior_sensitivity(
      model = sensitivity_test_model,
      parameters = "b_x",
      prior_grid = alt_priors,
      comparison_metric = "Wasserstein",
      n_draws = 100,
      plot = FALSE
    )

    expect_equal(res_w$comparison_metric, "Wasserstein")
    expect_true(is.numeric(res_w$sensitivity_metrics$metric_value))

    res_o <- prior_sensitivity(
      model = sensitivity_test_model,
      parameters = "b_x",
      prior_grid = alt_priors,
      comparison_metric = "overlap",
      n_draws = 100,
      plot = FALSE
    )

    expect_equal(res_o$comparison_metric, "overlap")
    expect_true(all(res_o$sensitivity_metrics$metric_value >= 0))
    expect_true(all(res_o$sensitivity_metrics$metric_value <= 1))
  })

  test_that("plot generation works", {
    alt_priors <- list(
      test = brms::set_prior("normal(0, 5)", class = "b")
    )

    result <- prior_sensitivity(
      model = sensitivity_test_model,
      parameters = "b_x",
      prior_grid = alt_priors,
      plot = TRUE,
      n_draws = 100
    )

    expect_true("plots" %in% names(result))
    expect_s3_class(result$plots, "ggplot")
    expect_error(plot(result), NA)
  })

  test_that("input validation catches errors", {
    alt_priors <- list(
      test = brms::set_prior("normal(0, 5)", class = "b")
    )

    expect_error(
      prior_sensitivity(
        model = lm(y ~ x, data = test_data),
        parameters = "b_x",
        prior_grid = alt_priors
      ),
      "Model must be a brmsfit or stanfit object"
    )

    expect_error(
      prior_sensitivity(
        model = sensitivity_test_model,
        parameters = "b_x",
        prior_grid = alt_priors,
        comparison_metric = "invalid_metric"
      ),
      "Must be element of set"
    )

    expect_error(
      prior_sensitivity(
        model = sensitivity_test_model,
        parameters = "b_x",
        prior_grid = list()
      ),
      "Must have length >= 1"
    )
  })

  test_that("print method output", {
    alt_priors <- list(
      test = brms::set_prior("normal(0, 5)", class = "b")
    )

    result <- prior_sensitivity(
      model = sensitivity_test_model,
      parameters = "b_x",
      prior_grid = alt_priors,
      plot = FALSE,
      n_draws = 100
    )

    expect_output(print(result), "Prior Sensitivity Analysis")
    expect_output(print(result), "Parameters analyzed")
    expect_output(print(result), "Sensitivity Metrics")
  })

})
