library(testthat)
library(brms)
library(checkmate)

# Helper to fit a fast but convergent model for testing
fit_test_model <- function(formula, data) {
  brm(
    formula,
    data = data,
    chains = 2,      # Minimum chains for R-hat
    iter = 2000,     # Sufficient iterations for convergence
    warmup = 1000,
    refresh = 0,
    silent = 2,      # Suppress startup messages
    seed = 123       # reproducible
  )
}

test_that("prior_robustness validates inputs correctly", {
  skip_if_not_installed("brms")

  # Create simple test model
  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  # Test invalid model
  expect_error(
    prior_robustness(
      model = "not_a_model",
      prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
      parameters = "b_x"
    )
  )

  # Test missing prior specifications
  expect_error(
    prior_robustness(
      model = fit,
      prior_specifications = list(),
      parameters = "b_x"
    )
  )
})

test_that("prior_robustness computes robustness index", {
  skip_if_not_installed("brms")

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  result <- prior_robustness(
    model = fit,
    prior_specifications = list(
      weak = prior(normal(0, 10), class = "b"),
      strong = prior(normal(0, 0.5), class = "b")
    ),
    parameters = "b_x",
    perturbation_direction = "expand",
    dimensions = c(0.5, 1, 2),
    comparison_metric = "KL",
    plot = FALSE
  )

  # Check object class
  expect_s3_class(result, "prior_robustness")

  # Check robustness index exists and is in valid range
  expect_true(!is.null(result$robustness_index))
  expect_true(result$robustness_index >= 0 & result$robustness_index <= 1)
})

test_that("prior_robustness identifies concerning parameters", {
  skip_if_not_installed("brms")

  test_data <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  fit <- fit_test_model(y ~ x1 + x2, test_data)

  result <- prior_robustness(
    model = fit,
    prior_specifications = list(
      weak = prior(normal(0, 10), class = "b"),
      strong = prior(normal(0, 0.5), class = "b")
    ),
    parameters = c("b_x1", "b_x2"),
    plot = FALSE
  )

  # Check concerning parameters is a character vector
  expect_type(result$concerning_parameters, "character")
})

test_that("prior_robustness generates recommendations", {
  skip_if_not_installed("brms")

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  result <- prior_robustness(
    model = fit,
    prior_specifications = list(
      p1 = prior(normal(0, 5), class = "b")
    ),
    parameters = "b_x",
    plot = FALSE
  )

  # Check recommendations exist and are character
  expect_true(!is.null(result$recommendations))
  expect_type(result$recommendations, "character")
  expect_true(length(result$recommendations) > 0)
})

test_that("prior_robustness handles different metrics", {
  skip_if_not_installed("brms")

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  # Test KL metric
  result_kl <- prior_robustness(
    model = fit,
    prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
    parameters = "b_x",
    comparison_metric = "KL",
    plot = FALSE
  )
  expect_equal(result_kl$comparison_metric, "KL")

  # Test Wasserstein metric
  result_ws <- prior_robustness(
    model = fit,
    prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
    parameters = "b_x",
    comparison_metric = "Wasserstein",
    plot = FALSE
  )
  expect_equal(result_ws$comparison_metric, "Wasserstein")
})

test_that("prior_robustness respects credible_level parameter", {
  skip_if_not_installed("brms")

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  result <- prior_robustness(
    model = fit,
    prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
    parameters = "b_x",
    credible_level = 0.90,
    plot = FALSE
  )

  expect_s3_class(result, "prior_robustness")
  expect_true(!is.null(result$sensitivity_surfaces))
})

test_that("print.prior_robustness works without error", {
  skip_if_not_installed("brms")

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  result <- prior_robustness(
    model = fit,
    prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
    parameters = "b_x",
    plot = FALSE
  )

  # Capture output and check it contains key information
  expect_output(print(result), "Prior Robustness Analysis")
})

test_that("prior_robustness handles multiple parameters", {
  skip_if_not_installed("brms")

  test_data <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  fit <- fit_test_model(y ~ x1 + x2, test_data)

  result <- prior_robustness(
    model = fit,
    prior_specifications = list(
      p1 = prior(normal(0, 1), class = "b"),
      p2 = prior(normal(0, 5), class = "b")
    ),
    parameters = c("b_x1", "b_x2"),
    plot = FALSE
  )

  # Should have multiple sensitivity surfaces
  expect_true(length(result$sensitivity_surfaces) >= 2)
})

test_that("prior_robustness handles plot parameter", {
  skip_if_not_installed("brms")

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- fit_test_model(y ~ x, test_data)

  # With plot = FALSE
  result_no_plot <- prior_robustness(
    model = fit,
    prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
    parameters = "b_x",
    plot = FALSE
  )
  expect_null(result_no_plot$plots)

  # With plot = TRUE
  result_with_plot <- prior_robustness(
    model = fit,
    prior_specifications = list(p1 = prior(normal(0, 1), class = "b")),
    parameters = "b_x",
    plot = TRUE
  )
  expect_s3_class(result_with_plot, "prior_robustness")
})
