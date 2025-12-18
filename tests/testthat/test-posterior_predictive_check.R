test_that("posterior_predictive_check computes statistics", {
  skip_if_not_installed("brms")
  library(brms)

  # Fit simple model with enough iterations to avoid ESS warnings
  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- brm(y ~ x, data = test_data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  # Run PPC
  result <- posterior_predictive_check(
    model = fit,
    observed_data = test_data$y,
    n_samples = 100,
    test_statistics = "mean",
    plot = FALSE
  )

  # Verify output
  expect_s3_class(result, "posterior_predictive_check")
  expect_true(!is.null(result$p_values))
  expect_length(result$p_values, 1)
})

test_that("posterior_predictive_check handles multiple statistics", {
  skip_if_not_installed("brms")
  library(brms)

  test_data <- data.frame(y = rnorm(50), x = rnorm(50))
  # Increased iterations for stability
  fit <- brm(y ~ x, data = test_data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  result <- posterior_predictive_check(
    model = fit,
    observed_data = test_data$y,
    n_samples = 100,
    test_statistics = c("mean", "sd", "median"),
    plot = FALSE
  )

  expect_length(result$p_values, 3)
  expect_true(all(result$p_values >= 0 & result$p_values <= 1))
})
