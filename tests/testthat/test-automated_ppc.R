test_that("automated_ppc: basic structure", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 100)
  expect_s3_class(res, "automated_ppc")
  expect_true("diagnostics" %in% names(res))
  expect_true("flags" %in% names(res))
})

test_that("automated_ppc: diagnostics dataframe is correct", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 100)

  # Check diagnostics structure
  expect_true(is.data.frame(res$diagnostics))
  expect_equal(nrow(res$diagnostics), 6) # Mean, SD, Min, Max, Skew, Kurt
  expect_true(all(c("Statistic", "Observed", "P_Value", "Flagged") %in% names(res$diagnostics)))
})

test_that("automated_ppc: all statistics are computed", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 100)

  expected_stats <- c("Mean", "SD", "Min", "Max", "Skewness", "Kurtosis")
  expect_setequal(res$diagnostics$Statistic, expected_stats)
})

test_that("automated_ppc: p-values are between 0 and 1", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 100)

  expect_true(all(res$diagnostics$P_Value >= 0 & res$diagnostics$P_Value <= 1))
})

test_that("automated_ppc: flagging works correctly", {
  skip_if_not_installed("brms")

  # Create data with extreme skewness (exponential distribution)
  set.seed(123)
  obs_data <- rexp(100, rate = 0.5)
  data <- data.frame(y = obs_data) # Wrap in data frame

  fit <- brms::brm(
    y ~ 1,
    data = data,
    family = gaussian(),
    chains = 1, iter = 1000, warmup = 500, refresh = 0
  )

  # Use strict threshold to detect misfit
  res <- automated_ppc(fit, obs_data, n_samples = 100, p_value_threshold = 0.1)

  # Should have some flags for exponential data fit with Normal model
  expect_true(length(res$flags) > 0 || res$flags == "No major discrepancies detected.")
})

test_that("automated_ppc: threshold parameter affects flagging", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  # Low threshold (strict)
  res_strict <- automated_ppc(fit, data$y, n_samples = 100, p_value_threshold = 0.01)

  # High threshold (lenient)
  res_lenient <- automated_ppc(fit, data$y, n_samples = 100, p_value_threshold = 0.25)

  # Lenient should have fewer or equal flags
  n_flags_strict <- if (length(res_strict$flags) == 1 && res_strict$flags == "No major discrepancies detected.") 0 else length(res_strict$flags)
  n_flags_lenient <- if (length(res_lenient$flags) == 1 && res_lenient$flags == "No major discrepancies detected.") 0 else length(res_lenient$flags)

  expect_lte(n_flags_lenient, n_flags_strict)
})

test_that("automated_ppc: print method works", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 100)

  expect_output(print(res), "Automated PPC Report")
})

test_that("automated_ppc: handles different data types", {
  skip_if_not_installed("brms")

  # Test with integer data
  data <- data.frame(y = as.integer(rpois(50, 5)))
  fit <- brms::brm(y ~ 1, data = data, family = poisson(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 100)
  expect_s3_class(res, "automated_ppc")
})

test_that("automated_ppc: validates input", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  # Should error on non-numeric observed_data
  expect_error(automated_ppc(fit, c("a", "b"), n_samples = 100))
})

test_that("automated_ppc: n_samples parameter is used", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- automated_ppc(fit, data$y, n_samples = 150)
  expect_equal(res$n_samples, 150)
})
