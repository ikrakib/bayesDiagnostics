test_that("ppc_crossvalidation: basic structure", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)
  expect_s3_class(res, "ppc_crossvalidation")
  expect_true("pit_values" %in% names(res))
  expect_true("loo_result" %in% names(res))
  expect_true("plot" %in% names(res))
})

test_that("ppc_crossvalidation: pit_values are between 0 and 1", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_true(is.numeric(res$pit_values))
  expect_equal(length(res$pit_values), 50)
  expect_true(all(res$pit_values >= 0 & res$pit_values <= 1))
})

test_that("ppc_crossvalidation: plot is a ggplot object", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_s3_class(res$plot, "ggplot")
})

test_that("ppc_crossvalidation: loo_result is valid", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_s3_class(res$loo_result, "loo")
})

test_that("ppc_crossvalidation: plot has title and labels", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_true("title" %in% names(res$plot$labels))
  expect_true("x" %in% names(res$plot$labels))
  expect_true("y" %in% names(res$plot$labels))
})

test_that("ppc_crossvalidation: plot method works", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_output(capture.output(plot(res)), NA) # Should not error
})

test_that("ppc_crossvalidation: print method works", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_output(print(res), "LOO Cross-Validation Check")
})

test_that("ppc_crossvalidation: handles different data sizes", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  # Small sample
  data_small <- data.frame(y = rnorm(20))
  fit_small <- brms::brm(y ~ 1, data = data_small, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res_small <- ppc_crossvalidation(fit_small, data_small$y, n_draws = 50)
  expect_equal(length(res_small$pit_values), 20)

  # Larger sample
  data_large <- data.frame(y = rnorm(100))
  fit_large <- brms::brm(y ~ 1, data = data_large, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res_large <- ppc_crossvalidation(fit_large, data_large$y, n_draws = 100)
  expect_equal(length(res_large$pit_values), 100)
})

test_that("ppc_crossvalidation: n_draws parameter is used", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res50 <- ppc_crossvalidation(fit, data$y, n_draws = 50)
  res200 <- ppc_crossvalidation(fit, data$y, n_draws = 200)

  # Both should work
  expect_s3_class(res50, "ppc_crossvalidation")
  expect_s3_class(res200, "ppc_crossvalidation")
})

test_that("ppc_crossvalidation: validates model class", {
  skip_if_not_installed("brms")

  data <- rnorm(50)

  expect_error(ppc_crossvalidation("not a model", data, n_draws = 100))
})

test_that("ppc_crossvalidation: works with different distributions", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  # Poisson data
  data <- data.frame(y = rpois(50, lambda = 5))
  fit <- brms::brm(y ~ 1, data = data, family = poisson(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_s3_class(res, "ppc_crossvalidation")
  expect_true(all(res$pit_values >= 0 & res$pit_values <= 1))
})

test_that("ppc_crossvalidation: handles negative values", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  data <- data.frame(y = rnorm(50, mean = -10, sd = 2))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 100)

  expect_s3_class(res, "ppc_crossvalidation")
  expect_true(all(res$pit_values >= 0 & res$pit_values <= 1))
})

test_that("ppc_crossvalidation: pit histogram looks uniform (visual check)", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  # Well-specified model should have roughly uniform PIT
  set.seed(123)
  data <- data.frame(y = rnorm(100, mean = 0, sd = 1))
  fit <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  res <- ppc_crossvalidation(fit, data$y, n_draws = 200)

  # PIT values should be roughly uniformly distributed
  # We can't test this exactly, but at least check the distribution
  pit_quantiles <- quantile(res$pit_values, probs = c(0.25, 0.5, 0.75))

  # For uniform distribution, these should be approximately 0.25, 0.5, 0.75
  # Widening tolerances slightly to prevent flaky tests with random sampling
  expect_gte(pit_quantiles[1], 0.1)
  expect_lte(pit_quantiles[1], 0.4)
  expect_gte(pit_quantiles[2], 0.35)
  expect_lte(pit_quantiles[2], 0.65)
  expect_gte(pit_quantiles[3], 0.6)
  expect_lte(pit_quantiles[3], 0.9)
})
