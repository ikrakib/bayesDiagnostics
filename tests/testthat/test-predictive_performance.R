test_that("predictive_performance: basic structure", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "rmse")

  expect_s3_class(perf, "predictive_performance")
  expect_true(!is.null(perf$point_metrics))
  expect_equal(perf$sample_size, 50)
})

test_that("predictive_performance: requires observed_y", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  expect_error(predictive_performance(model))
})

test_that("predictive_performance: validates observed_y length", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  wrong_y <- rnorm(30)
  expect_error(predictive_performance(model, observed_y = wrong_y))
})

test_that("predictive_performance: RMSE metric works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "rmse")

  expect_true("RMSE" %in% perf$point_metrics$Metric)
  expect_true(perf$point_metrics$Value[1] > 0)
})

test_that("predictive_performance: MAE metric works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "mae")

  expect_true("MAE" %in% perf$point_metrics$Metric)
  expect_true(perf$point_metrics$Value[2] > 0)
})

test_that("predictive_performance: coverage metric works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "coverage",
                                 credible_level = 0.95)

  expect_true("Coverage" %in% perf$interval_metrics$Metric)
  coverage <- perf$interval_metrics$Value[1]
  expect_true(coverage >= 0 && coverage <= 1)
})

test_that("predictive_performance: coverage near credible level", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(100), x = rnorm(100))
  model <- brms::brm(y ~ x, data = data, chains = 2, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "coverage",
                                 credible_level = 0.90)

  coverage <- perf$interval_metrics$Value[1]
  # Coverage should be reasonably close to credible level
  expect_true(coverage > 0.70 && coverage < 1.0)
})

test_that("predictive_performance: CRPS metric works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "crps")

  expect_true("CRPS" %in% perf$proper_scores$Metric)
  expect_true(perf$proper_scores$Value > 0)
})

test_that("predictive_performance: all metrics works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "all")

  expect_equal(length(perf$metrics_requested), 4)
  expect_true(!is.null(perf$point_metrics))
  expect_true(!is.null(perf$interval_metrics))
  expect_true(!is.null(perf$proper_scores))
})

test_that("predictive_performance: prediction_summary has correct structure", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "coverage")

  expect_true("Observation" %in% names(perf$prediction_summary))
  expect_true("Observed" %in% names(perf$prediction_summary))
  expect_true("Predicted_Mean" %in% names(perf$prediction_summary))
  expect_true("Predicted_SD" %in% names(perf$prediction_summary))
  expect_equal(nrow(perf$prediction_summary), 50)
})

test_that("predictive_performance: newdata parameter works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  newdata <- data.frame(x = rnorm(30))
  new_y <- rnorm(30)

  perf <- predictive_performance(model, newdata = newdata, observed_y = new_y,
                                 metrics = "rmse")

  expect_equal(perf$sample_size, 30)
})

test_that("predictive_performance: print method works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "all")

  expect_output(print(perf), "Predictive Performance")
})

test_that("predictive_performance: plot method works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "coverage")

  expect_output(capture.output(plot(perf)), NA) # Should not error
})

test_that("predictive_performance: n_draws parameter works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf1 <- predictive_performance(model, observed_y = data$y, metrics = "rmse", n_draws = 100)
  perf2 <- predictive_performance(model, observed_y = data$y, metrics = "rmse", n_draws = NULL)

  # Both should compute successfully
  expect_true(!is.null(perf1$point_metrics))
  expect_true(!is.null(perf2$point_metrics))
})

test_that("predictive_performance: credible_level parameter works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "coverage",
                                 credible_level = 0.80)

  expect_equal(perf$credible_level, 0.80)
  expect_true("Lower_CI" %in% names(perf$prediction_summary))
  expect_true("Upper_CI" %in% names(perf$prediction_summary))
})

test_that("predictive_performance: correlation computed correctly", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "rmse")

  expect_true("Correlation" %in% perf$point_metrics$Metric)
  corr_val <- perf$point_metrics$Value[3]
  expect_true(corr_val >= -1 && corr_val <= 1)
})

test_that("predictive_performance: RMSE always positive", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "rmse")

  rmse_val <- perf$point_metrics$Value[1]
  expect_true(rmse_val > 0)
})

test_that("predictive_performance: MAE <= RMSE", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "all")

  mae_val <- perf$point_metrics$Value[2]
  rmse_val <- perf$point_metrics$Value[1]

  # MAE should always be <= RMSE due to Jensen's inequality
  expect_true(mae_val <= rmse_val + 1e-10)
})

test_that("predictive_performance: invalid credible_level rejected", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  expect_error(predictive_performance(model, observed_y = data$y,
                                      credible_level = 1.5))
  expect_error(predictive_performance(model, observed_y = data$y,
                                      credible_level = -0.1))
})

test_that("predictive_performance: model_formula stored", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "rmse")

  expect_true(!is.null(perf$model_formula))
})

test_that("predictive_performance: handles intercept-only model", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50))
  model <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  perf <- predictive_performance(model, observed_y = data$y, metrics = "all")

  expect_equal(perf$sample_size, 50)
  expect_true(!is.null(perf$point_metrics))
})
