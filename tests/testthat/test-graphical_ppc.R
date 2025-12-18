test_that("graphical_ppc: density plot returns ggplot", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(30))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "density", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: intervals plot returns ggplot", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(20))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "intervals", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: ribbon plot returns ggplot", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(30))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "ribbon", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: all plot types work", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(25))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  plot_types <- c("density", "intervals", "ribbon")
  for (type in plot_types) {
    p <- graphical_ppc(fit, data$y, type = type, n_draws = 30)
    expect_s3_class(p, "ggplot")
  }
})

test_that("graphical_ppc: density plot has correct title", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(30))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "density", n_draws = 50)
  expect_true("title" %in% names(p$labels))
})

test_that("graphical_ppc: handles small sample size", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(10))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "intervals", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: handles large sample size", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(500))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "density", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: n_draws parameter affects computation", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(30))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p1 <- graphical_ppc(fit, data$y, type = "density", n_draws = 30)
  p2 <- graphical_ppc(fit, data$y, type = "density", n_draws = 80)

  # Both should be valid ggplots
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("graphical_ppc: ribbon plot sorts data", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  # Create unsorted data
  data <- data.frame(y = c(5, 1, 3, 2, 4))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "ribbon", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: validates model class", {
  skip_if_not_installed("brms")

  data <- rnorm(30)

  expect_error(graphical_ppc("not a model", data, type = "density"))
})

test_that("graphical_ppc: validates plot type", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(30))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  expect_error(graphical_ppc(fit, data$y, type = "invalid_type"))
})

test_that("graphical_ppc: works with different distributions", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  # Poisson data
  data <- data.frame(y = rpois(50, lambda = 5))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "density", n_draws = 50)
  expect_s3_class(p, "ggplot")
})

test_that("graphical_ppc: handles negative values", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayesplot")

  data <- data.frame(y = rnorm(30, mean = -5, sd = 2))
  fit <- brms::brm(y ~ 1, data = data, family = gaussian(), chains = 1, iter = 1000, warmup = 500, refresh = 0)

  p <- graphical_ppc(fit, data$y, type = "density", n_draws = 50)
  expect_s3_class(p, "ggplot")
})
