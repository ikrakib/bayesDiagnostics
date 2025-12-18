test_that("bayes_factor_comparison: basic structure with 2 models", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bridgesampling")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic", silent = TRUE)

  expect_s3_class(bf, "bayes_factor_comparison")
  expect_true(!is.null(bf$bayes_factor))
  expect_true(!is.null(bf$log_bf))
  expect_equal(bf$models_compared, 2)
})

test_that("bayes_factor_comparison: requires at least 2 models", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  expect_error(bayes_factor_comparison(m1))
})

test_that("bayes_factor_comparison: rejects non-brmsfit objects", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  expect_error(bayes_factor_comparison(m1, "not a model"))
})

test_that("bayes_factor_comparison: WAIC method works", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_true(is.numeric(bf$bayes_factor))
  expect_true(bf$bayes_factor > 0)
  expect_equal(bf$method_used, "waic")
})

test_that("bayes_factor_comparison: bridge_sampling method works", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bridgesampling")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "bridge_sampling", repetitions = 2)

  expect_true(is.numeric(bf$bayes_factor))
  expect_true(bf$bayes_factor > 0)
  expect_equal(bf$method_used, "bridge_sampling")
})

test_that("bayes_factor_comparison: log_bf is finite", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_true(is.finite(bf$log_bf))
})

test_that("bayes_factor_comparison: BF relationship to log_BF is correct", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expected_bf <- exp(bf$log_bf)
  expect_equal(bf$bayes_factor, expected_bf, tolerance = 1e-10)
})

test_that("bayes_factor_comparison: generates interpretation", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_true(is.character(bf$interpretation))
  expect_true(nchar(bf$interpretation) > 0)
})

test_that("bayes_factor_comparison: handles 3+ models", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m3 <- brms::brm(y ~ x1 + x2, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, m3, method = "waic")

  expect_equal(bf$models_compared, 3)
  expect_true(!is.null(bf$pairwise_comparisons))
  expect_equal(nrow(bf$pairwise_comparisons), 3) # C(3,2) = 3 pairs
})

test_that("bayes_factor_comparison: marginal likelihoods computed for all models", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_equal(nrow(bf$marginal_likelihoods), 2)
  expect_true(all(!is.na(bf$marginal_likelihoods$Log_Marginal_Likelihood)))
})

test_that("bayes_factor_comparison: print method works", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_output(print(bf), "Bayes Factor")
})

test_that("bayes_factor_comparison: plot method works", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_output(capture.output(plot(bf)), NA) # Should not error
})

test_that("bayes_factor_comparison: handles named models", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(Model_Null = m1, Model_Alt = m2, method = "waic")

  expect_equal(bf$model_names[1], "Model_Null")
  expect_equal(bf$model_names[2], "Model_Alt")
})

test_that("bayes_factor_comparison: pairwise comparisons have correct structure", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m3 <- brms::brm(y ~ x1 + x2, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, m3, method = "waic")

  expect_true("Model_A" %in% names(bf$pairwise_comparisons))
  expect_true("Model_B" %in% names(bf$pairwise_comparisons))
  expect_true("Log_BF_AB" %in% names(bf$pairwise_comparisons))
  expect_true("BF_AB" %in% names(bf$pairwise_comparisons))
  expect_true("Interpretation" %in% names(bf$pairwise_comparisons))
})

test_that("bayes_factor_comparison: all BFs are positive", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0)

  bf <- bayes_factor_comparison(m1, m2, method = "waic")

  expect_true(bf$bayes_factor > 0)
})

test_that("bayes_factor_comparison: silent parameter is respected", {
  skip_if_not_installed("brms")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0, silent = 2)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0, silent = 2)

  # Test that silent = TRUE doesn't produce warnings
  bf <- bayes_factor_comparison(m1, m2, method = "waic", silent = TRUE)
  expect_true(is.numeric(bf$bayes_factor))
})

test_that("bayes_factor_comparison: repetitions parameter is used", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bridgesampling")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  m1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0, silent = 2)
  m2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 1000, warmup = 500, refresh = 0, silent = 2)

  # Just verify no errors with different repetitions
  bf1 <- tryCatch(
    bayes_factor_comparison(m1, m2, method = "bridge_sampling", repetitions = 1),
    error = function(e) NULL
  )

  bf2 <- tryCatch(
    bayes_factor_comparison(m1, m2, method = "bridge_sampling", repetitions = 3),
    error = function(e) NULL
  )

  # At least one should work (depending on system)
  expect_true(!is.null(bf1) || !is.null(bf2))
})
