test_that("model_comparison_suite: basic structure with 2 models", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = FALSE)

  expect_s3_class(comp, "model_comparison")
  expect_true("comparison_table" %in% names(comp))
  expect_true("ic_differences" %in% names(comp))
  expect_true("model_names" %in% names(comp))
  expect_equal(nrow(comp$comparison_table), 2)
})

test_that("model_comparison_suite: requires at least 2 models", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  expect_error(model_comparison_suite(model1))
})

test_that("model_comparison_suite: rejects non-brmsfit objects", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  expect_error(model_comparison_suite(model1, "not a model"))
})

test_that("model_comparison_suite: LOO criterion works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = FALSE)

  expect_true("LOO" %in% comp$ic_differences$IC_Type)
  expect_true(all(!is.na(comp$comparison_table$LOO_ELPD)))
})

test_that("model_comparison_suite: WAIC criterion works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "waic", plot = FALSE)

  expect_true("WAIC" %in% comp$ic_differences$IC_Type)
  expect_true(all(!is.na(comp$comparison_table$WAIC)))
})

test_that("model_comparison_suite: bayes_r2 criterion works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "bayes_r2", plot = FALSE)

  expect_true(all(!is.na(comp$comparison_table$Bayes_R2)))
})

test_that("model_comparison_suite: all criteria works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "all", plot = FALSE)

  expect_equal(length(comp$criterion_used), 3)
  expect_true(all(c("loo", "waic", "bayes_r2") %in% comp$criterion_used))
})

test_that("model_comparison_suite: model weights sum to 1", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = FALSE)

  loo_weights <- comp$ic_differences[comp$ic_differences$IC_Type == "LOO", "Model_Weight"]
  expect_true(abs(sum(loo_weights) - 1.0) < 1e-10)
})

test_that("model_comparison_suite: generates plots when plot = TRUE", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "all", plot = TRUE)

  expect_true(!is.null(comp$plots))
  expect_true(length(comp$plots) > 0)
})

test_that("model_comparison_suite: plot method works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = TRUE)

  expect_output(capture.output(plot(comp)), NA)
})

test_that("model_comparison_suite: print method works", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = FALSE)

  expect_output(print(comp), "Model Comparison Results")
})

test_that("model_comparison_suite: handles named models", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(
    Intercept = model1,
    With_Predictor = model2,
    criterion = "loo",
    plot = FALSE
  )

  expect_equal(comp$model_names[1], "Intercept")
  expect_equal(comp$model_names[2], "With_Predictor")
})

test_that("model_comparison_suite: handles 3+ models", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model3 <- brms::brm(y ~ x1 + x2, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, model3, criterion = "loo", plot = FALSE)

  expect_equal(nrow(comp$comparison_table), 3)
  expect_equal(length(comp$model_names), 3)
})

test_that("model_comparison_suite: handles single criterion string", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "waic", plot = FALSE)

  expect_equal(comp$criterion_used, "waic")
})

test_that("model_comparison_suite: IC differences are non-negative", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = FALSE)

  expect_false(is.null(comp$ic_differences))
  ic_diffs <- comp$ic_differences$Delta_IC
  expect_true(all(ic_diffs >= 0, na.rm = TRUE))
})

test_that("model_comparison_suite: best model has minimum Delta IC", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp <- model_comparison_suite(model1, model2, criterion = "loo", plot = FALSE)

  expect_false(is.null(comp$ic_differences))
  ic_diffs <- comp$ic_differences$Delta_IC
  expect_equal(min(ic_diffs, na.rm = TRUE), 0, tolerance = 1e-10)
})

test_that("model_comparison_suite: detailed parameter is stored", {
  skip_if_not_installed("brms")
  set.seed(123)

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  model1 <- brms::brm(y ~ 1, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)
  model2 <- brms::brm(y ~ x, data = data, chains = 1, iter = 2000, warmup = 1000, refresh = 0)

  comp1 <- model_comparison_suite(model1, model2, detailed = TRUE, criterion = "loo", plot = FALSE)
  comp2 <- model_comparison_suite(model1, model2, detailed = FALSE, criterion = "loo", plot = FALSE)

  expect_true(comp1$detailed)
  expect_false(comp2$detailed)
})
