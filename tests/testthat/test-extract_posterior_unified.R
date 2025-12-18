library(testthat)
library(bayesDiagnostics)
library(brms)
library(posterior)

# Create test fixture
if (!exists("extract_test_model")) {
  set.seed(321)
  test_data <- data.frame(
    y = rnorm(30),
    x = rnorm(30)
  )

  extract_test_model <- brm(
    y ~ x,
    data = test_data,
    chains = 2,
    iter = 1000,  # Increased
    warmup = 500, # Increased
    refresh = 0,
    silent = 2,
    seed = 321
  )
}

test_that("extract_posterior_unified: basic extraction works", {
  draws <- extract_posterior_unified(
    model = extract_test_model,
    format = "draws_df"
  )

  # Should return draws_df
  expect_s3_class(draws, "draws_df")

  # Should have draws (rows)
  expect_true(nrow(draws) > 0)

  # Should have parameters (columns)
  expect_true(ncol(draws) > 0)
})

test_that("extract_posterior_unified: extracts specific parameters", {
  draws <- extract_posterior_unified(
    model = extract_test_model,
    parameters = c("b_Intercept", "b_x"),
    format = "draws_df"
  )

  # Should have requested parameters
  expect_true("b_Intercept" %in% names(draws))
  expect_true("b_x" %in% names(draws))

  # Should also have metadata columns
  expect_true(".chain" %in% names(draws) || ".draw" %in% names(draws))
})

test_that("extract_posterior_unified: format conversion works", {
  # Test draws_df
  df <- extract_posterior_unified(extract_test_model, format = "draws_df")
  expect_s3_class(df, "draws_df")

  # Test draws_matrix
  mat <- extract_posterior_unified(extract_test_model, format = "draws_matrix")
  expect_s3_class(mat, "draws_matrix")
  expect_true(is.matrix(mat))

  # Test draws_array
  arr <- extract_posterior_unified(extract_test_model, format = "draws_array")
  expect_s3_class(arr, "draws_array")
  expect_true(is.array(arr))
  expect_equal(length(dim(arr)), 3)  # iterations x chains x parameters

  # Test list
  lst <- extract_posterior_unified(extract_test_model, format = "list")
  expect_type(lst, "list")
  expect_true(length(lst) > 0)
})

test_that("extract_posterior_unified: n_draws subsetting", {
  # Extract only first 50 draws
  draws <- extract_posterior_unified(
    model = extract_test_model,
    n_draws = 50,
    format = "draws_df"
  )

  # Should have 50 rows
  expect_equal(nrow(draws), 50)
})

test_that("extract_posterior_unified: chain selection", {
  # Extract only chain 1
  draws <- extract_posterior_unified(
    model = extract_test_model,
    chains = 1,
    format = "draws_df"
  )

  # All draws should be from chain 1
  expect_true(all(draws$.chain == 1))

  # Extract chain 2
  draws2 <- extract_posterior_unified(
    model = extract_test_model,
    chains = 2,
    format = "draws_df"
  )

  expect_true(all(draws2$.chain == 2))
})

test_that("extract_posterior_unified: include_warmup parameter", {
  # By default, warmup should not be included
  draws_no_warmup <- extract_posterior_unified(
    model = extract_test_model,
    include_warmup = FALSE,
    format = "draws_df"
  )

  # With warmup
  draws_with_warmup <- extract_posterior_unified(
    model = extract_test_model,
    include_warmup = TRUE,
    format = "draws_df"
  )

  # With warmup should have more draws
  expect_true(nrow(draws_with_warmup) >= nrow(draws_no_warmup))
})

test_that("extract_posterior_unified: handles different model types", {
  # Test with brmsfit (already tested above)
  expect_error(
    extract_posterior_unified(extract_test_model, format = "draws_df"),
    NA
  )

  # Test with unsupported model
  lm_model <- lm(y ~ x, data = test_data)
  expect_error(
    extract_posterior_unified(lm_model),
    "Unsupported model class"
  )
})

test_that("extract_posterior_unified: input validation", {
  # Invalid format
  expect_error(
    extract_posterior_unified(extract_test_model, format = "invalid"),
    "'arg' should be one of"
  )

  # Invalid n_draws
  expect_error(
    extract_posterior_unified(extract_test_model, n_draws = -10),
    ">= 1"
  )

  # Invalid chains
  expect_error(
    extract_posterior_unified(extract_test_model, chains = 0),
    ">= 1"
  )
})

test_that("extract_posterior_unified: preserves parameter values", {
  # Extract in different formats
  df <- extract_posterior_unified(extract_test_model,
                                  parameters = "b_Intercept",
                                  format = "draws_df")
  mat <- extract_posterior_unified(extract_test_model,
                                   parameters = "b_Intercept",
                                   format = "draws_matrix")

  # Values should be the same (accounting for format differences)
  expect_equal(df$b_Intercept, as.vector(mat[, "b_Intercept"]))
})

test_that("extract_posterior_unified: handles empty parameter list", {
  # Should extract all parameters when parameters = NULL
  all_params <- extract_posterior_unified(
    model = extract_test_model,
    parameters = NULL,
    format = "draws_df"
  )

  # Should have multiple parameters
  expect_true(ncol(all_params) > 3)  # More than just metadata
})

test_that("extract_posterior_unified: list format excludes metadata", {
  lst <- extract_posterior_unified(
    model = extract_test_model,
    format = "list"
  )

  # Should not have metadata columns
  expect_false(".chain" %in% names(lst))
  expect_false(".iteration" %in% names(lst))
  expect_false(".draw" %in% names(lst))

  # Should have parameter columns
  expect_true("b_Intercept" %in% names(lst))
})

test_that("extract_posterior_unified: combined parameter and chain selection", {
  draws <- extract_posterior_unified(
    model = extract_test_model,
    parameters = "b_x",
    chains = 1,
    n_draws = 25,
    format = "draws_df"
  )

  # Should satisfy all conditions
  expect_equal(nrow(draws), 25)
  expect_true(all(draws$.chain == 1))
  expect_true("b_x" %in% names(draws))
})
