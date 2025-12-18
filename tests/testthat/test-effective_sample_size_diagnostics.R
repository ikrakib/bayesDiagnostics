library(testthat)
library(bayesDiagnostics)
library(brms)

# Create test fixture
if (!exists("ess_test_model")) {
  set.seed(999)
  test_data <- data.frame(
    y = rnorm(50),
    x = rnorm(50)
  )

  ess_test_model <- brm(
    y ~ x,
    data = test_data,
    chains = 2,
    iter = 1000,
    warmup = 500,
    refresh = 0,
    silent = 2,
    seed = 999
  )
}

test_that("effective_sample_size_diagnostics: basic structure", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    plot = FALSE
  )

  # Check class
  expect_s3_class(result, "ess_diagnostics")

  # Check structure
  expect_named(result, c("ess_summary", "bulk_ess", "tail_ess",
                         "by_chain_ess", "problematic_params",
                         "recommendations", "model", "min_ess"))

  # Check data types
  expect_s3_class(result$ess_summary, "data.frame")
  expect_s3_class(result$bulk_ess, "data.frame")
  expect_s3_class(result$tail_ess, "data.frame")
})

test_that("effective_sample_size_diagnostics: analyzes all parameters by default", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    parameters = NULL,
    plot = FALSE
  )

  # Should analyze multiple parameters
  expect_true(nrow(result$bulk_ess) > 2)

  # Should include fixed effects
  params <- result$bulk_ess$variable
  expect_true(any(grepl("b_", params)))
})

test_that("effective_sample_size_diagnostics: respects parameter selection", {
  # Analyze only specific parameter
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    parameters = c("b_Intercept", "b_x"),
    plot = FALSE
  )

  # Should only have selected parameters
  expect_equal(nrow(result$bulk_ess), 2)
  expect_true(all(result$bulk_ess$variable %in% c("b_Intercept", "b_x")))
})

test_that("effective_sample_size_diagnostics: bulk and tail ESS", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    plot = FALSE
  )

  # Both should have same parameters
  expect_equal(nrow(result$bulk_ess), nrow(result$tail_ess))
  expect_equal(result$bulk_ess$variable, result$tail_ess$variable)

  # ESS values should be positive
  expect_true(all(result$bulk_ess$ess_bulk > 0))
  expect_true(all(result$tail_ess$ess_tail > 0))
})

test_that("effective_sample_size_diagnostics: per-chain analysis", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    by_chain = TRUE,
    plot = FALSE
  )

  # Should have by_chain_ess
  expect_false(is.null(result$by_chain_ess))

  # Should have multiple chains
  if (!is.null(result$by_chain_ess)) {
    expect_true("chain" %in% names(result$by_chain_ess))
    expect_true(length(unique(result$by_chain_ess$chain)) >= 2)
  }
})

test_that("effective_sample_size_diagnostics: identifies problematic params", {
  # Use very high threshold to force some parameters to be problematic
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    min_ess = 10000,  # Very high threshold
    plot = FALSE
  )

  # Should identify some as problematic
  expect_true(nrow(result$problematic_params) > 0)

  # Problematic params should have issue description
  expect_true("issue" %in% names(result$problematic_params))
})

test_that("effective_sample_size_diagnostics: generates recommendations", {
  # Use high threshold to trigger recommendations
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    min_ess = 5000,
    plot = FALSE
  )

  # If there are problematic params, should have recommendations
  if (nrow(result$problematic_params) > 0) {
    expect_true(length(result$recommendations) > 0)
    expect_type(result$recommendations, "character")
  }
})

test_that("effective_sample_size_diagnostics: ESS summary statistics", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    plot = FALSE
  )

  summ <- result$ess_summary

  # Should have bulk and tail rows
  expect_true("Bulk ESS" %in% summ$metric)
  expect_true("Tail ESS" %in% summ$metric)

  # Should have summary stats
  expect_true(all(c("min", "median", "max", "n_below_threshold") %in% names(summ)))

  # Min should be less than median, median less than max
  bulk_row <- summ[summ$metric == "Bulk ESS", ]
  expect_true(bulk_row$min <= bulk_row$median)
  expect_true(bulk_row$median <= bulk_row$max)
})

test_that("effective_sample_size_diagnostics: plot generation", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    plot = TRUE
  )

  expect_true("plots" %in% names(result))

  # Test plot method
  expect_error(plot(result), NA)
})

test_that("effective_sample_size_diagnostics: print method", {
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    plot = FALSE
  )

  expect_output(print(result), "Effective Sample Size Diagnostics")
  expect_output(print(result), "Total parameters analyzed")
  expect_output(print(result), "ESS Summary")
})

test_that("effective_sample_size_diagnostics: input validation", {
  # Invalid model
  expect_error(
    effective_sample_size_diagnostics(model = lm(y ~ x, data = test_data)),
    "Must inherit from class"
  )

  # Invalid min_ess
  expect_error(
    effective_sample_size_diagnostics(model = ess_test_model, min_ess = 50),
    ">= 100"
  )

  # Invalid tail_quantiles
  expect_error(
    effective_sample_size_diagnostics(model = ess_test_model, tail_quantiles = c(0.1)),
    "Must have length 2"
  )
})

test_that("effective_sample_size_diagnostics: handles low ESS model", {
  set.seed(888)
  low_ess_model <- withCallingHandlers(
    brm(
      y ~ x,
      data = test_data,
      chains = 1,
      iter = 100,
      warmup = 50,
      refresh = 0,
      silent = 2,
      seed = 888
    ),
    warning = function(w) {
      if (grepl("Effective Samples Size.*too low", w$message)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  result <- withCallingHandlers(
    effective_sample_size_diagnostics(
      model = low_ess_model,
      min_ess = 400,
      plot = FALSE
    ),
    warning = function(w) {
      if (grepl("ESS has been capped", w$message)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_true(nrow(result$problematic_params) > 0)

  expect_true(length(result$recommendations) > 0)
})

test_that("effective_sample_size_diagnostics: passes with good ESS", {
  # Normal model should have adequate ESS
  result <- effective_sample_size_diagnostics(
    model = ess_test_model,
    min_ess = 100,  # Low threshold
    plot = FALSE
  )

  # Should have few or no problematic parameters
  expect_true(nrow(result$problematic_params) == 0 ||
                nrow(result$problematic_params) < nrow(result$bulk_ess) * 0.5)
})
