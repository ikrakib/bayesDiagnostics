library(testthat)
library(bayesDiagnostics)
library(brms)
library(lme4)

# Create test fixture: hierarchical model
if (!exists("hier_test_model")) {
  set.seed(456)
  data("sleepstudy", package = "lme4")

  hier_test_model <- brm(
    Reaction ~ Days + (Days | Subject),
    data = sleepstudy,
    chains = 2,
    iter = 2000,
    warmup = 1000,
    refresh = 0,
    silent = 2,
    seed = 456,
    control = list(
      max_treedepth = 12,
      adapt_delta = 0.95    # Better sampling
    )
  )
}

test_that("hierarchical_convergence: basic structure", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    plot = FALSE
  )

  # Check class
  expect_s3_class(result, "hierarchical_convergence")

  # Check structure
  expect_named(result, c("population_diagnostics", "group_diagnostics",
                         "shrinkage_metrics", "convergence_summary",
                         "warnings", "model", "group_vars"))

  # Check data types
  expect_s3_class(result$population_diagnostics, "data.frame")
  expect_s3_class(result$group_diagnostics, "data.frame")
  expect_s3_class(result$convergence_summary, "data.frame")
  expect_type(result$warnings, "character")
})

test_that("hierarchical_convergence: auto-detects group variables", {
  # Don't provide group_vars explicitly
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = NULL,
    plot = FALSE
  )

  # Should auto-detect "Subject"
  expect_true("Subject" %in% result$group_vars)
})

test_that("hierarchical_convergence: identifies population vs group params", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    plot = FALSE
  )

  # Population params should include fixed effects
  pop_vars <- result$population_diagnostics$variable
  expect_true(any(grepl("^b_", pop_vars)))

  # Group params should include random effects
  group_vars <- result$group_diagnostics$variable
  expect_true(any(grepl("Subject", group_vars)))
})

test_that("hierarchical_convergence: checks convergence thresholds", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    rhat_threshold = 1.01,
    ess_threshold = 400,
    plot = FALSE
  )

  # Check that convergence flags exist
  expect_true("rhat_ok" %in% names(result$population_diagnostics))
  expect_true("ess_bulk_ok" %in% names(result$population_diagnostics))
  expect_true("converged" %in% names(result$population_diagnostics))

  # All flags should be logical
  expect_type(result$population_diagnostics$rhat_ok, "logical")
  expect_type(result$population_diagnostics$ess_bulk_ok, "logical")
})

test_that("hierarchical_convergence: shrinkage metrics", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    check_shrinkage = TRUE,
    plot = FALSE
  )

  # Shrinkage metrics should exist
  expect_false(is.null(result$shrinkage_metrics))

  # Should have group column
  if (!is.null(result$shrinkage_metrics)) {
    expect_true("group" %in% names(result$shrinkage_metrics))
  }
})

test_that("hierarchical_convergence: convergence summary", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    plot = FALSE
  )

  summ <- result$convergence_summary

  # Should have both levels
  expect_true("Population" %in% summ$level)
  expect_true("Group" %in% summ$level)

  # Should have counts and percentages
  expect_true(all(c("n_params", "n_converged", "pct_converged") %in% names(summ)))

  # Percentages should be 0-100
  expect_true(all(summ$pct_converged >= 0 & summ$pct_converged <= 100))
})

test_that("hierarchical_convergence: generates warnings correctly", {
  # Create a model with potential convergence issues
  # (by using very few iterations)
  set.seed(789)
  bad_model <- brm(
    Reaction ~ Days + (Days | Subject),
    data = sleepstudy,
    chains = 1,
    iter = 1200,
    warmup = 600,
    refresh = 0,
    silent = 2,
    seed = 789,
    control = list(
      max_treedepth = 12,
      adapt_delta = 0.95
    )
  )

  result <- hierarchical_convergence(
    model = bad_model,
    group_vars = "Subject",
    rhat_threshold = 1.01,
    ess_threshold = 400,
    plot = FALSE
  )

  # Should generate warnings due to low ESS
  expect_true(length(result$warnings) > 0)
})

test_that("hierarchical_convergence: plot generation", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    plot = TRUE
  )

  # Plot should be created
  expect_true("plots" %in% names(result))

  # Test plot method
  expect_error(plot(result), NA)
})

test_that("hierarchical_convergence: print method", {
  result <- hierarchical_convergence(
    model = hier_test_model,
    group_vars = "Subject",
    plot = FALSE
  )

  expect_output(print(result), "Hierarchical Model Convergence Diagnostics")
  expect_output(print(result), "Population-level parameters")
  expect_output(print(result), "Group-level parameters")
})

test_that("hierarchical_convergence: input validation", {
  # Invalid model class
  expect_error(
    hierarchical_convergence(model = lm(Reaction ~ Days, data = sleepstudy)),
    "Model must be of class 'brmsfit' or 'stanfit'"
  )

  # Invalid rhat_threshold
  expect_error(
    hierarchical_convergence(model = hier_test_model, rhat_threshold = 0.9),
    ">= 1"
  )

  # Invalid ess_threshold
  expect_error(
    hierarchical_convergence(model = hier_test_model, ess_threshold = 50),
    ">= 100"
  )
})

test_that("hierarchical_convergence: handles models with no random effects", {
  # Fit model without random effects
  simple_model <- brm(
    Reaction ~ Days,
    data = sleepstudy,
    chains = 1,
    iter = 1000,
    warmup = 500,
    refresh = 0,
    silent = 2
  )

  result <- hierarchical_convergence(
    model = simple_model,
    group_vars = NULL,
    plot = FALSE
  )

  # Should still work, but group diagnostics should be empty
  expect_equal(nrow(result$group_diagnostics), 0)
  expect_true(nrow(result$population_diagnostics) > 0)
})
