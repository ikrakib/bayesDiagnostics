library(brms)

#Global fixtures - used by ALL tests
.global_fixtures <- new.env(parent = emptyenv())

message("Building global fixtures...")
options(warn = -1) # Suppress ESS warnings

#============================================================================
  #SIMPLE MODEL (for ~10 tests)
#============================================================================
  set.seed(123)
.global_fixtures$data_simple <- data.frame(y = rnorm(25))

tryCatch({
  .global_fixtures$fit_simple <- brms::brm(
    y ~ 1,
    data = .global_fixtures$data_simple,
    chains = 1,
    iter = 200,
    warmup = 100,
    refresh = 0,
    silent = 2,
    seed = 123
  )
  .global_fixtures$fit_simple_ok <- TRUE
}, error = function(e) {
  .global_fixtures$fit_simple_ok <- FALSE
  .global_fixtures$fit_simple_error <- e$message
})

#============================================================================
  #COMPLEX MODEL (for ~5 tests)
#============================================================================
  set.seed(456)
.global_fixtures$data_complex <- data.frame(y = rnorm(25), x = rnorm(25))

tryCatch({
  .global_fixtures$fit_complex <- brms::brm(
    y ~ x,
    data = .global_fixtures$data_complex,
    chains = 1,
    iter = 200,
    warmup = 100,
    refresh = 0,
    silent = 2,
    seed = 456
  )
  .global_fixtures$fit_complex_ok <- TRUE
}, error = function(e) {
  .global_fixtures$fit_complex_ok <- FALSE
  .global_fixtures$fit_complex_error <- e$message
})

options(warn = 0)
message("Global fixtures ready!")

#============================================================================
  #HELPER FUNCTIONS (accessible to all test files)
#============================================================================
  get_simple_fixture <- function() {
    if (!isTRUE(.global_fixtures$fit_simple_ok)) {
      skip(paste("Model fitting failed:", .global_fixtures$fit_simple_error))
    }
    list(
      fit = .global_fixtures$fit_simple,
      data = .global_fixtures$data_simple
    )
  }

get_complex_fixture <- function() {
  if (!isTRUE(.global_fixtures$fit_complex_ok)) {
    skip(paste("Model fitting failed:", .global_fixtures$fit_complex_error))
  }
  list(
    fit = .global_fixtures$fit_complex,
    data = .global_fixtures$data_complex
  )
}
