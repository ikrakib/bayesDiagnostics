# Test setup for brms models
# Suppress Stan compilation messages during tests
if (requireNamespace("rstan", quietly = TRUE)) {
  rstan::rstan_options(auto_write = TRUE)
}

options(
  mc.cores = 1,
  brms.backend = "rstan"
)
