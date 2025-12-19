# ---- Mock objects ----

mock_brmsfit <- structure(
  list(
    formula = y ~ x,
    data = data.frame(y = rnorm(10), x = rnorm(10)),
    draws = matrix(rnorm(100), ncol = 10)
  ),
  class = "brmsfit"
)

mock_stanreg <- structure(
  list(
    coefficients = rnorm(5),
    vcov = diag(5)
  ),
  class = "stanreg"
)

mock_loo <- structure(
  list(
    estimates = data.frame(Estimate = rnorm(3))
  ),
  class = "loo"
)

mock_draws <- posterior::as_draws_df(
  data.frame(a = rnorm(100), b = rnorm(100))
)
