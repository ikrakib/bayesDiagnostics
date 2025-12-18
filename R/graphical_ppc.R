#' Graphical Posterior Predictive Checks
#'
#' Generates professional visualization of PPCs, including ribbon plots
#' for uncertainty intervals and comparison densities.
#'
#' @param model A fitted brmsfit object.
#' @param observed_data Numeric vector.
#' @param type Character. "density" (default), "intervals", or "ribbon".
#' @param n_draws Integer.
#'
#' @return A ggplot2 object.
#' @export
graphical_ppc <- function(model, observed_data, type = "density", n_draws = 50) {
  checkmate::assert_class(model, "brmsfit")
  checkmate::assert_choice(type, c("density", "intervals", "ribbon"))

  yrep <- brms::posterior_predict(model, ndraws = n_draws)

  if (type == "density") {
    # Overlay density of yrep vs y
    p <- bayesplot::ppc_dens_overlay(observed_data, yrep) +
      ggplot2::labs(title = "PPC: Density Comparison")

  } else if (type == "intervals") {
    # Intervals for each data point (good for small N)
    p <- bayesplot::ppc_intervals(observed_data, yrep) +
      ggplot2::labs(title = "PPC: Predictive Intervals per Point")

  } else if (type == "ribbon") {
    # Ribbon plot (often for time-series or sorted data)
    # We sort the data to make the ribbon meaningful if x isn't provided
    idx <- order(observed_data)
    y_sorted <- observed_data[idx]
    yrep_sorted <- yrep[, idx]

    p <- bayesplot::ppc_ribbon(y_sorted, yrep_sorted) +
      ggplot2::labs(title = "PPC: Ribbon Plot (Sorted by Y)")
  }

  return(p)
}
