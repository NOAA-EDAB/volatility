#' Perform Monte Carlo simulations
#'
#' @description
#' Simulation study
#'
#'
#' @export

run_simulations <- function(
  n,
  nsims,
  window_size,
  sigma_vec,
  lambda = FALSE,
  homo = TRUE,
  trend = c("none", "linear", "sinusoid", "both"),
  detrend = c("none", "linear", "diff", "rolling")
) {
  detrend <- match.arg(detrend)
  trend <- match.arg(trend)
  metrics <- array(NA, dim = c(nsims, length(sigma_vec), 2))
  for (i in 1:length(sigma_vec)) {
    for (isim in 1:nsims) {
      sigma <- sigma_vec[i]

      # Simulate data set
      y <- simulate_data(n, sigma, lambda, homo, trend)

      # Remove trend
      # Handle non-stationary means based on selected method
      detrended <- detrend_data(y, window_size, detrend)

      metrics[isim, i, ] <- volatility::compute_all_metrics(
        detrended,
        window_size = window_size
      )
    }
  }

  return(metrics)
}
