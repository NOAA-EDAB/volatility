#' simulate time series.
#'
#' @description Either stationary time series, or non stationary (1st or second order)
#' non stationarity is coded to increase linearly with time
#'
#' @param n length of the time series
#' @param mu Mean value of the Gaussian error (Default = 0)
#' @param sigma Standar deviation of the Gaussian error (Default = 1)
#' @param mu_t Boolean. Should the time series incorporate a linear trend (Default = FALSE)
#' @param sigma_t Boolean Should the time series incorporate herteroskedasticity (Default = FALSE)
#'
#' @section Model:
#'
#'
#'
#'
#' @return vector
#'

simulate_data <- function(n, mu = 0, sigma = 1, mu_t = FALSE, sigma_t = FALSE) {
  t <- 1:n
  if (sigma_t) {
    errors <- rnorm(n, mean = mu, sd = sqrt(t) * sigma)
  } else {
    errors <- rnorm(n, mean = mu, sd = sigma)
  }

  if (mu_t) {
    ts <- t + errors
  } else {
    ts <- errors
  }
  return(ts)
}
