#' simulate time series.
#'
#' @description Either stationary time series, or non stationary (1st or second order)
#' non stationarity is coded to increase linearly with time
#'
#' @param n length of the time series
#' @param sigma Standar deviation of the Gaussian error (Default = 1)
#' @param lambda Boolean. Should the time series exponential growth in variance (Default = FALSE)
#' @param homo Boolean. Should the time series incorporate homoskedasticity (Default = TRUE)
#' @param trend Character. Form of trend: "none", "linear", "sinusoid", "both"
#'
#' @section Model:
#'
#'
#'
#'
#' @return vector
#'
#'@export

simulate_data <- function(
  n,
  sigma = 1,
  lambda = FALSE,
  homo = TRUE,
  trend = c("none", "linear", "sinusoid", "both")
) {
  trend <- match.arg(trend)

  if (!lambda && homo) {
    errors <- rnorm(n, mean = 0, sd = sigma)
  } else if (!lambda && !homo) {
    t <- seq(0, 1, length.out = n)
    errors <- rnorm(n, mean = 0, sd = sigma * t)
  } else {
    # lambda is TRUE regardless of homo
    t <- seq(0, 1, length.out = n)
    errors <- rnorm(n, mean = 0, sd = exp(sigma * t))
  }

  ## Simulate a trend
  if (trend == "none") {
    sim_trend <- 0
  } else if (trend == "linear") {
    sim_trend <- seq(0, 50, length.out = n)
  } else if (trend == "sinusoid") {
    sim_trend <- 10 * sin(seq(0, 3 * pi, length.out = n))
  } else if (trend == "both") {
    sim_trend <- seq(0, 50, length.out = n) +
      10 * sin(seq(0, 3 * pi, length.out = n))
  }

  y <- sim_trend + errors

  return(y)
}
