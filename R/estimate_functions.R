#' Calculate the slope of data set.
#'
#' @description Assumes Gausian, iid errors, and no fit through the origin
#'
#' @param data dataframe with a field named t
#'
#' @return The slope of the fitted line. scalar
#'

get_slope <- function(data) {
  fit <- stats::lm(y ~ t, data = data)
  slope <- stats::coef(fit)["t"]
  return(slope)
}

#' Calculate the sample variance
#'
#' @description Use moments for speed
#'
#' @param data time series vector
#' @param window scalar representing the width of the window to center variance calculation
#'
#' @return vector of sample variance esimates for each window

get_sample_variance <- function(data, window) {
  ex <- stats::filter(
    data,
    filter = rep(1 / window, window),
    method = "convolution",
    circular = FALSE,
    sides = 2
  )
  ex2 <- stats::filter(
    data^2,
    filter = rep(1 / window, window),
    method = "convolution",
    circular = FALSE,
    sides = 2
  )

  biased_var <- (ex2 - (ex^2))

  unbiased_var <- biased_var * (window / (window - 1))

  return(unbiased_var)
}

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
