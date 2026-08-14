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
