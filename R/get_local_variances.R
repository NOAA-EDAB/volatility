#' Calculate local variances of a time series
#'
#' @description Estimates over either non overlapping chunks of the time series
#' or overlapping are available. Choice is often based on available time series length
#'
#' @param y Time series (vector)
#' @param window_size scalar. Width of the window over which variances are calculated
#' @param overlapping boolean. Whether to use overlapping (Default = TRUE) or not overlapping chunks (FALSE)
#'
#' @return A vector of local variances
#'

get_local_variances <- function(y, window_size = 20, overlapping = TRUE) {
  n <- length(y)
  if (overlapping) {
    num_windows <- n - window_size + 1
    if (n < window_size) {
      stop("Window size cannot exceed data length after detrending.")
    }

    local_vars <- numeric(num_windows)
    for (i in 1:(n - window_size + 1)) {
      window_data <- y[i:(i + window_size - 1)]
      local_vars[i] <- var(window_data)
    }
  } else {
    num_windows <- floor(n / window_size)
    local_vars <- numeric(num_windows)

    for (i in 1:num_windows) {
      window_data <- y[((i - 1) * window_size + 1):(i * window_size)]
      local_vars[i] <- var(window_data)
    }
  }
  return(local_vars)
}
