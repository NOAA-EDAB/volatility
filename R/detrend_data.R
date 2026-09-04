#'
#'
#'
#' @export

detrend_data <- function(
  y,
  window_size,
  detrend = c("none", "linear", "diff", "rolling")
) {
  if (detrend == "none") {
    # do nothing
  } else if (detrend == "linear") {
    time_idx <- 1:length(y)
    y <- residuals(lm(y ~ time_idx))
  } else if (detrend == "diff") {
    y <- diff(y)
  } else if (detrend == "rolling") {
    # Compute a centered rolling mean using the window_size
    n_y <- length(y)
    half_w <- floor(window_size / 2)
    rmean <- numeric(n_y)
    for (i in 1:n_y) {
      s_idx <- max(1, i - half_w)
      e_idx <- min(n_y, i + half_w)
      rmean[i] <- mean(y[s_idx:e_idx])
    }
    y <- y - rmean
  }

  return(y)
}
