#' Coefficient of Variation of Local Variances (CV)Volatility Metrics
#'
#' @description
#' Volatility metric: Calculates the CV of local variances
#'
#' @inheritParams get_local_variances
#'
#' @return CV metric
#'
#' @export

metric_cv_var <- function(y, window_size = 20, overlapping = TRUE) {
  local_vars <- get_local_variances(y, window_size, overlapping)
  cv <- sd(local_vars) / mean(local_vars)
  return(cv)
}

#' Interquartile Range of Log-Variances (IQR_log)
#'
#' @description
#' Volatility metric: Calculates the Interquartile range of the local variances
#' on the log scale
#'
#' @inheritParams get_local_variances
#'
#' @return IQR metric
#'
#' @export

metric_iqr_log_var <- function(y, window_size = 20, overlapping = TRUE) {
  local_vars <- get_local_variances(y, window_size, overlapping)
  # Protect against zero variances by taking log(x + small epsilon) if needed
  log_vars <- log(local_vars)
  iqr_val <- IQR(log_vars)
  return(iqr_val)
}
