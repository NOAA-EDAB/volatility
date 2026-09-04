#' print results in a table
#'
#'
#'
#'@export

create_results_table <- function(metrics) {
  mns <- t(apply(metrics, c(2, 3), mean))
  stds <- t(apply(metrics, c(2, 3), sd))
  colnames(mns) <- sigma_vec
  colnames(stds) <- sigma_vec

  metrics_summary <- rbind(
    `CV (Mean)` = mns[1, ],
    `CV (SD)` = stds[1, ],
    `IQR (Mean)` = mns[2, ],
    `IQR (SD)` = stds[2, ]
  )

  baseline <- metrics_summary

  return(baseline)
}
