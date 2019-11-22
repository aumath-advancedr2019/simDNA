#' Internal function
#'
#' @description
#' This is an internal function.
#'
#' @export
#'


print.summary_SFS <- function(res){
  cat("Summary statistics:\n")
  cat("Estimators of the mutation rate:\n",
      "Watterson's estimator:", res$Watterson, "\n",
      "Pairwise difference estimator:", res$pairwDiff, "\n")
  cat("Tajima's D:", res$TajimaD, "\n")
}
