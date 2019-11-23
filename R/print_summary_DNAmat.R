#' Internal function
#'
#' @description
#' This is an internal function
#'
#' @export


print.summary_DNAmat <- function(res){
  cat("Summary statistics:\n")
  cat("Site frequency spectrum:", res$SFS, "\n")
  cat("Estimators of the scaled mutation rate:\n",
      "Watterson's estimator:", res$Watterson, "\n",
      "Pairwise difference estimator:", res$pairwDiff, "\n")
  cat("Tajima's D:", res$TajimaD, "\n")
}

