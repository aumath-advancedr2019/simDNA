#' Summary statistics
#'
#' @description Gives summary statistics useful for analyzing DNA.
#'
#' @usage
#' ## S3 method for class SFS_class
#' summary(SFS)
#'
#' @param SFS
#' vector with the site frequency spectrum.
#'
#' @examples
#' SFS <- c(2,1,0,0,0,1,0,0)
#' class(SFS) <- "SFS_class"
#' summary(SFS)
#'
#' @seealso
#' \code{\link{mutRate}}, \code{\link{TajimaD}}
#'
#' @export
#'

summary.SFS_class <- function(SFS){
  SFS <- SFS[1:length(SFS)]
  cat("Summary statistics:\n")
  estMutRate <- mutRate(SFS)
  cat("Estimators of the mutation rate:\n",
      "Watterson's estimator:", estMutRate$Watterson, "\n",
      "Pairwise difference estimator:", estMutRate$pairwDiff, "\n")
  cat("Tajima's D:", TajimaD(SFS), "\n")
}
