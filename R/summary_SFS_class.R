#' Summary statistics
#'
#' @description
#' Prints summary statistics useful for analyzing DNA
#' and returns themn in a list.
#'
#' @usage
#' ## S3 method for class 'SFS_class'
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
#' @return
#' List containing the following components:
#' \item{Watterson}{Watterson's estimator.}
#' \item{pairwDiff}{The pairwise difference estimator.}
#' \item{TajimaD}{Tajima's D}
#'
#' @seealso
#' \code{\link{mutRate}}, \code{\link{TajimaD}}
#'
#' @export
#'

summary.SFS_class <- function(SFS){
  SFS <- SFS[1:length(SFS)]
  res <- list()
  estMutRate <- mutRate(SFS)
  res$Watterson <- estMutRate$Watterson
  res$pairwDiff <- estMutRate$pairwDiff
  res$TajimaD <- TajimaD(SFS)
  cat("Summary statistics:\n")
  cat("Estimators of the mutation rate:\n",
      "Watterson's estimator:", estMutRate$Watterson, "\n",
      "Pairwise difference estimator:", estMutRate$pairwDiff, "\n")
  cat("Tajima's D:", res$TajimaD, "\n")
  return(res)
}
