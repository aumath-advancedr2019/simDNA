#' Mutation Rate
#'
#' @description
#' The function calculates two different estimates of the mutation rate: Watterson's estimator and the pairwise difference estimator.
#'
#' @usage
#' mutRate(c(2,1,0,0,0))
#'
#' @param SFS
#' The input is a vector with the site frequency spectrum.
#'
#' @return
#' The function returns a list with Watterson's estimator and the pairwise difference estimator
#'
#' @examples
#' mutRate(c(2,1,0,0,0,1,0))
#' ## $Watterson
#' ## [1] 1.5427
#' ##
#' ## $pairwDiff
#' ## [1] 1.357143
#'
#' @export


mutRate <- function(SFS){
  res <- list()
  # Watterson's estimator (the numerator is the number of segregating sites):
  res$Watterson <- sum(SFS)/sum(1/c(1:length(SFS)))
  # The estimator based on the pairwise difference:
  res$pairwDiff <- pairwDiff(SFS)
  return(res)
}
