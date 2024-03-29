#' Mutation Rate
#'
#' @description
#' The function calculates two different estimates of the mutation rate:
#' Watterson's estimator and the pairwise difference estimator.
#'
#' @usage
#' mutRate(SFS)
#'
#' @param SFS
#' vector with the site frequency spectrum.
#'
#' @return
#' List containing the following components:
#' \item{Watterson}{Watterson's estimator.}
#' \item{pairwDiff}{The pairwise difference estimator.}
#'
#' @examples
#' mutRate(c(2,1,0,0,0,1,0))
#'
#' @details
#' See \code{\link{SFS}} for explanation of the site frequency spectrum.
#'
#' If one runs \code{mutRate}
#' with a vector that contains anything other than natural numbers,
#' an error will occur.
#'
#' For details about Watterson's estimator and the pairwise difference
#' estimator, consult the analyzeDNA vignette.
#'
#' @references
#' Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
#' Roberts and Company Publishers.
#'
#' @export

mutRate <- function(SFS){
  SFS <- SFS[1:length(SFS)]
  # Make error if input is not a vector
  if(is.vector(SFS)=="FALSE"){
    stop('Input should be a vector')
  }
  # Make error if we have negative entries
  if(min(SFS)<0){
    stop('Entries in SFS must be natural numbers (0 included)')
  }
  # Make error if not whole numbers
  if(!isTRUE(all(SFS == floor(SFS)))){
    stop('Entries in SFS must be natural numbers (0 included)')
  }
  res <- list()
  # Watterson's estimator (the numerator is the number of segregating sites):
  res$Watterson <- sum(SFS)/sum(1/c(1:length(SFS)))
  # The estimator based on the pairwise difference:
  res$pairwDiff <- pairwDiff(SFS)
  return(res)
}
