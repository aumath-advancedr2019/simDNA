#' Pairwise Difference
#'
#' @description
#' The function calculates the pairwise difference based on the site
#' frequency spectrum.
#'
#' @usage
#' pairwDiff(SFS)
#'
#' @param SFS
#' vector with the site frequency spectrum.
#'
#' @return
#' The pairwise difference.
#'
#' @examples
#' pairwDiff(c(2,1,0,0))
#'
#' @details
#' See \code{\link{SFS}} for explanation of the site frequency spectrum.
#'
#' If one runs \code{pairwDiff}
#' with a vector that contains anything other than natural numbers,
#' an error will occur.
#'
#' For details about the pairwise difference, consult the analyzeDNA vignette.
#'
#' @references
#' Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
#' Roberts and Company Publishers.
#'
#' @export

pairwDiff <- function(SFS){
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
  # The sample size:
  sampSize <- length(SFS) +1
  # Helping vector:
  i <- 1:(sampSize-1)
  # The pairwise difference:
  res <- 1/choose(sampSize,2)*sum(i*(sampSize-i)*SFS)
  return(res)
}
