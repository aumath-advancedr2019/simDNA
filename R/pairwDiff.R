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
#' Vector with the site frequency spectrum.
#'
#' @return
#' The pairwise difference.
#'
#' @examples
#' pairwDiff(c(2,1,0,0))
#'
#' @details
#' The site frequency spectrum is a vector of length \eqn{n-1}, where
#' \eqn{n} is the sample size. The \eqn{i}'th entry is the number of mutations
#' that occurred where exactly \eqn{i} sequences had coalesced, and
#' thus all entries must be natural numbers (0 included).
#' If one runs \code{pairwDiff}
#' with a vector that contains anything other than natural numbers,
#' an error will occur.
#'
#' For details about the pairwise difference, see the analyzeDNA
#' vignette by running the following code:
#'
#' \code{vignette("analyzeDNA", package = "simDNA")}
#'
#' @references
#' Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
#' Roberts and Company Publishers.
#'
#' @export

pairwDiff <- function(SFS){
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
  # Helping vectors:
  i <- 1:(sampSize-1)
  # The prairwise difference:
  res <- 1/choose(sampSize,2)*sum(i*(sampSize-i)*SFS)
  return(res)
}
