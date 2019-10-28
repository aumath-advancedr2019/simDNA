#' Pairwise Difference
#'
#' @description
#' The function calculates the pairwise difference based on the site frequency spectrum.
#'
#' @usage
#' pairwDiff(c(2,1,0,0))
#'
#' @param SFS
#' The input is a vector with the site frequency spectrum.
#'
#' @return
#' The function returns the pairwise difference.
#'
#' @examples
#' pairwDiff(c(2,1,0,0))
#' ## [1] 1.4
#'
#' @export

pairwDiff <- function(SFS){
  # The sample size:
  sampSize <- length(SFS) +1
  # Helping vectors:
  i <- 1:(sampSize-1)
  # The prairwise difference:
  res <- 1/choose(sampSize,2)*sum(i*(sampSize-i)*SFS)
  return(res)
}

pairwDiff(c(2,1,0))
