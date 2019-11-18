#' Tajima's D
#'
#' @description
#' The function calculates Tajima's D.
#'
#' @usage
#' TajimaD(SFS)
#'
#' @param SFS
#' vector with the site frequency spectrum.
#'
#' @return Tajima's D.
#'
#' @examples
#' TajimaD(c(3,0,2,1,0,0,0))
#'
#' @details
#' The site frequency spectrum is a vector of length \eqn{n-1}, where
#' \eqn{n} is the sample size. The \eqn{i}'th entry is the number of
#' segregating sites where a mutation occur in \eqn{i} sequences, and
#' thus all entries must be natural numbers (0 included).
#' If one runs \code{TajimaD}
#' with a vector that contains anything other than natural numbers,
#' an error will occur.
#'
#' For details about Tajima's D, consult the analyzeDNA vignette.
#'
#'
#' @references
#' Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
#' Roberts and Company Publishers.
#'
#' @export

TajimaD <- function(SFS){
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
  mutRate <- mutRate(SFS)
  # Estimate of mutation rate based on pairwise difference estimator:
  pairwDiff <- mutRate$pairwDiff
  # Estimate of mutation rate based on Watterson's estimator:
  Watterson <- mutRate$Watterson
  # The numerator of Tajima's D:
  numerator <- pairwDiff - Watterson
  # Calculations needed for the denominator:
  sampSize <- length(SFS) +1
  a1 <- sum(1/(1:(sampSize-1)))
  a2 <- sum(1/((1:(sampSize-1))^2))
  b1 <- (sampSize+1)/(3*(sampSize-1))
  b2 <- 2*(sampSize^2+sampSize+3)/(9*sampSize*(sampSize-1))
  c1 <- b1-1/a1
  c2 <- b2-(sampSize+2)/(a1*sampSize)+a2/a1^2
  e1 <- c1/a1
  e2 <- c2/(a1^2+a2)
  S <- sum(SFS)
  # The denominator:
  denominator <- sqrt(e1*S+e2*S*(S-1))
  # Tajima's D:
  D <- numerator/denominator
  return(D)
}
