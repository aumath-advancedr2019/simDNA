#' Tajima's D
#'
#' @description
#' The function calculates Tajima's D.
#'
#' @usage
#' TajimaD(c(3,0,2,1,0,0,0))
#'
#' @param SFS
#' Vector with the site frequency spectrum.
#'
#' @return The function returns Tajima's D.
#'
#' @examples
#' TajimaD(c(3,0,2,1,0,0,0))
#'
#' @export

# Details: skriv formel (med Watterson's theta etc). Skriv ikke Watterson's og
 # pairwise difference, link til deres help.

TajimaD <- function(SFS){
  mutRate <- mutRate(SFS)
  # Estimate of mutation rate based on pairwise difference:
  pairwDiff <- mutRate$pairwDiff
  # Estimate of mutation rate based on Watterson's estimater
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
