#' Tajima's D
#'
#' @description
#' The function calculates Tajima's D.
#'
#' @usage
#' TajimaD(c(3,0,2,1,0,0,0))
#'
#' @param SFS
#' The input is a vector with the site frequency spectrum.
#'
#' @return The function returns Tajima's D.
#'
#' @examples
#' TajimaD(c(3,0,2,1,0,0,0))
#'
#' @export


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
  a1 <- sum(1:(sampSize-1))
  a2 <- sum((1:(sampSize-1))^2)
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
  #class(D) <- "TajD"
  return(D)
}


# D <- TajimaD(c(1,0,0,1,0))
# print(D)
# D[[1]]




# print.TajD <- function(d){
#   cat("Tajima's D was calculated to be", d[[1]], "\n")
#   cat("If Tajima's D is approximately 0 (often: in the interval [-2,2]), it could suggest a standard neutral model. \n")
#   cat("If Tajima's D << 0 (often: <2), it could suggest a growing population size or bottleneck.\n")
#   cat("If Tajima's D >> 0 (often: >2), it could suggest population subdivision.")
# }

# print(D)
