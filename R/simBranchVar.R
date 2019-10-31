#Input:
#n = the sample size.
#expRate = the rate of the exponentially growing population.
#
#Output:
#a vector of branch lengths in the following order (T_2, ..., T_n) using algorithm 2.1 from
#Simon Tavaré (2004): Ancestral Inference in Population Genetics (ST 2004).
simBranchVar <- function(n, expRate){
  branches <- rev(simBranchFixed(n))
  cumuBranch <- cumsum(branches)

  #from example on page 29 in ST 2004:
  inv <- function(y){
    return(log(1+expRate*y)/expRate)
  }
  invCumuBranch <- rep(0, n-1)
  for(i in 1:(n-1)){
    invCumuBranch[i] <- inv(cumuBranch[i])
  }
  res <- rev(diff(c(0, invCumuBranch)))
  class(res) <- "varPop"
  return(res)

}
