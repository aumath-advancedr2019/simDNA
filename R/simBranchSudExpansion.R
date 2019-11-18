#Input:
#n = sample size.
#expansionTime = parameter determining the time of expansion.
#proportion = parameter determining the fraction of the population size before the expansion.
#
#Output:
#a vector of branch lengths in the following order (T_2, ..., T_n) using algorithm 2.1 from
#Simon Tavaré (2004): Ancestral Inference in Population Genetics (ST 2004).
simBranchSudExpansion <- function(n, expansionTime, proportion){
  # Make error if proportion not in (0,1]:
  if(proportion>1 || proportion<=0){
    stop('proportion must be in interval (0,1]')
  }

  branches <- rev(simBranchFixed(n))
  cumuBranch <- cumsum(branches)

  #from HO6, exercise 2 in the DNA course:
  inv <- function(y){
    if(y < expansionTime){y}
    else{expansionTime+proportion*(y-expansionTime)}
  }

  invCumuBranch <- rep(0, n-1)
  for(i in 1:(n-1)){
    invCumuBranch[i] <- inv(cumuBranch[i])
  }

  res <- rev(diff(c(0, invCumuBranch)))
  class(res) <- "sudExpPop"
  return(res)
}










