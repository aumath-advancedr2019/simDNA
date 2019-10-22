

# Using algorithm 2.1 from Simon Tavaré (2004): Ancestral Inference in
#Population Genetics (ST 2004)

simBranchSudExpansion <- function(n,expansionTime,proportion){
  branches <- rev(simBranchFixed(n))
  cumuBranch <- cumsum(branches)

  # HO6 from exercise 2 in the course Probability Models for DNA Sequence
  # Evolution
  inv=function(y){
    if(y < expansionTime){y}
    else{expansionTime+proportion*(y-expansionTime)}
  }

  invCumuBranch <- rep(0,n-1)
  for(i in 1:(n-1)){
    invCumuBranch[i] <- inv(cumuBranch[i])
  }

  res <- diff(c(0,invCumuBranch))
  class(res) <- "sudExpansionPop"
  return(res)

}










