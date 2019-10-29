#Input:
#n = the sample size.
#
#Output:
#a vector of branch lengths in the following order (T_2, ..., T_n).
simBranchFixed <- function(n){
  res <- rexp(n=n-1, rate=choose(c(2:n), 2))
  class(res) <- "fixedPop"
  return(res)
}
