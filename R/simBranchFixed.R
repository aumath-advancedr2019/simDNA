
simBranchFixed <- function(n){
  res <- rep(0,n-1)
  for(i in 2:n){
    res[i-1] <- rexp(n=1,rate=choose(i,2))
  }
  class(res) <- "fixedPop"
  return(res)
}
