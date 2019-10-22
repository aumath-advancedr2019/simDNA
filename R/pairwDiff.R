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
