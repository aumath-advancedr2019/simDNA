mutRate <- function(SFS){
  res <- list()
  # Watterson's estimator (the numerator is the number of segregating sites):
  res$Watterson <- sum(SFS)/sum(1/c(1:length(SFS)))
  # The estimator based on the pairwise difference:
  res$pairwDiff <- pairwDiff(SFS)
  return(res)
}
