#Input:
#n = sample size.
#seqLen = the length of DNA sequences.
#mutRate = the mutation rate.
#expRate = the rate of the exponentially growing population.
#expansionTime = parameter determining the time of expansion.
#proportion = parameter determining the fraction of the population size before the expansion.
#popType = the type of population size.
#
#Output:
#a vector of branch lengths in the following order (T_2, ..., T_n) using help functions
#simBranchFixed, simBranchVar and simBranchSudExpansion.
simBranch <- function(n, seqLen, mutRate, expRate, expansionTime, proportion, popType){
  if(popType=="fixedPop"){
    return(simBranchFixed(n))
  }

  if(popType=="varPop"){
    return(simBranchVar(n, expRate))
  }

  if(popType=="sudExpansionPop"){
    return(simBranchSudExpansion(n, expansionTime, proportion))
  }
  else(warning("Type of population not defined"))
}
