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
