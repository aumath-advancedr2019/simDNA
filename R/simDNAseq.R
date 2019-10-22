simDNAseq <- function(n, seqLen, mutRate, expRate, expansionTime, proportion, popType){
  branchLen <- simBranch(n, seqLen, mutRate, expRate, expansionTime, proportion, popType)
  UseMethod("simDNAseq", branchLen)
}

simDNAseq.fixedPop <- function(n, seqLen, mutRate, expRate=NULL,
                               expansionTime=NULL, proportion=NULL){
  branchLen <- simBranchFixed(n) #might not be necessary
  return(simSeq(branchLen, seqLen, mutRate))
  #should probably return the SNP matrix instead
}

simDNAseq.varPop <- function(n, seqLen, mutRate, expRate,
                               expansionTime=NULL, proportion=NULL){
  branchLen <- simBranchVar(n, expRate) #might not be necessary
  return(simSeq(branchLen, seqLen, mutRate))
  #should probably return the SNP matrix instead
}


simDNAseq.sudExpansionPop <- function(n, seqLen, mutRate, expRate=NULL,
                               expansionTime, proportion){
  branchLen <- simBranchSudExpansion(n, expanstionTime, proportion) #might not be necessary
  return(simSeq(branchLen, seqLen, mutRate))
  #should probably return the SNP matrix instead
}

