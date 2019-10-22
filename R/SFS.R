# Input: SNP or matrix with gene sequences

SFS <- function(SNP){
  count <- colSums(SNP)
  # If there is anything other than 0's and 1's in the matrix, throw an error
  if(sum(length(which(SNP==0))+length(which(SNP==1)))!=ncol(SNP)*nrow(SNP)){
    stop('The SNP matrix is only allowed to contain zeroes and ones')
  }
  # If there is a position on which all sequences has a mutation, throw an error
  if(any(count==nrow(SNP))){
    stop('There is a position on which a mutation occur on all sequences.
         This is not allowed and suggests the ancestral state is wrong.')
  }
  res <- rep(0,nrow(SNP)-1)
  dat <- as.data.frame(table(count[count>0]))
  res[dat[,1]] <- dat[,2]
  return(res)
}

# SFS(SNP)
#
# SNP <- matrix(0,4,8)
# SNP[1,2] = SNP[1:2,3] = SNP[2:4,5] = SNP[2:4,6] = SNP[,8] <- 1
# SNP[,4] <- 1