#Input:
#branchLen = the vector of branch lengths in the following order (T_2, ..., T_n) where
#n is the sample size.
#seqLen = the length of DNA sequences.
#mutRate = the mutation rate.
#
#Output:
#simulates DNA sequences and returns the segregating sites matrix.
simSeq <- function(branchLen, seqLen, mutRate){
  #the sample size n
  nOfSeq <- length(branchLen)+1
  #the (empty) segregating sites matrix
  sequences <- matrix(0, nrow=nOfSeq, ncol=seqLen)
  repeat{ #generates the number of mutations in the population such that the number
    #is smaller than the sequence length
    nOfMut <- rpois(n=length(branchLen), lambda=mutRate/2*c(2:nOfSeq)*branchLen)
    if(sum(nOfMut)<seqLen) break
  }
  #possible positions to place mutations
  positions <- 1:seqLen
  #list helping us keep track of the coalesced branches
  seqAfterCoal <- as.list(1:nOfSeq)
  #we sprinkle on mutations within each level of the tree
  for(i in 1:length(branchLen)){
    if(nOfMut[i]!=0){
      #which sequences should have mutation(s)
      seqWithMut <- sample(x=c(1:length(seqAfterCoal)), size=nOfMut[i], replace=TRUE)
      #at which site(s) should the mutation(s) occur
      posWithMut <- sample(x=positions, size=nOfMut[i], replace=FALSE)
      #updating the possible positions to place mutations
      positions <- positions[-which(positions %in% posWithMut)]
      #placing mutations at the above chosen site(s) and sequence(s)
      for(j in 1:nOfMut[i]){
        sequences[seqAfterCoal[[ (seqWithMut[j]) ]], posWithMut[j]] <- 1
      }
    }
    #determining the next coalescent event
    coalEvent <- sample(x=c(1:length(seqAfterCoal)), size=2, replace=FALSE)
    #updating the list
    seqAfterCoal[[ (coalEvent[1]) ]] <- c(seqAfterCoal[[ (coalEvent[1]) ]],
                                          seqAfterCoal[[ (coalEvent[2]) ]])
    seqAfterCoal <- seqAfterCoal[-(coalEvent[2])]
  }
  return(sequences)
}


#To do:
#test at den virker, hvis branchLen består af ét tal samt en vektor
#test at den virker (på den måde, vi ønsker det)
#dokumentation
#tilføj kommentarer undervejs i koden
