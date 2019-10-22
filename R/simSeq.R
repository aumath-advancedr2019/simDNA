#simulates DNA sequences based on branch lengths, sequence length and mutation rate
#branch lengths should start with 2 branches and end with sample size branches
simSeq <- function(branchLen, seqLen, mutRate){
  nOfSeq <- length(branchLen)+1
  sequences <- matrix(0, nrow=nOfSeq, ncol=seqLen)
  repeat{
    nOfMut <- rpois(n=length(branchLen), lambda=mutRate/2*c(2:nOfSeq)*branchLen)
    if(sum(nOfMut)<seqLen) break
  }
  positions <- 1:seqLen
  seqAfterCoal <- as.list(1:nOfSeq)
  for(i in 1:length(branchLen)){
    if(nOfMut[i]!=0){
      seqWithMut <- sample(x=c(1:length(seqAfterCoal)), size=nOfMut[i], replace=TRUE)
      posWithMut <- sample(x=positions, size=nOfMut[i], replace=FALSE)
      positions <- positions[-which(positions %in% posWithMut)]
      for(j in 1:nOfMut[i]){
        sequences[seqAfterCoal[[ (seqWithMut[j]) ]], posWithMut[j]] <- 1
      }
    }
    coalEvent <- sample(x=c(1:length(seqAfterCoal)), size=2, replace=FALSE)
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

l2=as.list(c(1:5))
l2[[3]]=c(2,4)
seqWithMut=c(1,3,5)
posWithMut=c(7,4,5)
mat=matrix(0,nrow=5,ncol=7)
j=2
mat[l2[[ (seqWithMut[j]) ]], posWithMut[j]] <- 1




