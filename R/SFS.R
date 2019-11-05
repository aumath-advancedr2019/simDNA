#' Site Frequency Spectrum
#'
#' @description
#' The function SFS calcultes the site frequency spectrum.
#'
#' @usage
#' SFS(matrix(c(0,1,0,1,0,0,0,0,0),nrow=3))
#'
#' @details
#' The function calculates the site frequency spectrum and outputs it as a vector. That is, the i'th entry of the output vector is the number of mutations that happened on a branch where exactly i sequences has coalesced.
#'
#' @param X
#' Matrix; either a SNP or a segregating sites matrix, see details. RYK RESTEN TIL DETAILS where each row is a sequence. The matrix should have a 1 where a mutation has occured and a 0 otherwise. If this is not the case, it will cause an error. Also, an error will be caused if there exists a position on which all sequences have a mutation, as this is an indication that the ancestral state is in fact not the ancestral state.
#'
#' @return
#' The function returns a vector with the site frequency spectrum
#'
#' @examples
#' SFS(matrix(c(0,1,0,1,0,0,0,0,0),nrow=3))
#' ## [1] 2 0
#'
#' @export

# Ret input til at være fx X så den dækker alt. RET DET ALLE STEDER!!!

# Fjern forklaring om errors. Forklar at hvis rene 1'er ændrer den det til 0'er.
# Ændr det også i koden.

# I details: slet første linje, omformuler resten. Indsæt hvad der sker med error for rene 0'er.


SFS <- function(SNP){
  count <- colSums(SNP)
  # If there is anything other than 0's and 1's in the matrix, throw an error
  if(sum(length(which(SNP==0))+length(which(SNP==1)))!=ncol(SNP)*nrow(SNP)){
    stop('The SNP matrix is only allowed to contain zeroes and ones')
  }
  # If there is a position on which all sequences has a mutation, make warning,
  # change to all 0's
  # if(any(count==nrow(SNP))){
  #   stop('There is a position on which a mutation occur on all sequences.
  #        This is not allowed and suggests the ancestral state is wrong.')
  # }
  if(any(count==nrow(SNP))){
    warning('There is one or more position on which a mutation occur on all sequences. See ?SFS')
  }
  for(i in 1:ncol(SNP)){
    if(count[i]==nrow(SNP)){
      count[i]=0
    }
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
#
# SFS(SNP)



# REMEMBER: TO RUN ROXYGEN HELP FILES: devtools::document()
