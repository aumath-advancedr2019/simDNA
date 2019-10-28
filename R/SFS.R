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
#' @param SNP
#' The input is a SNP (single-nucleotide polymorphism) matrix, or a matrix with DNA sequences, where each row is a sequence. The matrix should have a 1 where a mutation has occured and a 0 otherwise. If this is not the case, it will cause an error. Also, an error will be caused if there exists a position on which all sequences have a mutation, as this is an indication that the ancestral state is in fact not the ancestral state.
#'
#' @return
#' The function returns a vector with the site frequency spectrum
#'
#' @examples
#' SFS(matrix(c(0,1,0,1,0,0,0,0,0),nrow=3))
#' ## [1] 2 0
#'
#' @export

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



# REMEMBER: TO RUN ROXYGEN HELP FILES: devtools::document()
