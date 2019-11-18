#' Site Frequency Spectrum
#'
#' @description
#' The function calculates the site frequency spectrum.
#'
#' @usage
#' SFS(DNAmat)
#'
#' @details
#' The site frequency spectrum is a vector of length \eqn{n-1}, where
#' \eqn{n} is the sample size. The \eqn{i}'th entry is the number of
#' segregating sites where a mutation occur in \eqn{i} sequences.
#'
#' The input to the function should be either a single-nucleotide
#' polymorphism matrix or a segregating sites matrix. Either way,
#' each row in the matrix represents a DNA sequence.
#' The matrix should have a 1 where a mutation has occured and a 0 otherwise.
#' If this is not the case, it will cause an error.
#' If there exists a position on which all sequences have a mutation,
#' it will cause a warning, and such a position will be treated
#' as if it had no mutations.
#'
#'
#' @param DNAmat
#' matrix; either a single-nucleotide polymorphism matrix or a segregating
#' sites matrix. See details.
#'
#'
#' @return
#' The site frequency spectrum.
#'
#' @examples
#' SFS(matrix(c(0,1,0,1,0,0,0,0,0),nrow=3))
#'
#' @export

SFS <- function(DNAmat){
  # Count how many mutations are present in each position
  count <- colSums(DNAmat)
  # If there is anything other than 0's and 1's in the matrix, throw an error
  if(sum(length(which(DNAmat==0))+length(which(DNAmat==1)))!=ncol(DNAmat)*nrow(DNAmat)){
    stop('The input is only allowed to contain zeroes and ones')
  }
  # If there is a position on which all sequences has a mutation, make warning
  if(any(count==nrow(DNAmat))){
    warning('There is one or more positions on which a mutation occur on all sequences. See ?SFS')
  }
  # If position on which all sequences has mutation, change it to appear as no mutations
  count[count %in% nrow(DNAmat)]=0
  res <- rep(0,nrow(DNAmat)-1)
  if(sum(count)!=0){
    # Find out how many times we observed 1,2,3,... mutations
    dat <- as.data.frame(table(count[count>0]))
    # i'th entry of the SFS is the number of times we observed i mutations
    res[dat[,1]] <- dat[,2]
  }
  return(res)
}
