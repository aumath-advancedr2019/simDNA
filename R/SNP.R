#' SNP (Single-Nucleotide Polymorphism)
#' @description
#' The function calculates the SNP matrix.
#'
#' @usage
#' SNP(matrix(c(0,0,0,1,0,0,0,0,1),nrow=3))
#'
#' @param DNAseq
#' A matrix with DNA sequences. Each row is a DNA sequence represented by 0's and 1's, where 1 indicates that a mutation has happened,0 that no mutations has happened.
#'
#' @return
#' The output of the function is a list.
#' The first element in the list is a vector containing the positions on which one or more mutations has occured.
#' The second element is the SNP matrix itself.
#'
#' @examples
#' SNP(matrix(c(0,0,0,1,0,0,0,0,1),nrow=3))
#' ## $positions
#' ## [1] 2 3
#' ##
#' ## $SNPmat
#' ##      [,1] [,2]
#' ## [1,]    1    0
#' ## [2,]    0    0
#' ## [3,]    0    1
#'
#' @export
#'

SNP <- function(DNAseq){
  count <- colSums(DNAseq)
  positions <- which(count>0)
  mat <- DNAseq[,positions]
  res <- list()
  res$positions <- positions
  res$SNPmat <- mat
  return(res)
}


# mat <- matrix(0,3,5)
# mat[1,2] <- mat[3,3] <- 1
# mat
#
# SNP(mat)
