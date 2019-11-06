#' SNP (Single-Nucleotide Polymorphism) matrix
#' @description
#' The function calculates the SNP matrix.
#'
#' @usage
#' SNP(segSites)
#'
#' @param segSites
#' Segregating sites matrix. See details.
#'
#' @return
#' List containing the following components:
#' \item{positions}{The positions on which one or more mutations occurred.}
#' \item{SNPmat}{The single-nucleotide polymorphism matrix.}
#'
#' @examples
#' SNP(matrix(c(0,0,0,1,0,0,0,0,1),nrow=3))
#'
#' @details
#' In the segregating sites matrix, each row is a DNA sequence represented
#' by zeros and ones, where 1 indicates that a mutation has occurred,
#' 0 that no mutations have occurred. If the \code{segSites} matrix contains
#' anything other than zeros and ones, it will cause an error.
#'
#' The SNP matrix extracts the columns from the segregating sites matrix
#' where one or more mutations have happened, and thus it is a more compact
#' representation than the segregating sites matrix.
#'
#' @export

SNP <- function(segSites){
  count <- colSums(segSites)
  positions <- which(count>0)
  # If there is anything other than 0's and 1's in the matrix, throw an error
  if(sum(length(which(segSites==0))+length(which(segSites==1)))!=ncol(segSites)*nrow(segSites)){
    stop('The segSites matrix is only allowed to contain zeroes and ones')
  }
  mat <- segSites[,positions]
  res <- list()
  res$positions <- positions
  res$SNPmat <- mat
  return(res)
}
