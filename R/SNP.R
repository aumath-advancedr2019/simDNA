#' SNP (Single-Nucleotide Polymorphism) matrix
#' @description
#' The function calculates the SNP matrix.
#'
#' @usage
#' SNP(segSites)
#'
#' @param segSites
#' segregating sites matrix. See details.
#'
#' @return
#' List containing the following components:
#' \item{positions}{the positions on which one or more mutations occurred.}
#' \item{SNPmat}{the single-nucleotide polymorphism matrix.}
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
  # Count how many mutations are present in each position
  count <- colSums(segSites)
  # The positions on which we have a mutation
  positions <- which(count>0)
  # If there is anything other than 0's and 1's in the matrix, throw an error
  if(sum(length(which(segSites==0))+length(which(segSites==1)))!=ncol(segSites)*nrow(segSites)){
    stop('The input is only allowed to contain zeroes and ones')
  }
  # For the SNP matrix, we only display the rows where mutations happened
  mat <- segSites[,positions]
  res <- list()
  res$positions <- positions
  res$SNPmat <- mat
  class(res$SNPmat) <- c("DNAmat_class", class(res$SNPmat))
  return(res)
}
