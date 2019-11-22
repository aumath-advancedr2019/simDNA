#' Summary statistics
#'
#' @description
#' Calculates summary statistics useful for analyzing DNA.
#'
#' @usage
#' ## S3 method for class 'DNAmat_class'
#' summary(DNAmat)
#'
#' @param DNAmat
#' matrix; either a single-nucleotide polymorphism matrix or a segregating
#' sites matrix of class 'DNAmat_class'.
#'
#' @return
#' List containing the following components:
#' \item{SFS}{The site frequency spectrum}
#' \item{Watterson}{Watterson's estimator.}
#' \item{pairwDiff}{The pairwise difference estimator.}
#' \item{TajimaD}{Tajima's D}
#'
#' @examples
#' # Using the function simDNAseq:
#' summary(simDNAseq(n = 12, seqLen = 20, mutRate = 5,
#'                popType = "varPop", expRate = 1.5))
#'
#' # Creating a segregating sites matrix by hand:
#' DNAmat <- matrix(c(0,1,0,0,
#'                    0,1,0,1,
#'                    0,0,0,0,
#'                    0,0,0,0), 4,4, byrow=TRUE)
#' class(DNAmat) <- "DNAmat_class"
#' summary(DNAmat)
#'
#' @seealso
#' \code{\link{SFS}}, \code{\link{mutRate}}, \code{\link{TajimaD}}
#'
#' @export


summary.DNAmat_class <- function(DNAmat){
  SFS <- SFS(DNAmat[1:nrow(DNAmat),1:ncol(DNAmat)])
  res <- list()
  res$SFS <- SFS
  estMutRate <- mutRate(SFS)
  res$Watterson <- estMutRate$Watterson
  res$pairwDiff <- estMutRate$pairwDiff
  res$TajimaD <- TajimaD(SFS)
  class(res) <- "summary_DNAmat"
  res
}
