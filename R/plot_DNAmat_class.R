#'plot.DNAmat_class
#'
#'@description
#'Plots the site frequency spectrum of \code{DNAmat}.
#'
#'@param DNAmat
#' matrix; either a single-nucleotide polymorphism matrix or a segregating
#' sites matrix. See \code{\link{SNP}} for explanation.
#'
#' @examples
#' plot(simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
#' expRate = 1.5), col="red")
#'
#' SegSitesMat <- matrix(sample(x=c(0, 1), size=26*4, replace=TRUE, prob=c(3/4, 1/4)),
#' nrow=4, ncol=26, byrow=TRUE)
#' plot.DNAmat_class(SegSitesMat)
#'
#' @export

plot.DNAmat_class <- function(DNAmat, ...){
  plot(SFS(DNAmat), xlab="Index i", ylab="Number of i-tons", main="Site frequency spectrum",
       type="h")
}
