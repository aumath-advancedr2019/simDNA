#'Site frequency spectrum plot
#'
#'@description
#'Plots the site frequency spectrum of \code{DNAmat}.
#'
#'@usage
#'## S3 method for class 'DNAmat_class'
#'plot(DNAmat, xlab="Index i", ylab="Number of i-tons",
#'main="Site frequency spectrum", ...)
#'
#'@param DNAmat
#' matrix; either a single-nucleotide polymorphism matrix or a segregating
#' sites matrix. See \code{\link{SNP}} for explanation.
#'
#' @examples
#' plot(simDNAseq(n = 8, seqLen = 20, mutRate = 5,
#'                popType = "varPop", expRate = 1.5), col="red")
#'
#'
#' SegSitesMat <- matrix(sample(x=c(0, 1), size=26*12,
#'                       replace=TRUE, prob=c(0.9, 0.1)),
#'                       nrow=12, ncol=26, byrow=TRUE)
#' plot.DNAmat_class(SegSitesMat)
#'
#'
#' SNPmat <- matrix(c(1, 0, 0, 1, 1, 0, 0,
#'                    0, 1, 0, 1, 0, 0, 0,
#'                    0, 0, 1, 0, 0, 1, 1,
#'                    1, 0, 0, 0, 0, 0, 1,
#'                    1, 0, 0, 1, 0, 0, 0,
#'                    1, 0, 1, 1, 0, 1, 0), nrow=6, ncol=7, byrow=TRUE)
#' class(SNPmat) <- "DNAmat_class"
#' plot(SNPmat, main="SFS of SNP matrix")
#'
#' @export

plot.DNAmat_class <- function(DNAmat, xlab="Index i", ylab="Number of i-tons",
                              main="Site frequency spectrum", ...){
  sfs <- SFS(DNAmat)
  plot(sfs, xlab=xlab, ylab=ylab, main=main, type="h", yaxt="n", xaxt="n", ...)
  axis(side=2, at=c(0:max(sfs)), labels=c(0:max(sfs)))
  axis(side=1, at=c(1:length(sfs)), labels=c(1:length(sfs)))
}
