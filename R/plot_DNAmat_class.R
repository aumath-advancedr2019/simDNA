#'Site frequency spectrum plot
#'
#'@description
#'Plots the site frequency spectrum of \code{DNAmat}.
#'
#'@param DNAmat
#' matrix; either a single-nucleotide polymorphism matrix or a segregating
#' sites matrix. See \code{\link{SNP}} for explanation.
#' @param xlab
#' a title for the x axis. Is by default "Index i". See \code{\link[graphics]{title}}.
#'@param ylab
#' a title for the y axis. Is by default "Number of i-tons". See \code{\link[graphics]{title}}.
#' @param main
#' an overall title for the plot. Is by default "Site frequency spectrum". See \code{\link[graphics]{title}}.
#' @param ...
#' arguments passed on from \code{\link[graphics]{plot}}.
#'
#' @details
#' The terminology "Number of i-tons" corresponds to the i'th entry in the site
#' frequency spectrum. See \code{\link{SFS}} for further explanation.
#'
#' @examples
#' plot(simDNAseq(n = 8, seqLen = 20, mutRate = 5,
#'                popType = "varPop", expRate = 1.5), col="red")
#'
#'
#' SegSitesMat <- matrix(sample(x=c(0, 1), size=26*12,
#'                       replace=TRUE, prob=c(0.9, 0.1)),
#'                       nrow=12, ncol=26, byrow=TRUE)
#' class(SegSitesMat) <- "DNAmat_class"
#' plot(SegSitesMat)
#'
#' @export

plot.DNAmat_class <- function(DNAmat, xlab="Index i", ylab="Number of i-tons",
                              main="Site frequency spectrum", ...){
  sfs <- SFS(DNAmat)
  sfs <- sfs[1:length(sfs)]
  plot(sfs, xlab=xlab, ylab=ylab, main=main, type="h", yaxt="n", xaxt="n", ...)
  axis(side=2, at=c(0:max(sfs)), labels=c(0:max(sfs)))
  axis(side=1, at=c(1:length(sfs)), labels=c(1:length(sfs)))
}
