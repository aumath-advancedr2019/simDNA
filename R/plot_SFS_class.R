#'Site frequency spectrum plot
#'
#'@description
#'Plots the site frequency spectrum.
#'
#'@param SFS
#' vector with the site frequency spectrum.
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
#' DNAmat <- simDNAseq(n = 24, seqLen = 58, mutRate = 7,
#'                     popType = "sudExpPop", expansionTime = 3,
#'                     proportion = 0.6)
#' plot(SFS(DNAmat), col="green")
#'
#'
#' SFSvec <- c(2, 4, 1, 0, 0)
#' class(SFSvec) <- "SFS_class"
#' plot(SFSvec)
#'
#' @export

plot.SFS_class <- function(SFS, xlab="Index i", ylab="Number of i-tons",
                              main="Site frequency spectrum", ...){
  plot.default(SFS, xlab=xlab, ylab=ylab, main=main, type="h", yaxt="n", xaxt="n", ...)
  axis(side=2, at=c(0:max(SFS)), labels=c(0:max(SFS)))
  axis(side=1, at=c(1:length(SFS)), labels=c(1:length(SFS)))
}
