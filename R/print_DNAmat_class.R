#' Print DNA sequences
#'
#' @description
#' Prints a segregating sites matrix.
#'
#' @param DNAmat either a segregating sites matrix or single-nucleotide polymorphism.
#' See \code{\link{SNP}} for explanation.
#' @param ...
#' arguments passed on from \code{\link[base]{print}}.
#'
#' @details
#' It is used for printing the simulated segregating sites matrix from \code{simDNAseq}
#' and the SNP matrix from \code{SNP}.
#'
#' @examples
#' SegSitesMat <- matrix(c(1, 1, 1, 1, 0,
#'                         0, 1, 1, 1, 0,
#'                         0, 0, 0, 1, 0,
#'                         0, 0, 0, 0, 0,
#'                         0, 0, 0, 0, 0), byrow=T, nrow=5)
#'class(SegSitesMat) <- "DNAmat_class"
#'print(SegSitesMat)
#'
#' @export

print.DNAmat_class <- function(DNAmat, ...){
  res <- DNAmat[1:nrow(DNAmat),1:ncol(DNAmat)]
  class(res) <- class(DNAmat)
  print.table(res)
}
