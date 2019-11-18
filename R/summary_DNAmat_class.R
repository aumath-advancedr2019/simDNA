#' Summary statistics
#'
#' @description Gives summary statistics useful for analyzing DNA.
#'
#' @usage
#' ## S3 method for class DNAmat_class
#' summary(DNAmat)
#'
#' @param DNAmat
#' matrix; either a single-nucleotide polymorphism matrix or a segregating
#' sites matrix.
#'
#' @examples
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
  cat("Summary statistics:\n")
  cat("Site frequency spectrum:", SFS, "\n")
  estMutRate <- mutRate(SFS)
  cat("Estimators of the scaled mutation rate:\n",
      "Watterson's estimator:", estMutRate$Watterson, "\n",
      "Pairwise difference estimator:", estMutRate$pairwDiff, "\n")
  cat("Tajima's D:", TajimaD(SFS), "\n")
}
