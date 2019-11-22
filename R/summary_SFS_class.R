#' Summary statistics
#'
#' @description
#' Calculates summary statistics useful for analyzing DNA.
#'
#' @usage
#' ## S3 method for class 'SFS_class'
#' summary(SFS)
#'
#' @param SFS
#' vector with the site frequency spectrum.
#'
#' @examples
#' # Using the function SFS:
#' DNAmat <- matrix(c(0,1,0,0,
#'                    0,1,0,1,
#'                    0,0,0,0,
#'                    0,0,0,0), 4,4, byrow=TRUE)
#' summary(SFS(DNAmat))
#'
#' Creating SFS by hand:
#' SFS <- c(2,1,0,0,0,1,0,0)
#' class(SFS) <- "SFS_class"
#' summary(SFS)
#'
#' @return
#' List containing the following components:
#' \item{Watterson}{Watterson's estimator.}
#' \item{pairwDiff}{The pairwise difference estimator.}
#' \item{TajimaD}{Tajima's D}
#'
#' @seealso
#' \code{\link{mutRate}}, \code{\link{TajimaD}}
#'
#' @export
#'

summary.SFS_class <- function(SFS){
  SFS <- SFS[1:length(SFS)]
  res <- list()
  estMutRate <- mutRate(SFS)
  res$Watterson <- estMutRate$Watterson
  res$pairwDiff <- estMutRate$pairwDiff
  res$TajimaD <- TajimaD(SFS)
  # Assign its own class to ensure that we only print at appropriate times
  class(res) <- "summary_SFS"
  # Use invisible to ensure correct printing
  return(invisible(res))
}
