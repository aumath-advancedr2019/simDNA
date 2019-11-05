#' Mutation Rate
#'
#' @description
#' The function calculates two different estimates of the mutation rate: Watterson's estimator and the pairwise difference estimator.
#'
#' @usage
#' mutRate(c(2,1,0,0,0))
#'
#' @param SFS
#' Vector with the site frequency spectrum.
#'
#' @return
#' The function returns a list with Watterson's estimator and the pairwise difference estimator
#'
#' @examples
#' mutRate(c(2,1,0,0,0,1,0))
#' ## $Watterson
#' ## [1] 1.5427
#' ##
#' ## $pairwDiff
#' ## [1] 1.357143
#'
#' @export

# Prøv og link til help for paiirwDiff

# RET HVER GANG DER ER LISTE: SE princomp

# Fjern output fra eksempler.

# Details: forklar kort hvad site frequency spectrum er. Hver gang.

# Details: hav enten formel der er brugt eller reference.

# Generelt: cleanup. Ret tastefejl.

mutRate <- function(SFS){
  if(min(SFS)<0){
    stop('Entries in SFS must be non-negative')
  }
  res <- list()
  # Watterson's estimator (the numerator is the number of segregating sites):
  res$Watterson <- sum(SFS)/sum(1/c(1:length(SFS)))
  # The estimator based on the pairwise difference:
  res$pairwDiff <- pairwDiff(SFS)
  return(res)
}
