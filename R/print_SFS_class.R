#' Print site frequency spectrum
#'
#' @description
#' Prints a vector with the site frequency spectrum.
#'
#' @param SFS a vector with the site frequency spectrum.
#' @param ...
#' arguments passed on from \code{\link[base]{print}}.
#'
#' @details
#' It is used for printing the site frequency spectrum from \code{SFS}.
#'
#' @examples
#' SFSvec <- c(4, 1, 2, 0, 0)
#'class(SFSvec) <- "SFS_class"
#'print(SFSvec)
#'
#' @export

print.SFS_class <- function(SFS, ...){
  res <- SFS[1:length(SFS)]
  class(res) <- class(SFS)
  print.table(res)
}
