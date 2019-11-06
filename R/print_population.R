#' Print DNA sequences
#'
#' @description
#' Prints a segregating sites matrix.
#'
#' @usage
#' ## S3 method for class 'population'
#' print.population(x, ...)
#'
#' @param x a segregating sites matrix. See details.
#'
#' @details
#' A segregating sites matrix consists of zeroes and ones.
#' Here the number 1 indicates that a mutation occured at this specific site, and
#' the number 0 indicates that no mutation occured.
#'
#' @examples
#' SegMat <- matrix(c(1, 1, 1, 1, 0,
#'                    0, 1, 1, 1, 0,
#'                    0, 0, 0, 1, 0,
#'                    0, 0, 0, 0, 1,
#'                    0, 0, 0, 0, 0), byrow=T, nrow=5)
#'class(SegMat) <- "population"
#'print(SegMat)
#'
#' @export

print.population <- function(x, ...){
  res <- x[1:nrow(x),1:ncol(x)]
  class(res) <- class(x)
  print.table(res)
}
