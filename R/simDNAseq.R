#' Simulate DNA sequences
#'
#' @description
#' \code{simDNAseq} simulates a segregating sites matrix consisting of \code{n}
#' sequences based on the type of population size.
#'
#' @usage
#' simDNAseq(n, seqLen, mutRate, popType, ...)
#'
#' ## S3 method for class 'fixedPop'
#' simDNAseq(n, seqLen, mutRate, popType="fixedPop", expRate=NULL, expansionTime=NULL,
#'         proportion=NULL)
#'
#' ## S3 method for class 'varPop'
#' simDNAseq(n, seqLen, mutRate, popType="varPop", expRate, expansionTime=NULL,
#'         proportion=NULL)
#'
#' ## S3 method for class 'sudExpPop'
#' simDNAseq(n, seqLen, mutRate, popType="sudExpPop", expRate=NULL, expansionTime, proportion)
#'
#' @param n the sample size.
#' @param seqLen the length of the DNA sequences.
#' @param mutRate the mutation rate.
#' @param popType a string indicating which population type to simulate from.
#' Should be either '\code{fixedPop}', '\code{varPop}' or '\code{sudExpPop}'.
#' See details.
#' @param expRate the rate of the exponentially growing population, only used
#' when \code{popType} is '\code{varPop}'.
#' @param expansionTime parameter \code{b} determining the time of expansion.
#' @param proportion parameter \eqn{0< \alpha \le 1} determining the fraction of the population size before
#' the expansion.
#'
#' @details
#' If \code{popType} is '\code{fixedPop}' we simulate the branches of the ancestral
#' tree from a population of fixed size.
#'
#' If \code{popType} is '\code{varPop}' we simulate the branches of the ancestral tree
#' from an exponentially growing population (forwards in time). This means that the relative size function is
#' \deqn{f(x)=exp(-\lambdax),}
#' where \eqn{\lambda} is the rate of the exponentially growing population.
#'
#' If \code{popType} is '\code{sudExpPop}' we simulate the brances of the
#' ancestral tree from a suddenly expanded population. This means that backwards in
#' time the population size is \eqn{N} before the expansion and \eqn{\alphaN} after
#' the expansion. The decline in population size happened at time \eqn{bN}
#' (in generations) in the past.
#'
#' @return
#' \code{simDNAseq} returns a simulated segregating sites matrix.
#' That is a \code{n} times \code{seqLen} matrix consisting of zeroes and ones. Here the
#' number 1 indicates that a mutation occured at this specific site, and the number 0
#' indicates that no mutation occured.
#'
#' @examples
#' ## An example with fixed population size
#' simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop")
#'
#' ## An example with variable population size
#' simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
#'         expRate = 1.5)
#'
#' ## An example with suddenly expanded population size
#' simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
#'         proportion = 0.9)
#'
#' @references
#' Tavaré, S. (2004) \emph{Ancestral Inference in Population Genetics}.
#' Berlin: Springer-Verlag.
#'
#' Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
#' Roberts and Company Publishers.
#'
#' @export

simDNAseq <- function(n, seqLen, mutRate, expRate, expansionTime, proportion, popType){
  branchLen <- simBranch(n, seqLen, mutRate, expRate, expansionTime, proportion, popType)
  UseMethod("simDNAseq", branchLen)
}

simDNAseq.fixedPop <- function(n, seqLen, mutRate, popType="fixedPop", expRate=NULL,
                               expansionTime=NULL, proportion=NULL){
  branchLen <- simBranchFixed(n)
  res <- simSeq(branchLen, seqLen, mutRate)
  class(res) <- c(class(branchLen), class(res))
  return(res)
}

simDNAseq.varPop <- function(n, seqLen, mutRate, popType="varPop", expRate,
                               expansionTime=NULL, proportion=NULL){
  branchLen <- simBranchVar(n, expRate)
  res <- simSeq(branchLen, seqLen, mutRate)
  class(res) <- c(class(branchLen), class(res))
  return(res)
}


simDNAseq.sudExpPop <- function(n, seqLen, mutRate, popType="sudExpPop", expRate=NULL,
                               expansionTime, proportion){
  branchLen <- simBranchSudExpansion(n, expansionTime, proportion)
  res <- simSeq(branchLen, seqLen, mutRate)
  class(res) <- c(class(branchLen), class(res))
  return(res)
}

print.fixedPop <- function(x, ...){
  res <- x[1:nrow(x),1:ncol(x)]
  class(res) <- class(x)
  print.table(res)
}

print.varPop <- function(x, ...){
  res <- x[1:nrow(x),1:ncol(x)]
  class(res) <- class(x)
  print.table(res)
}

print.sudExpPop <- function(x, ...){
  res <- x[1:nrow(x),1:ncol(x)]
  class(res) <- class(x)
  print.table(res)
}

