#' Simulate DNA sequences
#'
#' @description
#' \code{simDNAseq} simulates a segregating sites matrix consisting of \code{n}
#' sequences based on \code{popType}.
#'
#' @usage
#' simDNAseq(n, seqLen, mutRate, popType, ...)
#'
#' ## S3 method for class 'fixedPop'
#' simDNAseq(n, seqLen, mutRate, expRate=NULL, expansionTime=NULL,
#'         proportion=NULL)
#'
#' ## S3 method for class 'varPop'
#' simDNAseq(n, seqLen, mutRate, expRate, expansionTime=NULL,
#'         proportion=NULL)
#'
#' ## S3 method for class 'sudExpansionPop'
#' simDNAseq(n, seqLen, mutRate, expRate=NULL, expansionTime, proportion)
#'
#' @param n the sample size.
#' @param seqLen the length of the DNA sequences.
#' @param mutRate the mutation rate.
#' @param popType a string indicating which population type to simulate from.
#' Should be either '\code{fixedPop}', '\code{varPop}' or '\code{sudExpansionPop}'.
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
#' If \code{popType} is '\code{sudExpansionPop}' we simulate the brances of the
#' ancestral tree from a suddenly expanded population. This means that backwards in
#' time the population size is \eqn{N} before the expansion and \eqn{\alphaN} after
#' the expansion. The decline in population size happened at time \eqn{bN}
#' (in generations) in the past.
#'
#' @return
#' \code{simDNAseq} returns a simulated segregating sites matrix.
#' That is a \code{n} times \code{seqLen} matrix consisting of zeroes and ones, where the
#' number 1 indicates that a mutation occured at this specific site.
#'
#' @references
#' Tavaré, S. (2004) \emph{Ancestral Inference in Population Genetics}.
#' Berlin: Springer-Verlag.
#'
#' Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
#' Roberts and Company Publishers.
#'
#' @examples
#' ## An example with fixed population size
#' simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop")
#'
#' ## An example with variable population size
#' simDNAseq(n = 8, seqLen = 20, mutRate = 0.5, popType = "varPop",
#'         expRate = 1.5)
#'
#' ## An example with suddenly expanded population size
#' simDNAseq(n = 25, seqLen = 30, mutRate = 1.5, expansionTime = 50,
#'         proportion = 0.9)
#'
#' @export

simDNAseq <- function(n, seqLen, mutRate, expRate, expansionTime, proportion, popType){
  branchLen <- simBranch(n, seqLen, mutRate, expRate, expansionTime, proportion, popType)
  UseMethod("simDNAseq", branchLen)
}

simDNAseq.fixedPop <- function(n, seqLen, mutRate, expRate=NULL,
                               expansionTime=NULL, proportion=NULL){
  branchLen <- simBranchFixed(n) #might not be necessary
  return(simSeq(branchLen, seqLen, mutRate))
  #should probably return the SNP matrix instead
}

simDNAseq.varPop <- function(n, seqLen, mutRate, expRate,
                               expansionTime=NULL, proportion=NULL){
  branchLen <- simBranchVar(n, expRate) #might not be necessary
  return(simSeq(branchLen, seqLen, mutRate))
  #should probably return the SNP matrix instead
}


simDNAseq.sudExpansionPop <- function(n, seqLen, mutRate, expRate=NULL,
                               expansionTime, proportion){
  branchLen <- simBranchSudExpansion(n, expanstionTime, proportion) #might not be necessary
  return(simSeq(branchLen, seqLen, mutRate))
  #should probably return the SNP matrix instead
}

