% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simDNAseq.R
\name{simDNAseq}
\alias{simDNAseq}
\title{Simulate DNA sequences}
\usage{
simDNAseq(n, seqLen, mutRate, popType, ...)

## S3 method for class 'fixedPop'
simDNAseq(n, seqLen, mutRate, popType="fixedPop", expRate=NULL,
        expansionTime=NULL, proportion=NULL)

## S3 method for class 'varPop'
simDNAseq(n, seqLen, mutRate, popType="varPop", expRate, expansionTime=NULL,
        proportion=NULL)

## S3 method for class 'sudExpPop'
simDNAseq(n, seqLen, mutRate, popType="sudExpPop", expRate=NULL,
        expansionTime, proportion)
}
\arguments{
\item{n}{the sample size.}

\item{seqLen}{the length of the DNA sequences.}

\item{mutRate}{the mutation rate.}

\item{popType}{a string indicating which population type to simulate from.
Should be either '\code{fixedPop}', '\code{varPop}' or '\code{sudExpPop}'.
See details.}

\item{expRate}{the rate of the exponentially growing population, only used
when \code{popType} is '\code{varPop}'.}

\item{expansionTime}{parameter \code{b} determining the time of expansion.}

\item{proportion}{parameter \eqn{0< \alpha \le 1} determining the fraction of the population size before
the expansion.}
}
\value{
\code{simDNAseq} returns a simulated \code{n} times \code{seqLen} segregating
sites matrix. See details.
}
\description{
\code{simDNAseq} simulates a segregating sites matrix consisting of \code{n}
sequences based on how the population size develops.
}
\details{
If \code{popType} is '\code{fixedPop}' we simulate the branches of the ancestral
tree from a population of fixed size.

If \code{popType} is '\code{varPop}' we simulate the branches of the ancestral tree
from an exponentially growing population (forwards in time). This means that the relative size function is
\deqn{f(x)=exp(-\lambdax),}
where \eqn{\lambda} is the rate of the exponentially growing population.

If \code{popType} is '\code{sudExpPop}' we simulate the brances of the
ancestral tree from a suddenly expanded population. This means that backwards in
time the population size is \eqn{N} before the expansion and \eqn{\alphaN} after
the expansion. The decline in population size happened at time \eqn{bN}
(in generations) in the past.

The output is a segregating sites matrix; that is a matrix consisting of zeroes
and ones. Here the number 1 indicates that a mutation occured at this specific
site, and the number 0 indicates that no mutation occured.
}
\examples{
## An example with fixed population size
simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop")

## An example with variable population size
simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
        expRate = 1.5)

## An example with suddenly expanded population size
simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop",
        expansionTime = 2, proportion = 0.9)

}
\references{
Tavaré, S. (2004) \emph{Ancestral Inference in Population Genetics}.
Berlin: Springer-Verlag.

Wakeley J. (2009) \emph{Coalescent Theory: An Introduction}. Colorado:
Roberts and Company Publishers.
}
