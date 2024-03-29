% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_DNAmat_class.R
\name{print.DNAmat_class}
\alias{print.DNAmat_class}
\title{Print DNA sequences}
\usage{
\method{print}{DNAmat_class}(DNAmat, ...)
}
\arguments{
\item{DNAmat}{either a segregating sites matrix or single-nucleotide polymorphism.
See \code{\link{SNP}} for explanation.}

\item{...}{arguments passed on from \code{\link[base]{print}}.}
}
\description{
Prints a segregating sites matrix.
}
\details{
It is used for printing the simulated segregating sites matrix from \code{simDNAseq}
and the SNP matrix from \code{SNP}.
}
\examples{
SegSitesMat <- matrix(c(1, 1, 1, 1, 0,
                        0, 1, 1, 1, 0,
                        0, 0, 0, 1, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0), byrow=T, nrow=5)
class(SegSitesMat) <- "DNAmat_class"
print(SegSitesMat)

}
