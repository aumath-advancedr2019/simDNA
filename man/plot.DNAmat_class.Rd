% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_DNAmat_class.R
\name{plot.DNAmat_class}
\alias{plot.DNAmat_class}
\title{Site frequency spectrum plot}
\usage{
\method{plot}{DNAmat_class}(DNAmat, xlab = "Index i",
  ylab = "Number of i-tons", main = "Site frequency spectrum", ...)
}
\arguments{
\item{DNAmat}{matrix; either a single-nucleotide polymorphism matrix or a segregating
sites matrix of class 'DNAmat_class'. See \code{\link{SNP}} for explanation.}

\item{xlab}{a title for the x axis. Is by default "Index i". See \code{\link[graphics]{title}}.}

\item{ylab}{a title for the y axis. Is by default "Number of i-tons". See \code{\link[graphics]{title}}.}

\item{main}{an overall title for the plot. Is by default "Site frequency spectrum". See \code{\link[graphics]{title}}.}

\item{...}{arguments passed on from \code{\link[graphics]{plot}}.}
}
\description{
Plots the site frequency spectrum.
}
\details{
The terminology "Number of i-tons" corresponds to the i'th entry in the site
frequency spectrum. See \code{\link{SFS}} for further explanation.
}
\examples{
# Using the function simDNAseq:
plot(simDNAseq(n = 8, seqLen = 20, mutRate = 5,
               popType = "varPop", expRate = 1.5), col="red")


# Creating a segregating sites matrix by hand:
SegSitesMat <- matrix(sample(x=c(0, 1), size=26*12,
                      replace=TRUE, prob=c(0.9, 0.1)),
                      nrow=12, ncol=26, byrow=TRUE)
class(SegSitesMat) <- "DNAmat_class"
plot(SegSitesMat)

}
