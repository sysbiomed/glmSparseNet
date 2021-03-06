% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centrality.R
\name{degreeSparsebn}
\alias{degreeSparsebn}
\title{Calculate degree of correlation matrix}
\usage{
degreeSparsebn(
  xdata,
  type = "continuous",
  levels = NULL,
  ivn = NULL,
  n = NULL,
  object = NULL,
  cutoff = 0,
  consider.unweighted = FALSE,
  n.cores = 1,
  show.message = FALSE,
  force.recalc.degree = FALSE,
  force.recalc.network = FALSE,
  ...
)
}
\arguments{
\item{xdata}{calculate correlation matrix on each column}

\item{type}{either "discrete" or "continuous",
see sparsebnUtils::sparsebnData}

\item{levels}{(optional) list of levels for each node.
see sparsebnUtils::sparsebnData}

\item{ivn}{(optional) list of interventions for each observation,
see sparsebnUtils::sparsebnData}

\item{n}{(optional) number of rows from data matrix to print,
see sparsebnUtils::sparsebnData}

\item{object}{(optional) an object of type sparsebnData,
see sparsebnUtils::sparsebnData}

\item{cutoff}{positive value that determines a cutoff value}

\item{consider.unweighted}{consider all edges as 1 if they are greater than 0}

\item{n.cores}{number of cores to be used}

\item{show.message}{shows cache operation messages}

\item{force.recalc.degree}{force recalculation, instead of going to cache}

\item{force.recalc.network}{force recalculation of network and penalty
weights, instead of going to cache}

\item{...}{parameters for sparsebn::estimate.dag}
}
\value{
a vector of the degrees
}
\description{
Calculate degree of correlation matrix
}
\examples{
# generate a random matrix of observations
xdata <- matrix(rnorm(1000), nrow = 20)
degreeSparsebn(xdata)
}
