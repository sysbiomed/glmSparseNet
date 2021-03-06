% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centrality.R
\name{.networkGenericParallel}
\alias{.networkGenericParallel}
\title{Calculate the upper triu of the matrix}
\usage{
.networkGenericParallel(
  fun,
  fun.prefix,
  xdata,
  build.output = "matrix",
  n.cores = 1,
  force.recalc.network = FALSE,
  show.message = FALSE,
  ...
)
}
\arguments{
\item{fun}{function that will calculate the edge weight between 2 nodes}

\item{fun.prefix}{used to store low-level information on network as it can
become to large to be stored in memory}

\item{xdata}{base data to calculate network}

\item{build.output}{if output returns a 'matrix', 'vector' of the upper triu
without the diagonal or NULL with any other argument}

\item{n.cores}{number of cores to be used}

\item{force.recalc.network}{force recalculation, instead of going to cache}

\item{show.message}{shows cache operation messages}

\item{...}{extra parameters for fun}
}
\value{
depends on build.output parameter
}
\description{
Calculate the upper triu of the matrix
}
