% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_contrib.R
\name{cv.glmOrphan}
\alias{cv.glmOrphan}
\title{GLMNET cross-validation model penalizing nodes with high degree}
\usage{
cv.glmOrphan(xdata, ydata, network, network.options = networkOptions(), ...)
}
\arguments{
\item{xdata}{input data, can be a matrix or MultiAssayExperiment}

\item{ydata}{response data compatible with glmnet}

\item{network}{type of network, see below}

\item{network.options}{options to calculate network}

\item{...}{parameters that glmnet accepts}
}
\value{
see cv.glmSparseNet
}
\description{
This function overrides the `trans.fun` options in `network.options` with
an heuristic described in Veríssimo et al. that penalizes nodes with high
degree.
}
\examples{
xdata <- matrix(rnorm(100), ncol = 5)
cv.glmOrphan(xdata, rnorm(nrow(xdata)), 'correlation',
             family = 'gaussian',
             nfolds = 5,
             network.options = networkOptions(min.degree = .2))
}
\seealso{
glmNetSparse
}
