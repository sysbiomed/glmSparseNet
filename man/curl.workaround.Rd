% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/external_apis.R
\name{curl.workaround}
\alias{curl.workaround}
\title{Workaround for bug with curl when fetching specific ensembl mirror}
\usage{
curl.workaround(expr)
}
\arguments{
\item{expr}{expression}
}
\value{
result of expression
}
\description{
https://github.com/grimbough/biomaRt/issues/39
}
\examples{
\donttest{
glmSparseNet:::curl.workaround({
    biomaRt::useEnsembl(
        biomart = "genes", 
        dataset = 'hsapiens_gene_ensembl')
})
}
}
