% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/external_apis.R
\name{ensemblGeneNames}
\alias{ensemblGeneNames}
\title{Retrieve ensembl gene names from biomaRt}
\usage{
ensemblGeneNames(
  geneId,
  useCache = TRUE,
  verbose = FALSE,
  gene.id = deprecated(),
  use.cache = deprecated()
)
}
\arguments{
\item{geneId}{character vector with gene names}

\item{useCache}{Boolean indicating if biomaRt cache should be used}

\item{verbose}{When using biomaRt in webservice mode and setting verbose to
TRUE, the XML query to the webservice will be printed.}

\item{gene.id}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}}

\item{use.cache}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}}
}
\value{
a dataframe with external gene names, ensembl_id
}
\description{
Retrieve ensembl gene names from biomaRt
}
\examples{
ensemblGeneNames(c("MOB1A", "RFLNB", "SPIC", "TP53"))
}
