% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_db.R
\name{stringDBhomoSapiens}
\alias{stringDBhomoSapiens}
\title{Download protein-protein interactions from STRING DB}
\usage{
stringDBhomoSapiens(version = "11.0", score_threshold = 0, remove.text = TRUE)
}
\arguments{
\item{version}{version of the database to use}

\item{score_threshold}{remove scores below threshold}

\item{remove.text}{remove text mining-based scores}
}
\value{
a data.frame with rows representing an interaction between two
proteins, and columns
the count of scores above the given score_threshold
}
\description{
Download protein-protein interactions from STRING DB
}
\examples{
\donttest{
    stringDBhomoSapiens(score_threshold = 800)
}
}
