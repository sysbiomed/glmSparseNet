% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_db.R
\name{.combinedScore}
\alias{.combinedScore}
\title{Calculate combined score for STRINGdb interactions}
\usage{
.combinedScore(allInteractions, scoreThreshold, removeText)
}
\arguments{
\item{allInteractions}{table with score of all interactions}

\item{scoreThreshold}{threshold to keep interactions}

\item{removeText}{remove text-based interactions}
}
\value{
table with combined score
}
\description{
Please note that all the interactions have duplicates as it's
a two way interaction (score(ProteinA-Protein) == score(ProteinB, PorteinA))
}
\details{
To better understand how the score is calculated, please see:
https://string-db.org/help/faq/#how-are-the-scores-computed
}
