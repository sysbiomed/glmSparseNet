% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_db.R
\name{calculate.combined.score}
\alias{calculate.combined.score}
\title{Calculate combined score for STRINGdb interactions}
\usage{
calculate.combined.score(all.interactions, score_threshold, remove.text)
}
\arguments{
\item{all.interactions}{table with score of all interactions}

\item{score_threshold}{threshold to keep interactions}

\item{remove.text}{remove text-based interactions}
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
