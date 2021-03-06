% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{topline}
\alias{topline}
\title{Create a topline}
\usage{
topline(srv, other = NULL)
}
\arguments{
\item{srv}{A survey.}

\item{other}{The text column which contains the open response for 'other' in 
mainentity. Defaults to for instance \code{Q1a}, if mainentity is \code{Q1}.}
}
\value{
A list.
}
\description{
Simple function for \code{survey} objects, that procudes a count and mean per 
entity in an ongoing study, as well as open answers for respondents who have 
answered 'other'. It requires that both the measurement model and entities are
specified, and that the config and translations have been set.
}
\examples{
x <- topline(df)
}
\author{
Kristian D. Olsen
}

