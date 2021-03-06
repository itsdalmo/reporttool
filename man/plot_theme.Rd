% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{plot_theme}
\alias{plot_shared_legend}
\alias{plot_theme}
\title{ggplot2 theme}
\usage{
plot_theme(legend = "bottom")

plot_shared_legend(...)
}
\arguments{
\item{legend}{Default is bottom. Passed directly to theme call for ggplot2.}
}
\value{
A ggplot2 theme object
}
\description{
Wrapper to make styling plots for a report less verbose.
}
\examples{
p <- ggplot(df, aes(x=manifest, y=score, group=mainentity)) + geom_bar()
p + plot_theme()
}
\author{
Kristian D. Olsen
}

