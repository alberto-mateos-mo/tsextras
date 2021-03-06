% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_season.R
\name{ts_season}
\alias{ts_season}
\title{Seasonality Extraction function}
\usage{
ts_season(x, plot = TRUE, random = TRUE)
}
\arguments{
\item{x}{A list returned by ts_trend().}

\item{plot}{Whether to plot or not the seasonal component. Defaults to TRUE.}

\item{random}{Whether to calculate random component or not}
}
\description{
Extracts and plot the seasonality component of a time series
}
\examples{
 s <- AirPassengers
 x <- ts_trend(s)
 ts_season(x)
}
\seealso{
decompose().
}
\author{
David Alberto Mateos Montes de Oca.
}
\keyword{seasonality.}
\keyword{timeseries,}
\keyword{ts,}
