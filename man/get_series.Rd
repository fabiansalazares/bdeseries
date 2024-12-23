% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_series.R
\name{get_series}
\alias{get_series}
\title{Get series in the Banco de España databas}
\usage{
get_series(codes, verbose = FALSE, usefulldatabase = FALSE)
}
\arguments{
\item{codes}{series' code(s) to be downloaded}
}
\value{
This function returns a tibble containing the series that match the given series' code(s)
}
\description{
Get series in the Banco de España databas
}
\examples{
get_series("")
}
\keyword{download}
\keyword{get}
\keyword{series}
