\encoding{UTF-8}
\name{aep.int}
\alias{aep_int}
\title{Internal function for aep}
\description{Internal function for \code{\link{aep}}}
\arguments{
  \item{wb.par}{Weibull parameters A and k.}
  \item{lim}{Lower and upper calculation limit (wind speed) as numeric vector.}
  \item{pc}{Power curve object}
  \item{rho.pc}{Air desity the power curve is defined for.}
  \item{op}{Operational hours}
  \item{rho}{Air density}
  \item{avail}{Availability of turbine}
}
\value{Returns annual energy production as numeric value.}
\author{Christian Graul}
\seealso{\code{\link{aep}}}
\keyword{methods}
\keyword{internal}
