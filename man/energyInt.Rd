\encoding{UTF-8}
\name{energyInt}
\alias{energyInt}
\title{Internal function for energy}
\description{Internal function for \code{\link{energy}}}
\usage{energyInt(lim, k, A, rho)}
\arguments{
  \item{lim}{Lower and upper calculation limit (wind speed) as numeric vector.}
  \item{k}{Shape parameter of the weibull distribution.}
  \item{A}{Scale parameter of the weibull distribution.}
  \item{rho}{Air density}
}
\value{Returns energy as numeric value.}
\author{Christian Graul}
\seealso{\code{\link{energy}}}
\keyword{methods}
\keyword{internal}
