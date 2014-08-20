\encoding{UTF-8}
\name{availability.int}
\alias{availability.int}
\title{Internal function for availability}
\description{Internal function for \code{\link{availability}}}
\usage{
availability.int(v.avg, dir.avg, ts, start.year, start.month, 
  num.months, period.days, digits)
}
\arguments{
  \item{v.set}{Dataset containing wind speed data to be used.}
  \item{dir.set}{Dataset containing wind direction data to be used.}
  \item{ts}{Time stamp}
  \item{start.year}{First year of measurement period.}
  \item{start.month}{First month of measurement period.}
  \item{num.months}{Number of months.}
  \item{period.days}{Days of measurement period.}
  \item{digits}{Number of digits.}
}
\value{Returns a list of:
  \item{total}{Total availability}
  \item{daily}{Daily availability}
}
\author{Christian Graul}
\seealso{\code{\link{availability}}}
\keyword{methods}
\keyword{internal}
