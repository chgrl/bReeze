\encoding{UTF-8}
\name{availability.int}
\title{Internal function for availability}
\description{Internal function for \code{\link{availability}}}
\arguments{
  \item{v.avg}{Average wind speed.}
  \item{dir.avg}{Average wind direction.}
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
