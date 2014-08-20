\encoding{UTF-8}
\name{clean.int}
\alias{clean.int}
\title{Internal function for clean}
\description{Internal function for \code{\link{clean}}}
\usage{clean.int(dat, v.avg.min, v.avg.max, dir.clean, turb.clean, icing, rep, n.rep)}
\arguments{
  \item{dat}{Dataset to be cleaned.}
  \item{v.avg.min}{Lower limit for wind speeds as numeric value. Default is \code{0.4} m/s.}
  \item{v.avg.max}{Upper limit for wind speeds as numeric value. Default is \code{50} m/s.}
  \item{dir.clean}{If \code{TRUE}, faulty wind direction values are excluded. Faulty values are \code{dir.avg}<0, \code{dir.avg}>360 and \code{dir.avg}, where the wind speed is lower than the \code{v.avg.min} specified. Default is \code{TRUE}.}
  \item{turb.clean}{Wind speed limit for turbulence intensity. Turbulence intesity values are excluded for wind speeds lower then this limit. Default is 4 m/s.}
  \item{icing}{If \code{TRUE}, wind direction values are excluded, where standard deviation of wind direction is 0, assuming icing. Default is \code{FALSE}.}
  \item{rep}{Signal (or a vector of signals), for which repetitions shall be cleaned.}
  \item{n.rep}{Number of repetitions that shall be cleaned, as integer value -- default is \code{5}.}
}
\value{Returns cleaned dataset.}
\author{Christian Graul}
\seealso{\code{\link{clean}}}
\keyword{methods}
\keyword{internal}