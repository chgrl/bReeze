\encoding{UTF-8}
\name{set}
\alias{set}
\title{Creation of datasets}
\description{Creates a dataset object, by combining all signals of one height of measurement.}
\usage{
set(height, desc, v.avg, v.max, v.min, v.std, 
  dir.avg, dir.std, tmp, ...)
}
\arguments{
  \item{height}{Height of measurement in m as numeric value.}
  \item{desc}{Plain text information about the set, signals, instruments, etc. as string (optional).}
  \item{v.avg}{Average of wind speed within interval in m/s as numeric vector (optional, if at least one other signal is given).}
  \item{v.max}{Maximum of wind speed within interval in m/s as numeric vector (optional, if at least one other signal is given).}
  \item{v.min}{Minimum of wind speed within interval in m/s as numeric vector (optional, if at least one other signal is given).}
  \item{v.std}{Standard deviation of wind speed within interval in m/s as numeric vector (optional, if at least one other signal is given).}
  \item{dir.avg}{Average of wind direction within interval in degrees from north as numeric vector (optional, if at least one other signal is given).}
  \item{dir.std}{Standard deviation of wind direction within interval in degrees from north as numeric vector (optional, if at least one other signal is given).}
  \item{tmp}{Temperature in °C as numeric vector (optional, if at least one other signal is given).}
  \item{\dots}{Further signals, e.g. air pressure, humidity, etc. as numeric vector (optional).}
}
\details{
Anemometer and wind vanes are usually mounted as pairs at same or similar heights of the met mast. Signals with marginal differences in height, about 1 or 2 m, may or may not be combined to one dataset. In case of a combination of two heights the height of the anemometer should be used for the whole dataset, as in subsequent analyses wind speed might be extrapolated to other heights. A dataset shall not contain data from more than one sensor of the same type, say data of two anemometers.

A typical interval of wind measurments is 10 minutes, but also other intervals are applicable.

For datasets containing mean wind speed \code{v.avg} and its standard deviation within the time interval \code{v.std}, the turbulence intensity is calculated and added to the data, named \code{turb.int}. See \code{\link{turbulence}} for more details about turbulence intensity.
}
\value{
Returns a dataset object which is a list of:
  \item{height}{Height of measurement.}
  \item{data}{Data of measured signals.}
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
\dontrun{
# load data
data(winddata)

# minimal dataset
s <- set(height=40, v.avg=winddata[,2])

# detailed dataset
s2 <- set(height=40, desc=
  "C1: cup anemometer (SN: 4.3250.128), A1: wind vane (SN: 4.2800.205)", 
  v.avg=winddata[,2], v.max=winddata[,3], v.min=winddata[,4], 
  v.std=winddata[,5], dir.avg=winddata[,14], dir.std=winddata[,15])
}
}
\keyword{methods}
