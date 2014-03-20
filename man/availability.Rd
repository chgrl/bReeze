\encoding{UTF-8}
\name{availability}
\alias{availability}
\alias{avail}
\title{Calculate availability for pairs of wind speed and direction}
\description{Calculates the availability for pairs of wind speed and wind direction of a met mast.}
\usage{
availability(mast, v.set, dir.set, subset, digits=1, print=TRUE)
avail(mast, v.set, dir.set, subset, digits=1, print=TRUE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set(s) to be used for wind speed, specified as set number(s) or set name(s). Argument is optional -- if missing, all sets containing wind speed and wind direction data are used.}
  \item{dir.set}{Set(s) to be used for wind direction, specified as set number(s) or set name(s). Argument is optional -- if missing, the set(s) used for wind speed data will also be used for wind direction.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{1}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
The availability is the number of pairs of valid wind speed and valid wind direction data samples as a percentage of the total possible for the measurement period, i.e.:
\deqn{Availability = \frac{N(v_{valid} \wedge dir_{valid})}{N}}{Availability = N(v_valid & dir_valid) / N}
where \eqn{N} is the total possible number of samples and \eqn{v_{valid} \wedge dir_{valid}}{v_valid & dir_valid} is a pair of valid wind speed and direction data.

Causes of invalid or missing data are manifold. Typical causes are icing and defects of sensors or other measurement equipment.
}
\value{
  Returns a list of sub lists for each pair of \code{v.set}/\code{dir.set} containing:
  \item{total}{Total availability (\%), effective period (days) and total period (days) of a set/pair of wind speed and direction.}
  \item{daily}{Availability per day, i.e. number of samples per day.}
}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{clean}}, \code{\link{plotAvailability}}, \code{\link{printObject}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set40=set40, set30=set30, 
  set20=set20)
neubuerg <- clean(mast=neubuerg)

# calculate availability
neubuerg.avail <- availability(mast=neubuerg)

# compare the total availability of 'set40' and 'set30'
neubuerg.avail$set40$total
neubuerg.avail$set30$total

# check the daily availability of 'set40'
neubuerg.avail$set40$daily

# note: availability for 'set20' is missing - its availability is NULL,
# since it does not contain wind direction data

# calculate availability for 'set20' using wind direction data from 'set40'
set20.avail <- availability(mast=neubuerg, v.set=3, dir.set=1)
# same as above
set20.avail <- availability(mast=neubuerg, v.set="set20", dir.set="set40")

# calculate availability for all sets using wind direction data from 'set40'
neubuerg.avail <- availability(mast=neubuerg, v.set=c(1,2,3), dir.set=1)

# data subsets
availability(mast=neubuerg, v.set=1, 
  subset=c("2009-12-01 00:10:00", "2009-12-31 23:50:00"))
availability(mast=neubuerg, v.set=1, 
  subset=c("2010-01-01 00:10:00", NA)) # just 'start' time stamp
availability(mast=neubuerg, v.set=1, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp
}
}
\keyword{methods}
