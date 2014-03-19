\encoding{UTF-8}
\name{frequency}
\alias{frequency}
\alias{freq}
\title{Calculation of frequency and mean wind speed}
\description{Calculates the frequency of occurrence and mean wind speed per wind direction sector.}
\usage{
frequency(mast, v.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20), subset, digits=3, print=TRUE)
freq(mast, v.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20), subset, digits=3, print=TRUE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set used for wind speed, specified as set number or set name (optional, if \code{dir.set} is given).}
  \item{dir.set}{Set used for wind direction, specified as set number or set name (optional, if \code{v.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
The directional frequency of occurrence is an important factor for the design of a wind project. The influence is clear for the arrangement of turbines in a wind farm, that should be perpendicular to the most frequent wind direction. But also single turbines are affected, e.g. by fatigue tower loads in most frequent directions or in directions with highest wind speeds.
}
\value{Returns a data frame containing:
  \item{wind.speed}{Mean wind speed per direction sector.}
  \item{total}{Frequency per direction sector.}
  \item{\dots}{Frequencies per direction sector, for each given wind speed bin.}
}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotFrequency}}, \code{\link{printObject}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# calculate frequency
frequency(mast=neubuerg, v.set=1)

# if only one of v.set and dir.set is given, 
# the dataset is assigned to both
frequency(mast=neubuerg, v.set=1)
frequency(mast=neubuerg, dir.set=1)

# use different datasets for wind speed and direction
frequency(mast=neubuerg, v.set=3)	# no direction in dataset
frequency(mast=neubuerg, v.set=3, dir.set=2)
frequency(mast=neubuerg, v.set="set3", dir.set="set2")	# same as above

# change number of direction sectors 
frequency(mast=neubuerg, v.set=1, num.sectors=4)
frequency(mast=neubuerg, v.set=1, num.sectors=16)

# calculate frequency for 1 m/s speed bins and without binning
frequency(mast=neubuerg, v.set=1, bins=1:25)
frequency(mast=neubuerg, v.set=1, bins=0:25)	# same as above
frequency(mast=neubuerg, v.set=1, bins=NULL)

# data subsets
frequency(mast=neubuerg, v.set=1, 
  subset=c("2009-12-01 00:00:00", "2009-12-31 23:50:00"))
frequency(mast=neubuerg, v.set=1, 
  subset=c("2010-01-01 00:00:00", NA)) # just 'start' time stamp
frequency(mast=neubuerg, v.set=1, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# change number of digits and hide results
frequency(mast=neubuerg, v.set=1, digits=2)
frequency(mast=neubuerg, v.set=1, print=FALSE)
}
}
\keyword{methods}
