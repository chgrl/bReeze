\encoding{UTF-8}
\name{monthStats}
\alias{monthStats}
\alias{ms}
\title{Calculation of monthly means}
\description{Calculates monthly statistics.}
\usage{
monthStats(mast, set, signal="v.avg", 
  fun=c("mean", "median", "min", "max", "sd"), 
  digits=3, print=TRUE)
ms(mast, set, signal="v.avg", 
  fun=c("mean", "median", "min", "max", "sd"), 
  digits=3, print=TRUE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for calculation specified as set number or set name. If missing, the calculation is carried out for all datasets that contain the specified \code{signal}.}
  \item{signal}{The signal to be used, as string value. Default is \code{"v.avg"}.}
  \item{fun}{Statistical function, as string value -- one of \code{"mean"} (arithmetic mean), \code{"median"}, \code{"min"} (minimum), \code{"max"} (maximum) or \code{"sd"} (standard deviation).}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
\code{monthStats} calculates statistics of valid data for each month and year in the measurement period. Usually this function is used for the calculation of average wind speeds. Means strongly depend on the measurement period and the number of samples. One important requirement for a reliable wind assessment is a measurement period covering the full seasonal cycle of variations.  A typical bias is a measurement limited to winter months, which usually results in overestimated wind speeds.
}
\value{Returns a list of data frames (one for each dataset) containing monthly, annual and total statistics of the specified signal.}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotMonthStats}}, \code{\link{printObject}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.max=winddata[,3])
set30 <- createSet(height=30, v.avg=winddata[,6], v.max=winddata[,7])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# calculate monthly means
neubuerg.stats <- monthStats(mast=neubuerg)

# calculate monthly means for one dataset
monthStats(mast=neubuerg, set=1)
monthStats(mast=neubuerg, set="set1")	# same as above

# calculate monthly means for another signal
monthStats(mast=neubuerg, signal="v.max")

# calculate monthly median, min, max and standard deviation
monthStats(mast=neubuerg, fun="median")
monthStats(mast=neubuerg, fun="min")
monthStats(mast=neubuerg, fun="max")
monthStats(mast=neubuerg, fun="sd")

# change number of digits and hide results
monthStats(mast=neubuerg, digits=2)
monthStats(mast=neubuerg, print=FALSE)
}
}
\keyword{methods}
