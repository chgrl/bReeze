\encoding{UTF-8}
\name{printObject}
\alias{printObject}
\alias{probj}
\title{Print method for bReeze objects}
\description{Summarizes information about a bReeze object.}
\usage{
printObject(object)
probj(object)
}
\arguments{
  \item{object}{bReeze object (from \code{\link{aep}}, \code{\link{availability}}, \code{\link{createMast}}, \code{\link{createSet}}, 
  \code{\link{createPC}}, \code{\link{energy}}, \code{\link{frequency}}, \code{\link{monthStats}}, \code{\link{profile}}, \code{\link{readPC}}, 
  \code{\link{turbulence}}, \code{\link{uncertainty}} or \code{\link{weibull}}).}
}
\value{For met mast objects a list is returned:
  \item{location}{Lat/lon coordinates of the site (if available).}
  \item{description}{Plain text description of the met mast (if available).}
  \item{num.sets}{Number of datasets.}
  \item{heights}{List of measuring heights of the datasets.}
  \item{signals}{List of signals of each dataset.}
  \item{period}{List of Starting time stamp, end time stamp and duration of the measuring period.}
  \item{num.samples}{Number of samples.}
  \item{wind.speed}{List of average wind speed of the datasets (if available).}
  \item{availability}{List of the availability of the datasets (if available).}
  \item{cleaned}{List of the cleaning status of the datasets.}
  
  Other objects are just printed without return value.
}
\author{Christian Graul}
\seealso{\code{\link{availability}}, \code{\link{createMast}}, \code{\link{createSet}}, \code{\link{createPC}}, \code{\link{frequency}}, \code{\link{monthStats}}, \code{\link{readPC}}}
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
  set20=set20, loc=c(49.8909,11.4017), desc="Site #247 - Neubuerg")
pw.56 <- readPC(file="PowerWind_56_900kW.wtg")

# print met masts
printObject(object=neubuerg)

# for met masts a list is returned
neubuerg.detail <- printObject(object=neubuerg)
neubuerg.detail

# print power curve
printObject(object=pw.56)
}
}
\keyword{methods}
