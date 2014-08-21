\encoding{UTF-8}
\name{mast}
\alias{mast}
\title{Creation of met mast objects}
\description{Creates met mast objects from one or more datasets (measurement heights). All datasets are sorted by height in descending order.}
\usage{
mast(timestamp, ..., loc=NULL, desc=NULL)
}
\arguments{
  \item{timestamp}{Time stamp as \code{POSIXlt} vector, e.g. created by \code{\link{timestamp}}.}
  \item{\dots}{At least one dataset created by \code{\link{set}}.}
  \item{loc}{Lat/lon coordinates of the site in decimal degrees as vector of two numeric values (optional).}
  \item{desc}{Plain text information about the site, measurement, met mast condition, etc. as string (optional).}
}
\details{
Valuable information about a met mast is usually provided by an installation protocol.

Measurements of wind speed in several heights is required for the computation of the site's wind profile. Met masts might have mounted more than one sensor of the same type at the same height. Thus, the data of a broken sensor can later be substituted by the 'backup-sensor'.
}
\value{
  Returns a met mast object, which is necessary for all data analyses. A met mast object is a list of:
  \item{timestamp}{Time stamp of the observations.}
  \item{location}{Lat/lon coordinates of the site (optional).}
  \item{description}{Mast information (optional).}
  \item{sets}{List of one or more datasets (measurement heights) consisting of height information and the data.}
}
\author{Christian Graul}
\seealso{\code{\link{POSIXlt}}, \code{\link{set}}, \code{\link{timestamp}}}
\examples{
\dontrun{
# load data, prepare sets and time stamp
data(winddata)
set40 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set20 <- set(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- timestamp(timestamp=winddata[,1])

# create met mast object
neubuerg <- mast(timestamp=ts, set40, set30, set20)

# create met mast with location and description
neubuerg.2 <- mast(timestamp=ts, set40, set30, set20, 
  loc=c(49.8909,11.4017), desc="Site #247 - Neubuerg")

# view structure of met mast
str(neubuerg.2)

# name sets
neubuerg.3 <- mast(timestamp=ts, C1.A1=set40, C2.A2=set30, 
  C3=set20, loc=c(49.8909,11.4017), desc="Site #247 - Neubuerg")

# compare names
names(neubuerg.2$sets)	# default names
names(neubuerg.3$sets)
}
}
\keyword{methods}
