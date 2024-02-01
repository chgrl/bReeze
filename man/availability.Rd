\encoding{UTF-8}
\name{availability}
\alias{availability}
\alias{avail}
\alias{plot.availability}
\title{Calculate availability for pairs of wind speed and direction}
\description{Calculates the availability for pairs of wind speed and wind direction of a met mast.}
\usage{
availability(mast, v.set, dir.set, subset, digits=1, print=TRUE)
avail(mast, v.set, dir.set, subset, digits=1, print=TRUE)

\method{plot}{availability}(x, set, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{mast}}.}
  \item{v.set}{Set(s) to be used for wind speed, specified as set number(s) or set name(s). Argument is optional -- if missing, all sets containing wind speed and wind direction data are used.}
  \item{dir.set}{Set(s) to be used for wind direction, specified as set number(s) or set name(s). Argument is optional -- if missing, the set(s) used for wind speed data will also be used for wind direction.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{1}.}
  \item{print}{If \code{TRUE} (the default), results are printed directly.}
      
  \item{x}{Availability object created by \code{availability}.}
  \item{set}{Number of dataset to be plotted, specified as set number or set name (optional).}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
The availability is the number of pairs of valid wind speed and valid wind direction data samples as a percentage of the total possible for the measurement period, i.e.:
\deqn{Availability = \frac{N(v_{valid} \wedge dir_{valid})}{N}}{Availability = N(v_valid & dir_valid) / N}
where \eqn{N} is the total possible number of samples and \eqn{v_{valid} \wedge dir_{valid}}{v_valid & dir_valid} is a pair of valid wind speed and direction data.

Causes of invalid or missing data are manifold. Typical causes are icing and defects of sensors or other measurement equipment.
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{border}: Colour of the cell borders -- default value is \code{"black"}.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{col}: Vector of three text colours (or just one colour), where the first indicates 'full availability', the second 'partial availability' and the third 'not available' -- default is \code{"black"}.
	\item \code{col.axis}: Colour to be used for axis -- default is \code{"gray45"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{fill}: Vector of three fill colours (or just one colour), where the first indicates 'full availability', the second 'partial availability' and the third 'not available'.
    \item \code{lwd}: Line widths of the cell borders. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(0, 0, 0, 0)}.
    \item \code{plot.names}: If \code{TRUE} (the default), the names of the datasets is plotted as second label of the y axis.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
  }
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
\seealso{\code{\link{mast}}, \code{\link{clean}}}
\examples{
\dontrun{
## load and prepare data
data("winddata", package="bReeze")
set40 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set20 <- set(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- mast(timestamp=ts, set40=set40, set30=set30, 
  set20=set20)
neubuerg <- clean(mast=neubuerg)


## calculate availability
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

# change number of digits and hide results
neubuerg.avail <- availability(mast=neubuerg, digits=0)
neubuerg.avail <- availability(mast=neubuerg, print=FALSE)
neubuerg.avail


## plot availability objects
plot(neubuerg.avail)  # default
plot(neubuerg.avail, set=2)	 # one dataset
plot(neubuerg.avail, set="set30")  # same as above
plot(neubuerg.avail, set=c(1,2))  # several datasets

# customize plot
plot(neubuerg.avail, border="darkgray", cex.axis=0.7, 
  cex.lab=0.9, col=c("darkgreen", "orange", "red4"), col.axis="blue", 
  col.lab="blue", fill=c("lightgreen", "yellow", "red"), lwd=0.5, 
  mar=c(1,1,1,1), plot.names=FALSE, xlab="jour", ylab="mois")
}
}
\keyword{methods}
