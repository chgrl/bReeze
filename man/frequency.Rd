\encoding{UTF-8}
\name{frequency}
\alias{frequency}
\alias{freq}
\alias{plot.frequency}
\alias{print.frequency}
\title{Calculation of frequency and mean wind speed}
\description{Calculates the frequency of occurrence and mean wind speed per wind direction sector.}
\usage{
frequency(mast, v.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20), subset, digits=3, print=TRUE)
freq(mast, v.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20), subset, digits=3, print=TRUE)

\method{plot}{frequency}(x, ...)
\method{print}{frequency}(x, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set used for wind speed, specified as set number or set name (optional, if \code{dir.set} is given).}
  \item{dir.set}{Set used for wind direction, specified as set number or set name (optional, if \code{v.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired. Default is \code{c(5, 10, 15, 20)}.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE} (the default), results are printed directly.}
  
  \item{x}{Frequency object, created by \code{frequency}.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
The directional frequency of occurrence is an important factor for the design of a wind project. The influence is clear for the arrangement of turbines in a wind farm, that should be perpendicular to the most frequent wind direction. But also single turbines are affected, e.g. by fatigue tower loads in most frequent directions or in directions with highest wind speeds.
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{border.leg}: Border colour(s) for the legend. One colour for each wind speed bin or a single colour -- default is same as \code{col}.
	\item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
    \item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{circles}: Manual definition of circles to be drawn, as numeric vector of the form c(inner circle, outer circle, interval between the circles).
    \item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{frequency} only contains the total frequency per direction sector.
    \item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"gray45"}.
    \item \code{col.border}: Colour to be used for sector borders -- default is \code{NULL} (no border is drawn).
    \item \code{col.circle}: Colour to be used for circles -- default is \code{"gray45"}.
    \item \code{col.cross}: Colour to be used for axis lines -- default is \code{"gray45"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{fg}: If \code{TRUE}, sectors are plotted in foreground (on top of axis lines and circles) -- default is \code{FALSE}.
    \item \code{lty.circle}: Line type of circles -- default is \code{"dashed"}. See \code{\link{par}} for available line types.
    \item \code{lty.cross}: Line type of axis lines -- default is \code{"solid"}. See \code{\link{par}} for available line types.
    \item \code{lwd.border}: Line width of the sector borders -- default is \code{0.5}. Only used if \code{col.border} is set.
    \item \code{lwd.circle}: Line width of circles, as numeric value -- default is \code{0.7}.
    \item \code{lwd.cross}: Line width of axis lines, as numeric value -- default is \code{0.7}.
    \item \code{pos.axis}: Position of axis labels in degree, as numeric value -- default is \code{60}.
    \item \code{sec.space}: Space between plotted sectors, as numeric value between \code{0} and \code{1} -- default is \code{0.2}.
    \item \code{title.leg}: Alternative legend title, as string.
    \item \code{width.leg}: Widths of legend space relative to plot space, as numeric value between \code{0} and \code{1}. If \code{0}, the legend is omitted, default value is \code{0.15}.
    \item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.4}.
  }
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
\seealso{\code{\link{createMast}}}
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
