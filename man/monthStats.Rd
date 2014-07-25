\encoding{UTF-8}
\name{monthStats}
\alias{monthStats}
\alias{ms}
\alias{plot.monthStats}
\title{Calculation of monthly statistics}
\description{Calculates monthly statistics.}
\usage{
monthStats(mast, set, signal="v.avg", 
  fun=c("mean", "median", "min", "max", "sd"), subset, 
  digits=3, print=TRUE)
ms(mast, set, signal="v.avg", 
  fun=c("mean", "median", "min", "max", "sd"), subset, 
  digits=3, print=TRUE)

\method{plot}{monthStats}(x, set, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for calculation or plotting specified as set number or set name. If missing, the calculation or plotting is carried out for all datasets that contain the specified \code{signal}.}
  \item{signal}{The signal to be used, as string value. Default is \code{"v.avg"}.}
  \item{fun}{Statistical function, as string value -- one of \code{"mean"} (arithmetic mean), \code{"median"}, \code{"min"} (minimum), \code{"max"} (maximum) or \code{"sd"} (standard deviation).}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE} (the default), results are printed directly.}
      
  \item{x}{Monthly statistics object created by \code{monthStats}.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
\code{monthStats} calculates statistics of valid data for each month and year in the measurement period. Usually this function is used for the calculation of average wind speeds. Means strongly depend on the measurement period and the number of samples. One important requirement for a reliable wind assessment is a measurement period covering the full seasonal cycle of variations.  A typical bias is a measurement limited to winter months, which usually results in overestimated wind speeds.
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{    
    \item \code{border}: Colour to be used for the border of the bars -- default value is \code{"black"}. Use \code{border=NA} to omit borders.
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box. 
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Vector of colours, one for each year in the measurement period.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot (only for plots with one dataset) -- default is \code{c(4, 5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.5, 1, 0)}.
    \item \code{plot.names}: If \code{TRUE} (the default), the names of the datasets is plotted as second label of the y axis.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{top}, \code{"topright"}, \code{"right"} or \code{"center"}.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
  }
}
\value{Returns a list of data frames (one for each dataset) containing monthly, annual and total statistics of the specified signal.}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
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

# data subsets
monthStats(mast=neubuerg, 
  subset=c("2009-12-01 00:10:00", "2009-12-31 23:50:00"))
monthStats(mast=neubuerg, 
  subset=c("2010-01-01 00:10:00", NA)) # just 'start' time stamp
monthStats(mast=neubuerg, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# change number of digits and hide results
monthStats(mast=neubuerg, digits=2)
monthStats(mast=neubuerg, print=FALSE)
}
}
\keyword{methods}
