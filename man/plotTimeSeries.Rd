\encoding{UTF-8}
\name{plotTimeSeries}
\alias{plotTimeSeries}
\alias{plts}
\title{Plot time series of met mast data}
\description{Plots time series of one or more signals of a given met mast object.}
\usage{
plotTimeSeries(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), 
  subset, ...)
plts(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), 
  subset, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for plotting specified as set number or set name. Argument is optional -- if missing, all sets containing the given \code{signal}(s) are used.}
  \item{signal}{Signal(s) to be plotted as vector of strings giving the signal names.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55".}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box. 
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Vector of colours, one for each set plotted.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{lty}: Vector of line types -- one for each set plotted. See \code{\link{par}} for available line types.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot (only for plots with one dataset) -- default is \code{c(1, 4.5, 0, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.5, 0.7, 0)}.
	\item \code{ylab}: String vector of labels for the y axis.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], v.max=winddata[,3], 
  v.min=winddata[,4], v.std=winddata[,5], dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9], 
  dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# plot time series
plotTimeSeries(mast=neubuerg)
plotTimeSeries(mast=neubuerg, set=1, legend=FALSE)
plotTimeSeries(mast=neubuerg, set="set1", legend=FALSE)	# same as above

# change signals 
plotTimeSeries(mast=neubuerg, signal=c("v.avg", "v.min", "v.max", 
  "v.std"))
plotTimeSeries(mast=neubuerg, signal=c("turb.int"))
plotTimeSeries(mast=neubuerg, set=1, signal=c("v.avg", "dir.avg"))

# change time scale
plotTimeSeries(mast=neubuerg, subset=c("2010-01-01 00:10:00", NA))
plotTimeSeries(mast=neubuerg, subset=c("2009-10-11 00:10:00", 
  "2009-10-11 23:50:00"))
plotTimeSeries(mast=neubuerg, set=1, signal="v.max", 
  subset=c(NA, "2009-12-27 18:30:00"))

# customize plot
plotTimeSeries(mast=neubuerg, bty="n", bty.leg="o", cex.axis=1.2, 
  cex.lab=1.4, cex.leg=1.2, col=c("darkblue", "red2", "darkgreen"), 
  col.axis="darkgray", col.lab="darkgray", col.leg="darkgray", 
  col.ticks="darkgray", las=0, lty=rep(1.5,3), mar=c(0.5,4,0,0.5), 
  mgp=c(2,0.5,0), ylab=c("v [m/s]","dir [deg]","ti [-]"), x.intersp=1)
}
}
\keyword{methods}
