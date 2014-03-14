\encoding{UTF-8}
\name{plotMonthStats}
\alias{plotMonthStats}
\alias{plms}
\title{Plot monthly means}
\description{Plots monthly means of wind speed from a \code{\link{monthStats}} object.}
\usage{
plotMonthStats(stats, set, ...)
plms(stats, set, ...)
}
\arguments{
  \item{stats}{Monthly means created by \code{\link{monthStats}}.}
  \item{set}{Set used for plotting specified as set number or set name. Argument is optional -- if missing, all sets in \code{stats} are plotted.}
  \item{\dots}{Optional graphical parameters, see below for details.}
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
\author{Christian Graul}
\seealso{\code{\link{monthStats}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2])
set30 <- createSet(height=30, v.avg=winddata[,6])
set20 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# calculate stats
neubuerg.stats <- monthStats(mast=neubuerg, print=FALSE)

# plot all datasets
plotMonthStats(stats=neubuerg.stats)

# plot one dataset
plotMonthStats(stats=neubuerg.stats, set=1)
plotMonthStats(stats=neubuerg.stats, set="set1")	# same as above

# customize plot
plotMonthStats(stats=neubuerg.stats, border="darkgray", bty="l", cex.axis=0.7, 
  cex.lab=0.9, col=c(gray(0.3), gray(0.7)), col.axis="darkgray", 
  col.box="darkgray", col.lab="darkgray", col.ticks="darkgray", las=0, 
  legend=FALSE, mgp=c(2, 0.7, 0), plot.names=FALSE, ylim=c(0,7), 
  ylab="Wind speed [m/s]")
}
}
\keyword{methods}
