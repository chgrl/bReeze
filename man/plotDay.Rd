\encoding{UTF-8}
\name{plotDay}
\alias{plotDay}
\alias{plday}
\title{Plot diurnal wind speed}
\description{Plots the diurnal variation of wind speed or wind direction.}
\usage{
plotDay(mast, set, dir.set=set, signal, num.sectors=NULL, ...)
plday(mast, set, dir.set=set, signal, num.sectors=NULL, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{set}{Set used for plotting, specified as set number or set name. Argument is optional -- if missing, all sets containing the choosen \code{signal} are used.}
  \item{dir.set}{Direction set used for sectoral splitting of the data, specified as set number or set name. Argument is used for sectoral plotting only (see \code{num.sectors}).}
  \item{signal}{Signal to be plotted as string.}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Argument is optional -- if not \code{NULL}, data is splitted into directional sectors for plotting. Sectoral plots are available only for single sets.}
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
    \item \code{lty}: Vector of line types, one for each set plotted. See \code{\link{par}} for available line types.
    \item \code{lwd}: Vector of line widths, one for each set plotted. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(4.5, 5, 1.5, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.8}.
  }
}
\details{
\code{plotDay} reveals diurnal variations of a signal. The plot may show outliers that indicate data inconsistancy.
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

# plot all datasets
plotDay(mast=neubuerg, signal="v.avg")
plotDay(mast=neubuerg, signal="dir.avg")

# plot one dataset
plotDay(mast=neubuerg, set=1, signal="v.avg")
plotDay(mast=neubuerg, set="set1", signal="v.avg")	# same as above
plotDay(mast=neubuerg, set=2, signal="dir.avg")

# sectoral plot
plotDay(mast=neubuerg, set=1, signal="v.avg", num.sectors=8)
plotDay(mast=neubuerg, set="set3", dir.set="set2", signal="v.avg")

# customize plot
plotDay(mast=neubuerg, signal="v.avg", 
  bty="l", cex.axis=0.8, cex.lab=0.9, cex.leg=0.7, 
  col=c("darkgreen", "royalblue", "purple"), col.axis="darkgray", 
  col.box="darkgray", col.lab="darkgray", col.leg="darkgray", 
  col.ticks="darkgray", las=2, lty=c(2,3,4), lwd=1.5, mar=c(3, 3, 0.5, 0.5), 
  mgp=c(1.5, 0.5, 0), pos.leg="topleft", xlab="hour", ylab="velocity", 
  ylim=c(3.5,6), x.intersp=1, y.intersp=1)
}
}
\keyword{methods}
