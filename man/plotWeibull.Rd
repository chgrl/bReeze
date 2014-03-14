\encoding{UTF-8}
\name{plotWeibull}
\alias{plotWeibull}
\alias{plwb}
\title{Plot wind speed distribution and Weibull fit}
\description{Plots the distribution of wind speeds and the fitted weibull distribution from a \code{\link{weibull}} object.}
\usage{
plotWeibull(wb, show.ak=FALSE, ...)
plwb(wb, show.ak=FALSE, ...)
}
\arguments{
  \item{wb}{Weibull object created by \code{\link{weibull}}.}
  \item{show.ak}{If \code{TRUE}, the Weibull parameters A and k are added to the legend.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{  
    \item \code{border}: Colour, used for the border around the bars -- default is \code{"white"}.
    \item \code{breaks}: A numeric vector giving the breakpoints between histogram cells.
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box. 
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Colour, used to fill the bars.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{leg.text}: A character or \code{\link{expression}} vector to appear in the legend.
    \item \code{line}: Colour, used for the Weibull fit line.
    \item \code{lty}: Line type of the Weibull fit line -- see \code{\link{par}} for available line types.
    \item \code{lwd}: Line width for the Weibull fit line -- see \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot (only for plots with one dataset) -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.2, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Limits of the x axis, as vector of two values.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
	\item \code{y.intersp}: Vertical interspacing factor for legend text, as numeric -- default is \code{0.8}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{weibull}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set1)
neubuerg <- clean(mast=neubuerg)

# calculate Weibull parameters
neubuerg.wb <- weibull(mast=neubuerg, v.set=1, print=FALSE)

# plot
plotWeibull(wb=neubuerg.wb)

# show parameters in legend
plotWeibull(wb=neubuerg.wb, show.ak=TRUE)

# customize plot
plotWeibull(wb=neubuerg.wb, bty="l", bty.leg="l", cex.axis=1.2, 
  cex.lab=1.4, cex.leg=0.9, col.axis="darkgray", 
  col.box="darkgray", col.lab="darkgray", col.leg="darkgray", 
  col.ticks="darkgray", las=0, leg.text=c("measured", "calculated"), 
  mar=c(3,3,0.5,0.5), mgp=c(1.8,0.5,0), pos.leg="right", 
  xlab="velocity [m/s]", ylab="frequency [\%]", xlim=c(0,20), 
  ylim=c(0,15), x.intersp=1, y.intersp=1)

# customize bars
plotWeibull(wb=neubuerg.wb, border="darkgray", breaks=seq(0,21,0.5), 
  col="lightgray")

# customize line
plotWeibull(wb=neubuerg.wb, line="black", lty="dotdash", lwd=2)
}
}
\keyword{methods}
