\encoding{UTF-8}
\name{plotWbDir}
\alias{plotWbDir}
\alias{plwbd}
\title{Plot Weibull fit per direction sector}
\description{Plots the fitted Weibull distribution per direction sector from a \code{\link{weibull}} object.}
\usage{
plotWbDir(wb, show.ak=FALSE, ...)
plwbd(wb, show.ak=FALSE, ...)
}
\arguments{
  \item{wb}{Weibull object created by \code{\link{weibull}}.}
  \item{show.ak}{If \code{TRUE}, the Weibull parameters A and k are added to the legend.}
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
    \item \code{col}: Vector of colours for the sectoral distributions, with the length of sector number + 1, where the last colour is used for the general profile.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{lty}: Vector of line types, assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{lwd}: Vector of line widths, assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.5, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Numeric vector of the x limits of the plot.
    \item \code{ylim}: Numeric vector of the y limits.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.8}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{weibull}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- set(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- createMast(timestamp=ts, set1)
neubuerg <- clean(mast=neubuerg)

# calculate Weibull parameters
neubuerg.wb <- weibull(mast=neubuerg, v.set=1, print=FALSE)

# plot
plotWbDir(wb=neubuerg.wb)

# show parameters in legend
plotWbDir(wb=neubuerg.wb, show.ak=TRUE)

# customize plot
plotWbDir(wb=neubuerg.wb, bty="l", bty.leg="o", cex.axis=0.8, cex.lab=0.9, 
  cex.leg=0.7, col=c(rainbow(12), gray(0.4)), col.axis=gray(0.2), 
  col.box=gray(0.4), col.lab=gray(0.2), col.leg=gray(0.2), 
  col.ticks=gray(0.2), las=0, lty=c(rep(3, 12), 1), lwd=c(rep(1, 12), 2), 
  mar=c(3,3,0.5,0.5), mgp=c(1.5,0.5,0), pos.leg="right", 
  xlab="velocity [m/s]", ylab="frequency [m/s]", xlim=c(0,15), 
  ylim=c(0,25), x.intersp=1, y.intersp=1)
}
}
\keyword{methods}
