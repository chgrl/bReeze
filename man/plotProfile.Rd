\encoding{UTF-8}
\name{plotProfile}
\alias{plotProfile}
\alias{plpro}
\title{Plot wind profile}
\description{Plots wind profiles from a \code{\link{profile}} object.}
\usage{
plotProfile(profile, sector, measured=TRUE, ...)
plpro(profile, sector, measured=TRUE, ...)
}
\arguments{
  \item{profile}{Wind profile object created by \code{\link{profile}}.}
  \item{sector}{Direction sector as integer (sector number) or string (sector code). If missing or \code{NULL}, all sectors are plotted. For plotting the general profile only use \code{"all"}.}
  \item{measured}{If \code{TRUE}, measured sector mean wind speeds are added to the plot.}
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
    \item \code{lty}: Line type(s) of the profile lines -- assigned like \code{col}. See \code{\link{par}} for available line types.
    \item \code{lwd}: Line width(s) of the profile lines -- assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(4, 4, 1, 1)}.
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
\seealso{\code{\link{profile}}}
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

# create profile
neubuerg.wp <- profile(mast=neubuerg, v.set=c(1,2), dir.set=1, 
  print=FALSE)

# plot all profiles
plotProfile(profile=neubuerg.wp)

# plot only one sector
row.names(neubuerg.wp$profile)	# available sectors
plotProfile(profile=neubuerg.wp, sector=3)	# ENE by sector number
plotProfile(profile=neubuerg.wp, sector="ene")	# ENE by sector code
plotProfile(profile=neubuerg.wp, sector="all")	# general profile

# omit 'measured' points
plotProfile(profile=neubuerg.wp, measured=FALSE)

# customize plot
plotProfile(profile=neubuerg.wp, bty="l", bty.leg="o", cex.axis=0.8, 
  cex.lab=0.9, cex.leg=0.7, col=rainbow(13), col.axis=gray(0.2), 
  col.box=gray(0.2), col.lab=gray(0.2), col.leg=gray(0.2), 
  col.ticks=gray(0.2), las=0, lty=c(rep(3,12),1), 
  lwd=c(rep(1.2,12), 1.7), mar=c(3,3,0.5,0.5), mgp=c(2,0.7,0), 
  pos.leg="right", xlab="velocity [m/s]", ylab="height [m]", 
  xlim=c(0,11), ylim=c(0,150), x.intersp=1, y.intersp=1.2)
}
}
\keyword{methods}
