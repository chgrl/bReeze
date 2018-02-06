\encoding{UTF-8}
\name{polar.plot}
\alias{polar.plot}
\alias{pol}
\title{Plot wind speed vs. direction}
\description{Plots wind speeds against directions in a polar plot.}
\usage{
polar.plot(mast, v.set=1, dir.set=1, subset, ...)
pol(mast, v.set=1, dir.set=1, subset, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{mast}}.}
  \item{v.set}{Set used for wind speed values, specified as set number or set name.}
  \item{dir.set}{Set used for wind direction values, specified as set number or set name.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
    \item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.pts}: Amount by which the plot symbols should be scaled, as numeric value.
    \item \code{circles}: Manual definition of circles to be drawn, as numeric vector of the form c(inner circle, outer circle, interval between the circles).
    \item \code{col}: The colour of the symbols plotted.
    \item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"gray45"}.
    \item \code{col.circle}: Colour to be used for circles -- default is \code{"gray45"}.
    \item \code{col.cross}: Colour to be used for axis lines -- default is \code{"gray45"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{fg}: If \code{TRUE}, sectors are plotted in foreground (on top of axis lines and circles) -- default is \code{FALSE}.
    \item \code{lty.circle}: Line type of circles -- default is \code{"dashed"}. See \code{\link{par}} for available line types.
    \item \code{lty.cross}: Line type of axis lines -- default is \code{"solid"}. See \code{\link{par}} for available line types.
    \item \code{lwd.circle}: Line width of circles, as numeric value -- default is \code{0.7}.
    \item \code{lwd.cross}: Line width of axis lines, as numeric value -- default is \code{0.7}.
    \item \code{pch}: Either an integer specifying a symbol or a single character to be used as symbol -- default is \code{"."}, which is drawn much faster than other symbols. See \code{\link{points}} for possible values and their interpretation. 
    \item \code{pos.axis}: Position of axis labels in degree, as numeric value -- default is \code{60}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{mast}}}
\examples{
\dontrun{
# load and prepare data
data("winddata", package="bReeze")
set40 <- set(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set20 <- set(height=20, v.avg=winddata[,10])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- mast(timestamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# plot v vs. dir
polar.plot(mast=neubuerg)
polar.plot(mast=neubuerg, v.set=3, dir.set=2)
polar.plot(mast=neubuerg, v.set="set3", dir.set="set2")	# same as above

# data subsets
polar.plot(mast=neubuerg, 
  subset=c("2009-12-01 00:00:00", "2009-12-31 23:50:00"))
polar.plot(mast=neubuerg,  
  subset=c("2010-01-01 00:00:00", NA)) # just 'start' time stamp
polar.plot(mast=neubuerg, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# customize plot
polar.plot(mast=neubuerg, cex.axis=1.2, cex.lab=1.5, cex.pts=0.8, 
  circles=c(10,20,5), col="red3", col.axis=gray(0.2), col.circle=gray(0.3), 
  col.cross=gray(0.3), col.lab=gray(0.2), fg=TRUE, lty.circle="dotted", 
  lty.cross="longdash", lwd.circle=1.2, lwd.cross=1.2, pch=1, pos.axis=135)
}
}
\keyword{methods}
