\encoding{UTF-8}
\name{plotFrequency}
\alias{plotFrequency}
\alias{plfreq}
\title{Plot frequency data}
\description{Plots joint probability of wind speeds and directions from a \code{\link{frequency}} object in a rose plot.}
\usage{
plotFrequency(freq, ...)
plfreq(freq, ...)
}
\arguments{
  \item{freq}{Frequency object created by \code{\link{frequency}}.}
  \item{\dots}{Optional graphical parameters, see below for details.}
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
\author{Christian Graul}
\seealso{\code{\link{frequency}}}
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
neubuerg.freq <- frequency(mast=neubuerg, v.set=1, print=FALSE)
neubuerg.freq.2 <- frequency(mast=neubuerg, v.set=1, bins=NULL, 
  print=FALSE)

# plot frequency
plotFrequency(freq=neubuerg.freq)
plotFrequency(freq=neubuerg.freq.2)

# change colours
plotFrequency(freq=neubuerg.freq, col=gray(5:0 / 5.5))

# change text sizes
plotFrequency(freq=neubuerg.freq, cex=0.8)

# manual definition of circles
plotFrequency(freq=neubuerg.freq, circles=c(10, 30, 10))

# plot sectors in foreground
plotFrequency(freq=neubuerg.freq, fg=TRUE)

# change position of axis labels
plotFrequency(freq=neubuerg.freq, pos.axis=135)

# no legend
plotFrequency(freq=neubuerg.freq, width.leg=0)

# freaky
plotFrequency(freq=neubuerg.freq, border.leg=heat.colors(5), bty.leg="o", 
  cex.axis=0.5, cex.lab=2, cex.leg=0.5, circles=c(5, 30, 5), 
  col=rainbow(5), col.axis="green", col.border="orange", 
  col.circle="purple", col.cross="yellow", col.lab="pink", 
  col.leg="lightblue", fg=TRUE, lwd.border=2, lwd.circle=3, lwd.cross=3, 
  lty.circle="12345678", lty.cross="87654321", sec.space=0.6, 
  title.leg="* WiNd SpEeD *", x.intersp=2, y.intersp=5)
}
}
\keyword{methods}
