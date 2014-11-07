\encoding{UTF-8}
\name{combine}
\alias{combine}
\title{Combine plots}
\description{Combine plots.}
\usage{
combine(a, b, names, show.total=TRUE, ...)
}
\arguments{
  \item{a}{Plot object.}
  \item{b}{Plot object.}
  \item{names}{Names for \code{a} and \code{b}.}
  \item{show.total}{If \code{TRUE} (the default) the total numbers are added to the plot.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\note{
Please note: currently only \code{\link{aep}} plot objects are supported.
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{border.leg}: Border colour(s) for the legend. One colour for each wind speed bin or a single colour -- default is same as \code{col}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
    \item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{circles}: Manual definition of circles to be drawn, as numeric vector of the form c(inner circle, outer circle, interval between the circles).
    \item \code{col}: List of two (vectors of) colours, to be assigned to \code{a} and \code{b} -- one colour for each wind speed bin or a single colour.
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
    \item \code{width.leg}: Widths of legend space relative to plot space, as numeric value between \code{0} and \code{1}. If \code{0}, the legend is omitted, default value is \code{0.2}.
    \item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.4}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{aep}}}
\examples{
\dontrun{
## load and prepare data
data(winddata)
set1 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set2 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- mast(timestamp=ts, set1, set2)
neubuerg <- clean(mast=neubuerg)


## calculate AEP
# calculate wind profile
neubuerg.wp <- profile(mast=neubuerg, v.set=c(1,2), dir.set=1, 
  print=FALSE)

# load power curve
pw.56 <- pc("PowerWind_56_900kW.wtg")

# calculate AEP
aep.59 <- aep(profile=neubuerg.wp, pc=pw.56, hub.h=59)
aep.71 <- aep(profile=neubuerg.wp, pc=pw.56, hub.h=71)


## plot AEP objects
plt.59 <- plot(aep.59, type="n")
plt.71 <- plot(aep.71, type="n")


## combine plots
# default
combine(plt.59, plt.71)
}
}
\keyword{methods}
