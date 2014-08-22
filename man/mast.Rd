\encoding{UTF-8}
\name{mast}
\alias{mast}
\alias{plot.mast}
\title{Creation of met mast objects}
\description{Creates met mast objects from one or more datasets (measurement heights). All datasets are sorted by height in descending order.}
\usage{
mast(timestamp, ..., loc=NULL, desc=NULL)

\method{plot}{mast}(x, set, signal=c("v.avg", "dir.avg", "turb.int"), subset, ...)
}
\arguments{
  \item{timestamp}{Time stamp as \code{POSIXlt} vector, e.g. created by \code{\link{timestamp}}.}
  \item{\dots}{At least one dataset created by \code{\link{set}}. For plotting function: optional graphical parameters (see below).}
  \item{loc}{Lat/lon coordinates of the site in decimal degrees as vector of two numeric values (optional).}
  \item{desc}{Plain text information about the site, measurement, met mast condition, etc. as string (optional).}
  
  \item{x}{Met mast object, created by \code{mast}.}
  \item{set}{Set used for plotting specified as set number or set name. Argument is optional -- if missing, all sets containing the given \code{signal}(s) are used.}
  \item{signal}{Signal(s) to be plotted as vector of strings giving the signal names.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
}
\details{
Valuable information about a met mast is usually provided by an installation protocol.

Measurements of wind speed in several heights is required for the computation of the site's wind profile. Met masts might have mounted more than one sensor of the same type at the same height. Thus, the data of a broken sensor can later be substituted by the 'backup-sensor'.
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
\value{
  Returns a met mast object, which is necessary for all data analyses. A met mast object is a list of:
  \item{timestamp}{Time stamp of the observations.}
  \item{location}{Lat/lon coordinates of the site (optional).}
  \item{description}{Mast information (optional).}
  \item{sets}{List of one or more datasets (measurement heights) consisting of height information and the data.}
}
\author{Christian Graul}
\seealso{\code{\link{POSIXlt}}, \code{\link{set}}, \code{\link{timestamp}}}
\examples{
\dontrun{
# load data, prepare sets and time stamp
data(winddata)
set40 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set20 <- set(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- timestamp(timestamp=winddata[,1])

# create met mast object
neubuerg <- mast(timestamp=ts, set40, set30, set20)

# create met mast with location and description
neubuerg.2 <- mast(timestamp=ts, set40, set30, set20, 
  loc=c(49.8909,11.4017), desc="Site #247 - Neubuerg")

# view structure of met mast
str(neubuerg.2)

# name sets
neubuerg.3 <- mast(timestamp=ts, C1.A1=set40, C2.A2=set30, 
  C3=set20, loc=c(49.8909,11.4017), desc="Site #247 - Neubuerg")

# compare names
names(neubuerg.2$sets)	# default names
names(neubuerg.3$sets)
}
}
\keyword{methods}
