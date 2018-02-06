\encoding{UTF-8}
\name{turb.iec.plot}
\alias{turb.iec.plot}
\alias{iec}
\title{Plot turbulence intensity site classification}
\description{Plots the turbulence intensity and site classification after IEC.}
\usage{
turb.iec.plot(mast, set, subset, ...)
iec(mast, set, subset, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{mast}.}
  \item{set}{Set used for plotting specified as set number or set name.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{border}: Colour, used for the border around the bars -- default is \code{"white"}.
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
    \item \code{line}: Vector of three colours -- one for each IEC turbulence class.
    \item \code{lty}: Vector of three line types -- one for each IEC turbulence class. See \code{\link{par}} for available line types.
    \item \code{lwd}: Vector of three line widths -- one for each IEC turbulence class. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot (only for plots with one dataset) -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.2, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
    \item \code{space}: Numeric value between 0 and 1, giving the space left before each bar. Default space is \code{0.2}.
	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Limits of the x axis, as vector of two values.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
	\item \code{y.intersp}: Vertical interspacing factor for legend text, as numeric -- default is \code{0.8}.
  }
}
\details{
The IEC defines wind turbine classes by wind speed and turbulence characteristics. In terms of turbulence intensity three reference values (at 15 m/s) are defined:
\tabular{ll}{
  \emph{ Turbulence class} \tab \emph{Reference value} \cr
  A \tab 0.16 \cr
  B \tab 0.14 \cr
  C \tab 0.12
}
\code{plotTurbIEC} plots these IEC references together with the sites values to allow for a classification.

See \code{\link{turbulence}} for a definition of turbulence intensity.
}
\references{
International Electrotechnical Commission (2005) IEC 61400-1 Wind Turbines -- Part 1: Design Requirements. IEC Standard
}
\author{Christian Graul}
\seealso{\code{\link{mast}}}
\examples{
\dontrun{
# load and prepare data
data("winddata", package="bReeze")
set40 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5])
set30 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9])
set20 <- set(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- mast(timestamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# plot
turb.iec.plot(mast=neubuerg, set=1)
turb.iec.plot(mast=neubuerg, set="set1")	# same as above

# data subsets
turb.iec.plot(mast=neubuerg, set=1, 
  subset=c("2009-12-01 00:00:00", "2009-12-31 23:50:00"))
turb.iec.plot(mast=neubuerg, set=1,  
  subset=c("2010-01-01 00:00:00", NA)) # just 'start' time stamp
turb.iec.plot(mast=neubuerg, set=1, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# customize plot
turb.iec.plot(mast=neubuerg, set=1, bty="l", cex.axis=0.8, cex.lab=0.9, 
  cex.leg=0.7, col.axis="darkblue", col.box="lightblue", col.lab=
  "darkblue", col.leg="darkblue", col.ticks="darkblue", las=0, 
  leg.text=c("IEC class A", "IEC class B", "IEC class C", "measured"), 
  mar=c(3,3,0.5,0.5), mgp=c(1.8,0.5,0), pos.leg="top", xlab="v [m/s]", 
  ylab="ti [-]", xlim=c(0,25), ylim=c(0,0.5), x.intersp=1, y.intersp=1)

# customize bars
turb.iec.plot(mast=neubuerg, set=1, col="gray", border="black", space=0.6)

# customize lines
turb.iec.plot(mast=neubuerg, set=1, line=gray(1:3 / 10), lty=2:4, 
  lwd=0.5:2.5)
}
}
\keyword{methods}
