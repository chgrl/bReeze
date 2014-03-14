\encoding{UTF-8}
\name{plotUncertainty}
\alias{plotUncertainty}
\alias{pluc}
\title{Plot uncertainties and probability of exceedance}
\description{Plots uncertainties or probability of exceedance from an \code{\link{uncertainty}} object.}
\usage{
plotUncertainty(uncertainty, type=c("prob", "uncert"), 
  p.values=c(50, 75, 90), ...)
pluc(uncertainty, type=c("prob", "uncert"), 
  p.values=c(50, 75, 90), ...)
}
\arguments{
  \item{uncertainty}{Uncertainty object created by \code{\link{uncertainty}}.}
  \item{type}{Type of plot as string. One of \code{"prob"} (probability of exceedance plot) or \code{"uncert"} (uncertainties overview plot).}
  \item{p.values}{The P-values highlighted in the plot as numeric vector -- default is P50, P75 and P90.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the probability of exceedance plot (\code{type="prob"}):
  \itemize{
	\item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box.
	\item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
    \item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Vector of n colours -- [1] for the probability curve, [2:n] for the highlighted P-values (\code{p.values}).
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (\code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{leg.text}: Vector of strings used as alternative legend text.
    \item \code{lty}: Vector of line types -- usage like \code{col}. See \code{\link{par}} for available line types.
    \item \code{lwd}: Vector of line widths -- usage like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.7, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Limits of the x axis, as vector of two values.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.8}.
    
Optional graphical parameters for the uncertainty overview plot (\code{type="uncert"}):
    \item \code{border}: Colour to be used for the border of the bars -- usage like \code{col}, as default no border is drawn.
	\item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
    \item \code{cex.axis}: Amount by which the names of the uncertainty components should be scaled, as numeric value.
    \item \code{cex.text}: Amount by which the uncertainty values inside the bars should be scaled, as numeric value.
	\item \code{col}: One or a vector of colours. If one colour is specified, this colour is used for all bars. If two colours are specified, the first colour is used for uncertainty component bars and the second for the total uncertainty bar. Separate colours for each bar can also be specified.
	\item \code{col.axis}: Colour to be used for the names of the uncertainty components -- default is \code{"black"}.
	\item \code{col.text}: Colour to be used for the uncertainty values inside the bars -- default is \code{"white"}.
	\item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(3, 1, 0)}.
    \item \code{space}: Numeric value between 0 and 1, giving the space left before each bar. Default space is \code{0.2}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{uncertainty}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set2 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set1, set2)
neubuerg <- clean(mast=neubuerg)
nb.wp <- profile(mast=neubuerg, v.set=c(1, 2), dir.set=1, 
  print=FALSE)
pw.56 <- readPC(file="PowerWind_56_900kW.wtg")
nb.aep <- aep(profile=nb.wp, pc=pw.56, hub.h=71, print=FALSE)

# calculate uncertainty
nb.uc <- uncertainty(nb.aep, uc.values=c(5, 10, 5, 5), 
  uc.names=c("wind measurement", "long term data", 
  "flow model", "power curve"))

# probability of exceedance plot
plotUncertainty(uncertainty=nb.uc)

# change highlighted P-values
plotUncertainty(uncertainty=nb.uc, p.values=c(50, 95))

# change colours, line types, line width and text size
plotUncertainty(uncertainty=nb.uc, col="blue", lty=c(1, 2, 3, 4), 
  lwd=2, cex=1.2)

# freaky
plotUncertainty(uncertainty=nb.uc, bty="l", bty.leg="o", cex.axis=2, 
  cex.lab=0.5, cex.leg=0.8, col=c(5, 10, 15, 20), col.axis="sienna", 
  col.box="purple", col.lab="plum", col.leg="orchid", col.ticks="gold", 
  las=0, lty=c(8, 7, 6, 5), lwd=c(5, 3, 1, 0.5), mar=c(6, 5, 4, 3), 
  mgp=c(4, 2, 1), pos.leg="bottomleft", xlim=c(0.1, 0.9), ylim=c(1000, 2000), 
  x.intersp=2, y.intersp=1.5)


# uncertainty overview plot
plotUncertainty(uncertainty=nb.uc, type="uncert")

# change colours and border
plotUncertainty(uncertainty=nb.uc, type="uncert", 
  col="red", border="red4")
plotUncertainty(uncertainty=nb.uc, type="uncert", 
  col=c(gray(0.7), gray(0.5)), border=c(gray(0.6), gray(0.4)))
plotUncertainty(uncertainty=nb.uc, type="uncert", 
  col=c(5:1), border=c(1:5))

# change text size, space and margin
plotUncertainty(uncertainty=nb.uc, type="uncert", 
  cex=1.5, space=0.1, mar=c(1, 13, 1, 1))

# freaky
plotUncertainty(uncertainty=nb.uc, type="uncert", 
  border=c(11, 22, 33, 44, 55), cex.axis=0.7, cex.text=2, 
  col=c("maroon", "navy", "thistle", "rosybrown", "papayawhip"), 
  col.axis="pink3", col.text="seagreen", mar=c(3, 8, 2, 1), 
  mgp=c(0, 1, 2), space=1)
}
}
\keyword{methods}