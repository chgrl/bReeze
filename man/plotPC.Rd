\encoding{UTF-8}
\name{plotPC}
\alias{plotPC}
\alias{plpc}
\title{Plot power curve}
\description{Plots a power curve object.}
\usage{
plotPC(pc, cp=TRUE, ct=TRUE, ...)
plpc(pc, cp=TRUE, ct=TRUE, ...)
}
\arguments{
  \item{pc}{Power curve object ceated by \code{\link{createPC}} or imported from file by \code{\link{readPC}}, or a list of power curve objects.}
  \item{cp}{If \code{TRUE}, the power coefficient (cp) is added to the plot (if available).}
  \item{ct}{If \code{TRUE}, the thrust coefficient (ct) is added to the plot (if available).}
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
    \item \code{col}: Vector of colours. The first colour is used for the power curve. If only one coefficient is available, the second colour is used for this coefficient, if both coefficients are available, the second colour is used for \code{pc} and the third for \code{ct}.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{leg.text}: A character or \code{\link{expression}} vector to appear in the legend.
    \item \code{lty}: Vector of line types, assigned like \code{col}. See \code{\link{par}} for available line types.
    \item \code{lwd}: Vector of line widths, assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(5, 5, 1, 5)} for one y axis and \code{c(5, 5, 1, 1)} for two y axes.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(3, 1, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative labels for the y axis, as vector of the form c(left axis label, right axis label).
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.4}.
  }
}
\author{Christian Graul}
\seealso{\code{\link{createPC}}, \code{\link{readPC}}}
\examples{
\dontrun{
# create power curve
v <- seq(3, 25, 0.5)
p <- c(5, 15.5, 32, 52, 71, 98, 136, 182, 230, 285, 345, 419, 497, 594, 
  687, 760, 815, 860, 886, rep(900, 26))cp <- c(0.263, NA, 0.352, NA, 0.423, NA, 0.453, NA, 0.470, NA, 0.478, NA, 
  0.480, NA, 0.483, NA, 0.470, NA, 0.429, NA, 0.381, NA, 0.329, NA, 0.281, 
  NA, 0.236, NA, 0.199, NA, 0.168, NA, 0.142, NA, 0.122, NA, 0.105, NA, 
  0.092, NA, 0.080, NA, 0.071, NA, 0.063)ct <- c(0.653, NA, 0.698, NA, 0.705, NA, 0.713, NA, 0.720, NA, 0.723, NA, 
  0.724, NA, 0.727, NA, 0.730, NA, 0.732, NA, 0.385, NA, 0.301, NA, 0.242, 
  NA, 0.199, NA, 0.168, NA, 0.146, NA, 0.128, NA, 0.115, NA, 0.103, NA, 
  0.094, NA, 0.086, NA, 0.079, NA, 0.073)my.pc <- createPC(v=v, p=p, cp=cp, ct=ct, rho=1.195, rated.p="900 kW")

# plot power curve with and without coefficients
plotPC(pc=my.pc)
plotPC(pc=my.pc, cp=FALSE, ct=FALSE)

# plot list of power curves
enercon.e82 <- readPC(file="Enercon_E82_2.0MW.pow")
gamesa.g83 <- readPC(file="Gamesa_G83_2.0MW.pow")
vestas.v80 <- readPC(file="Vestas_V80_2.0MW.wtg")
vestas.v90 <- readPC(file="Vestas_V90_2.0MW.wtg")
pc.2mw <- list(V80=vestas.v80, E82=enercon.e82, G83=gamesa.g83)
plotPC(pc.2mw, ct=FALSE)
plotPC(list(vestas.v80, vestas.v90))

# customize plot
plotPC(pc=my.pc, bty="u", bty.leg="o", cex.axis=0.8, cex.lab=0.9, 
  cex.leg=0.7, col=c("red", gray(0.4), gray(0.4)), col.axis=gray(0.2), 
  col.box=gray(0.5), col.lab=gray(0.2), col.leg=gray(0.2), 
  col.ticks=gray(0.5), las=2, leg.text=c("electric Power", 
  "power coefficient", "thrust coefficient"), lty=2:4, lwd=c(2,1,1), 
  mar=c(3.5,3.5,0.5,3.5), mgp=c(1.8,0.6,0), pos.leg="top", 
  xlab="velocity [m/s]", ylab=c("electric power", "coefficients"), 
  ylim=c(0,1100), x.intersp=1, y.intersp=1)
}
}
\keyword{methods}
