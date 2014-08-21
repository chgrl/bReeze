\name{energy}
\encoding{UTF-8}
\alias{energy}
\alias{en}
\alias{plot.energy}
\title{Calculation of total wind energy content}
\description{Calculates the total wind energy content per direction sector from Weibull data.}
\usage{
energy(wb, rho=1.225, bins=c(5, 10, 15, 20), 
  digits=0, print=TRUE)
en(wb, rho=1.225, bins=c(5, 10, 15, 20), 
  digits=0, print=TRUE)

\method{plot}{energy}(x, show.total=TRUE, ...)
}
\arguments{
  \item{wb}{Weibull object created by \code{\link{weibull}}.}
  \item{rho}{Air density as numeric value. Default is \code{1.225} kg/m3 according to the International Standard Atmosphere (ISA) at sea level and 15°C.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired. Default is \code{c(5, 10, 15, 20)}.}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{0}.}
  \item{print}{If \code{TRUE} (the default), results are printed directly.}
    
  \item{x}{Energy object created by \code{energy}.}
  \item{show.total}{If \code{TRUE} (the default), the total amount of wind energy per square meter is shown.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
The total wind energy content can be perceived as the theoretic energy potential of a particular site. Therefore it is usefull for a resource assessment, independent of the wind turbine.

The power density function
\deqn{E(v) = \frac{1}{2} \, \rho \, v^3 \, f(v)}{E(v) = 0.5*rho*v^3*f(v)}
where \eqn{\rho}{rho} is the air density, \eqn{v} is the wind speed and \eqn{f(v)} is its probability density function, leads to an analytical solution using wind speed bins as:
\deqn{E(v) = \frac{1}{2} \, \rho \, H \, \sum_{b=1}^{n} \! v_b^3 \, W(v_b)}{E(v) = 0.5*rho*H*sum(v_b^3*W(v_b))}
where \eqn{H} is the number of hours of the desired period, \eqn{v_b} is the wind speed bin and \eqn{W(v_b)} is the probability of that bin estimated by the Weibull distribution. The result for \eqn{H=8760} is the available wind energy per square meter and year.
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
    \item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{energy} only contains the total energy per direction sector.
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
\value{
Returns a data frame containing:
  \item{total}{Total wind energy content per direction sector.}
  \item{\dots}{Wind energy content per direction sector for each given wind speed bin.}
}
\references{
Fördergesellschaft Windenergie e.V. (2007) Technical Guidelines for Wind Turbines, Part 6: Determination of Wind Potential and Energy Yields, Revision 7

International Organisation for Standardization (1975) ISO 2533:1975 Standard Atmosphere. ISO Standard

Troen, I., Petersen, E.L. (1989) \emph{European Wind Atlas}. Tønder: Laursen
}
\author{Christian Graul}
\seealso{\code{\link{weibull}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set2 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set3 <- set(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- mast(timestamp=ts, set1, set2, set3)
neubuerg <- clean(mast=neubuerg)

# calculate Weibull object
neubuerg.wb <- weibull(mast=neubuerg, v.set=1, print=FALSE)

# calculate energy
energy(wb=neubuerg.wb)

# calculate energy for 1 m/s speed bins and without binning
energy(wb=neubuerg.wb, bins=0:25)
energy(wb=neubuerg.wb, bins=NULL)

# calculate energy with site specific air density
energy(wb=neubuerg.wb, rho=1.115, bins=NULL)

# change number of digits and hide results
energy(wb=neubuerg.wb, digits=2)
energy(wb=neubuerg.wb, print=FALSE)
}
}
\keyword{methods}
