\encoding{UTF-8}
\name{aep}
\alias{aep}
\alias{plot.aep}
\title{Calculation of annual energy production}
\description{Calculates annual energy production (AEP) from a site's wind profile and wind turbine characteristics.}
\usage{
aep(profile, pc, hub.h, rho=1.225, avail=1, bins=c(5,10,15,20), 
  sectoral=FALSE, digits=c(3,0,0,3), print=TRUE)

\method{plot}{aep}(x, show.total=TRUE, ...)
}
\arguments{
  \item{profile}{Wind profile object created by \code{profile}.}
  \item{pc}{Power curve object created by \code{\link{pc}}.}
  \item{hub.h}{Hub heigth of wind turbine as numeric value.}
  \item{rho}{Air density as numeric value. Default is \code{1.225} kg/m3 according to the International Standard Atmosphere (ISA) at sea level and 15°C.}
  \item{avail}{Availability of turbine as numeric value between \code{0} (not available at all) and \code{1} (100\% available).}
  \item{bins}{Edges of wind speed bins as numeric vector or \code{NULL} if only total AEP is desired. Default values are \code{c(5, 10, 15, 20)}.}
  \item{sectoral}{If \code{TRUE}, wind speeds are extrapolated to hub height using the wind profiles of each direction sector. Otherwise the general profile (\code{"all"}) is used for extrapolation (default).}
  \item{digits}{Number of decimal places to be used for results as numeric vector. The first value is used for \code{wind.speed}, the second for \code{operation}, the third for \code{aep} and the fourth for \code{capacity} results. Default is \code{c(3,0,0,3)}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
  
  \item{x}{AEP object, created by \code{aep}.}
  \item{show.total}{If \code{TRUE} (the default) the total AEP is added to the plot.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
For a wind turbine the mean energy production can be expressed by
\deqn{E = T \, \int_{v_{in}}^{v_{out}} \! f(v) \, p(v)}{E = T*integral(v_in, v_out) f(v)*p(v)}
where \eqn{f(v)} is the probability density function of the wind speed \eqn{v}, \eqn{p(v)} is the power curve of the turbine and \eqn{T} is the production time period. Energy production starts at the turbine's cut-in wind speed \eqn{v_{in}}{v_in} and stops at cut-out wind speed \eqn{v_{out}}{v_out}.

Based on this fundamental expression, \code{aep} calculates the annual energy production as follows:   
\deqn{AEP = A_{turb} \, \frac{\rho}{\rho_{pc}} \, H \, \sum_{b=1}^{n} \! W(v_b) \, P(v_b)}{AEP = A_turb*rho/rho_pc*H*sum(W(v_b)*P(v_b))}
where \eqn{A_{turb}}{A_turb} is the average availability of the turbine, \eqn{\rho}{rho} is the air density of the site and \eqn{\rho_{pc}}{rho_pc} is the air density, the power curve is defined for. \eqn{W(v_b)} is the probability of the wind speed bin \eqn{v_b}, estimated by the Weibull distribution and \eqn{P(v_b)} is the power output for that wind speed bin. \eqn{H} is the number of operational hours -- the production time period of the AEP is per definition 8760 hours.

The wind speed \eqn{v_h} at hub height \eqn{h} of the turbine is extrapolated from the measured wind speed \eqn{v_{ref}}{v_ref} at reference height \eqn{h_{ref}}{h_ref} using the Hellman exponential law (see \code{profile}): 
\deqn{v_h = v_{ref} \, \left(\frac{h}{h_{ref}} \right)^\alpha}{v_h = v_ref*(h/h_ref)^alpha}

The productive suitability of a wind turbine for a site can be evaluated by the capacity factor \eqn{CF}. This factor is defined as the ratio of average power output of a turbine to the theoretical maximum power output. Using the AEP as the average power output, the rated power \eqn{P_{rated}} of a turbine and the maximum operational hours of a year we get:
\deqn{CF = \frac{AEP}{P_{rated} \, 8760}}{CF = AEP/(P_rated*8760)}
}
\note{
Sectoral extrapolation should be used carefully. Some sector's profile might be abnormal -- particularly in case of short measurement periods (<= one year) and/or few samples per sector -- causing biased results. Always check the profiles and set \code{sectoral} to \code{FALSE} to get more robust results.
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
    \item \code{col}: Vector of colours -- one colour for each wind speed bin or a single colour if \code{aep} only contains the total AEP.
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
  Returns a list containing:
  \item{wind.speed}{Mean wind speed for each direction sector.}
  \item{operation}{Operational hours per year for each direction sector.}
  \item{total}{Total annual energy production for each direction sector.}
  \item{\dots}{Annual energy production per wind speed bin for each direction sector.}
  \item{capacity}{Capacity factor of the wind turbine.}
}
\references{
Burton, T., Sharpe, D., Jenkins, N., Bossanyi, E. (2001) \emph{Wind Energy Handbook}. New York: Wiley

Fördergesellschaft Windenergie e.V. (2007) Technical Guidelines for Wind Turbines, Part 6: Determination of Wind Potential and Energy Yields, Revision 7

International Organisation for Standardization (1975) ISO 2533:1975 Standard Atmosphere. ISO Standard

Jangamshetti, S.H., Rau, V.G. (1999) Site Matching of Wind Turbine Generators: A Case Study. \emph{IEEE Transaction on Energy Conversion} \bold{14}(4), 1537--1543
}
\author{Christian Graul}
\seealso{\code{\link{profile}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set2 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set1, set2)
neubuerg <- clean(mast=neubuerg)

# calculate wind profile
neubuerg.wp <- profile(mast=neubuerg, v.set=c(1,2), dir.set=1, 
  print=FALSE)

# load power curve
pw.56 <- readPC(file="PowerWind_56_900kW.wtg")

# calculate AEP
aep(profile=neubuerg.wp, pc=pw.56, hub.h=71)

# calculate AEP with site specific air density and availability of 97%
aep(profile=neubuerg.wp, pc=pw.56, hub.h=71, rho=1.195, avail=0.97)

# calculate total AEP using sectoral profiles
aep(profile=neubuerg.wp, pc=pw.56, hub.h=71, sectoral=TRUE)

# calculate AEP for 1 m/s speed bins and without binning
aep(profile=neubuerg.wp, pc=pw.56, hub.h=71, bins=seq(0,25))
aep(profile=neubuerg.wp, pc=pw.56, hub.h=71, bins=NULL)

# change number of digits and hide results
aep(profile=neubuerg.wp, pc=pw.56, hub.h=71, digits=c(1,1,1,1))
neubuerg.aep <- aep(profile=neubuerg.wp, pc=pw.56, hub.h=71, print=FALSE)
}
}
\keyword{methods}
