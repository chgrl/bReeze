\encoding{UTF-8}
\name{turbulence}
\alias{turbulence}
\alias{turb}
\title{Calculation of turbulence intensity}
\description{Calculates turbulence intensity and mean wind speed for each given direction sector.}
\usage{
turbulence(mast, turb.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20), subset, digits=3, print=TRUE)
turb(mast, turb.set, dir.set, num.sectors=12, 
  bins=c(5, 10, 15, 20), subset, digits=3, print=TRUE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{turb.set}{Set used for turbulence intensity, specified as set number or set name (optional, if \code{dir.set} is given).}
  \item{dir.set}{Set used for wind direction, specified as set number or set name (optional, if \code{turb.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired. Default is \code{c(5, 10, 15, 20)}.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE} (the default), results are printed directly.}
}
\details{
Turbulence can be perceived as wind speed fluctuations on a relatively short time scale and it strongly depends on surface roughness, terrain features, as well as thermal effects. High turbulence should be avoided, since it is a main driver of fatigue loads and might decrease energy output. A measure of the overall level of turbulence, is the turbulence intensity \eqn{I}, which is defined as:
\deqn{I = \frac{\sigma}{\bar v}}{I = sigma / v}
where \eqn{\sigma}{sigma} is the standard deviation of wind speed -- usually measured over a 10-minutes period -- and \eqn{\bar v}{v} is the mean wind speed over this period. 
}
\section{Optional graphical parameters for plotting}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
    \item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{circles}: Manual definition of circles to be drawn, as numeric vector of the form c(inner circle, outer circle, interval between the circles).
    \item \code{col}: Colour to be used for the sectors.
    \item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"gray45"}.
    \item \code{col.border}: Colour to be used for sector borders -- default is \code{NULL} (no border is drawn).
    \item \code{col.circle}: Colour to be used for circles -- default is \code{"gray45"}.
    \item \code{col.cross}: Colour to be used for axis lines -- default is \code{"gray45"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{fg}: If \code{TRUE}, sectors are plotted in foreground (on top of axis lines and circles) -- default is \code{FALSE}.
    \item \code{lty.circle}: Line type of circles -- default is \code{"dashed"}. See \code{\link{par}} for available line types.
    \item \code{lty.cross}: Line type of axis lines -- default is \code{"solid"}. See \code{\link{par}} for available line types.
    \item \code{lwd.border}: Line width of the sector borders -- default is \code{0.5}. Only used if \code{col.border} is set.
    \item \code{lwd.circle}: Line width of circles, as numeric value -- default is \code{0.7}.
    \item \code{lwd.cross}: Line width of axis lines, as numeric value -- default is \code{0.7}.
    \item \code{pos.axis}: Position of axis labels in degree, as numeric value -- default is \code{60}.
    \item \code{sec.space}: Space between plotted sectors, as numeric value between \code{0} and \code{1} -- default is \code{0.2}.
  }
}
\value{Returns a data frame containing:
  \item{wind.speed}{Mean wind speed for each direction sector.}
  \item{total}{Total turbulence intensity for each direction sector.}
  \item{\dots}{Turbulence intensities per direction sector for each given wind speed bin.}
}
\references{
Albers, A. (2010) Turbulence and Shear Normalisation of Wind Turbine Power Curve. Proceedings of the EWEC 2010, Warsaw, Poland

Burton, T., Sharpe, D., Jenkins, N., Bossanyi, E. (2001) \emph{Wind Energy Handbook}. New York: Wiley

Langreder, W. (2010) Wind Resource and Site Assessment. In: Wei Tong (Ed.), Wind Power Generation and Wind Turbine Design, Chapter 2, p. 49--87, Southampton: WIT Press
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set20 <- set(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- timestamp(timestamp=winddata[,1])
neubuerg <- createMast(timestamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# calculate turbulence intensity
turbulence(mast=neubuerg, turb.set=1)

# use different datasets for turb.set and dir.set
turbulence(mast=neubuerg, turb.set=1, dir.set=2)
turbulence(mast=neubuerg, turb.set="set1", dir.set="set2")	# same as above

# change number of direction sectors
turbulence(mast=neubuerg, turb.set=1, num.sectors=4)

# calculate turbulence intensity for 1 m/s speed bins and without binning
turbulence(mast=neubuerg, turb.set=1, bins=1:25)
turbulence(mast=neubuerg, turb.set=1, bins=NULL)

# data subset
turbulence(mast=neubuerg, turb.set=1, 
  subset=c(NA, "2010-01-01 00:00:00"))

# change number of digits and hide results
turbulence(mast=neubuerg, turb.set=1, digits=2)
turbulence(mast=neubuerg, turb.set=1, print=FALSE)
}
}
\keyword{methods}
