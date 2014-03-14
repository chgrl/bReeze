\name{energy}
\encoding{UTF-8}
\alias{energy}
\alias{en}
\title{Calculation of total wind energy content}
\description{Calculates the total wind energy content per direction sector from Weibull data.}
\usage{
energy(wb, rho=1.225, bins=c(5, 10, 15, 20),
  digits=0, print=TRUE)
en(wb, rho=1.225, bins=c(5, 10, 15, 20),
  digits=0, print=TRUE)
}
\arguments{
  \item{wb}{Weibull object created by \code{\link{weibull}}.}
  \item{rho}{Air density as numeric value. Default is \code{1.225} kg/m3 according to the International Standard Atmosphere (ISA) at sea level and 15°C.}
  \item{bins}{Wind speed bins as numeric vector or \code{NULL} if no classification is desired.}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{0}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
The total wind energy content can be perceived as the theoretic energy potential of a particular site. Therefore it is usefull for a resource assessment, independent of the wind turbine.

The power density function
\deqn{E(v) = \frac{1}{2} \, \rho \, v^3 \, f(v)}{E(v) = 0.5*rho*v^3*f(v)}
where \eqn{\rho}{rho} is the air density, \eqn{v} is the wind speed and \eqn{f(v)} is its probability density function, leads to an analytical solution using wind speed bins as:
\deqn{E(v) = \frac{1}{2} \, \rho \, H \, \sum_{b=1}^{n} \! v_b^3 \, W(v_b)}{E(v) = 0.5*rho*H*sum(v_b^3*W(v_b))}
where \eqn{H} is the number of hours of the desired period, \eqn{v_b} is the wind speed bin and \eqn{W(v_b)} is the probability of that bin estimated by the Weibull distribution. The result for \eqn{H=8760} is the available wind energy per square meter and year.
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
\seealso{\code{\link{weibull}}, \code{\link{plotEnergy}}, \code{\link{printObject}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], v.std=winddata[,5],
  dir.avg=winddata[,14])
set2 <- createSet(height=30, v.avg=winddata[,6], v.std=winddata[,9],
  dir.avg=winddata[,16])
set3 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set1, set2, set3)
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
