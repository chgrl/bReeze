\encoding{UTF-8}
\name{weibull}
\alias{weibull}
\alias{wb}
\title{Calculation of Weibull parameters}
\description{Calculates the shape parameters (k) and scale parameters (A) of the Weibull distribution per direction sector.}
\usage{
weibull(mast, v.set, dir.set, num.sectors=12,
  digits=3, print=TRUE)
wb(mast, v.set, dir.set, num.sectors=12,
  digits=3, print=TRUE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set used for wind speed, specified as set number or set name (optional, if dir.set is given).}
  \item{dir.set}{Set used for wind direction, specified as set number or set name (optional, if \code{v.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
To evaluate the potential energy production of a site the observed data of a particular measurement period must be generalized to a wind speed distribution. This is commonly done by fitting the Weibull function to the data. The two-parametered Weibull distribution is expressed mathematically as:
\deqn{f(v) = \frac{k}{A} \left({\frac{v}{A}} \right)^{k-1} e^{-\left({\frac{v}{A}} \right)^k}}{f(v) = k/A*(v/A)^(k-1)*exp(-(v/A)^k))}
where \eqn{f(v)} is the frequency of occurence of wind speed \eqn{v}, \eqn{A} is the scale parameter (measure for the wind speed) and \eqn{k} is the shape parameter (description of the shape of the distribution).

The resulting Weibull distribution characterizes the wind regime on the site and can directly be used for the calculation of the potential energy production of a wind turbine (see \code{\link{aep}}).
}
\value{
Returns a data frame containing:
  \item{k}{Shape parameter of the Weibull distribution for each direction sector.}
  \item{A}{scale parameter of the Weibull distribution for each direction sector.}
  \item{wind.speed}{Mean wind speed of the Weibull distribution for each direction sector.}
  \item{frequency}{Frequency of the Weibull distribution for each direction sector.}
}
\references{
Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx

Langreder, W. (2010) Wind Resource and Site Assessment. In: Wei Tong (Ed.), Wind Power Generation and Wind Turbine Design, Chapter 2, p. 49--87, Southampton: WIT Press

Weibull, W. (1951) A Statistical Distribution Function of Wide Applicability. \emph{Journal of Applied Mechanics -- Trans. ASME} \bold{18}(3), 293--297
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotWeibull}}, \code{\link{printObject}}}
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

# calculate Weibull parameters
weibull(mast=neubuerg, v.set=1)

# if only one of v.set and dir.set is given, 
# the dataset is assigned to both
weibull(mast=neubuerg, v.set=1)
weibull(mast=neubuerg, dir.set=1)
weibull(mast=neubuerg, dir.set="set1")

# change number of direction sectors
weibull(mast=neubuerg, v.set=1, num.sectors=16)

# change number of digits and hide results
weibull(mast=neubuerg, v.set=1, digits=2)
weibull(mast=neubuerg, v.set=1, print=FALSE)
}
}
\keyword{methods}
