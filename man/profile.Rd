\encoding{UTF-8}
\name{profile}
\alias{profile}
\alias{pro}
\title{Calculate wind profile}
\description{Calculates a wind profile, using on the Hellman exponential law.}
\usage{
profile(mast, v.set, dir.set, num.sectors=12, 
  method=c("hellman", "loglm", "fixed"), alpha=NULL,
  subset, digits=3, print=TRUE)
pro(mast, v.set, dir.set, num.sectors=12, 
  method=c("hellman", "loglm", "fixed"), alpha=NULL,
  subset, digits=3, print=TRUE)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set(s) to be used for wind speed, specified as set number or set name. The first given dataset is used as reference height. If one single dataset is given, the same Hellman exponent is assumed for each sector and can be specified using \code{alpha} (see Details for empirical values). If two or more datasets are given, the Hellman exponent is calculated for each sector. If more than two datasets are given, currently only the first two datasets are used.}
  \item{dir.set}{Set to be used for wind direction, specified as set number or set name.}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{method}{Method to be used for the calculation. One of \code{"hellman"}, \code{"loglm"} or \code{"fixed"} (see Details section). Optional argument: \code{"fixed"} is used if \code{v.set} is a single dataset, \code{"hellman"} is used if \code{v.set} specifies two, \code{"loglm"} if \code{v.set} specifies three datasets.}
  \item{alpha}{Hellman exponent. Optional and only used if the choosen \code{method} is \code{"fixed"}. If \code{alpha} is \code{NULL}, \code{0.2} is used as default.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
The average wind speed as a function of height above ground gives a site's wind profile. For reasons of cost-efficiency met mast heights are usually below hub heights of modern wind turbines, thus measured wind speeds must be extrapolated by based wind profile. A common method is the Hellman exponential law or power law (\code{"hellman"}), defined as:
\deqn{\frac{v_2}{v_1} = \left(\frac{h_2}{h_1} \right)^\alpha}{v2/v1 = (h2/h1)^alpha}
where \eqn{v_2}{v2} is the wind speed at height \eqn{h_2}{h2}, \eqn{v_1}{v1} is the wind speed at height \eqn{h_1}{h1} and \eqn{\alpha}{alpha} is the Hellman exponent (also power law exponent or shear exponent).

To calculate \eqn{\alpha}{alpha}, \code{profile} uses the inverted equation as:
\deqn{\alpha = \frac{\log \left(\frac{v_2}{v_1} \right)}{\log \left(\frac{h_2}{h_1} \right)}}{alpha = log(v2/v1) / log(h2/h1)}

If data is available for two or more heights, a log-linear model fit can be used for the computation of \eqn{\alpha}{alpha}. In this case the data is logarithmized to allow for fitting a linear model. The models slope is then used as \eqn{\alpha}{alpha}. Please note: depending on the data this method might result in negative \eqn{\alpha}{alpha}.

If the wind speed is only known for one height, \eqn{\alpha}{alpha} must be estimated. \eqn{\alpha}{alpha} depends on various issues, including roughness and terrain of the site. Some empirical values for temperate climates are:

\tabular{ll}{
  \emph{ Site conditions} \tab \eqn{\alpha} \cr
  Open water \tab 0.08--0.15 \cr  Flat terrain, open land cover \tab 0.16--0.22 \cr  Complex terrain with mixed or continuous forest \tab 0.25--0.40 \cr  Exposed ridgetops, open land cover \tab 0.10--0.14 \cr  Sloping terrain with drainage flows \tab 0.10--0.15
}
}
\value{
Returns a list of:
  \item{profile}{Data frame containing alpha and reference wind speed for each direction sector.}
  \item{h.ref}{Reference height of the profile (the height of the first given dataset in \code{v.set}).}
}
\references{
Bañuelos-Ruedas, F., Camacho, C.A., Rios-Marcuello, S. (2011) Methodologies Used in the Extrapolation of Wind Speed Data at Different Heights and Its Impact in the Wind Energy Resource Assessment in a Region. In: Gastón O. Suvire (Ed.), Wind Farm -- Technical Regulations, Potential Estimation and Siting Assessment, Chapter 4, p. 97--114, InTech

Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx

International Electrotechnical Commission (2005) IEC 61400-12 Wind Turbines -- Part 12-1: Power Performance Measurements of Electricity Producing Wind Turbines. IEC Standard
}
\author{Christian Graul}
\seealso{\code{\link{createMast}}, \code{\link{plotProfile}}, \code{\link{printObject}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set40 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set30 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set20 <- createSet(height=20, v.avg=winddata[,10], v.std=winddata[,13])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set40, set30, set20)
neubuerg <- clean(mast=neubuerg)

# create profile based on one height
profile(mast=neubuerg, v.set=1, dir.set=1)	# default alpha=0.2
profile(mast=neubuerg, v.set=1, dir.set=1, alpha=0.15)

# calculate profile based on two heights
profile(mast=neubuerg, v.set=c(1,2), dir.set=1)
profile(mast=neubuerg, v.set=c(1,3), dir.set=1)
# same as above
profile(mast=neubuerg, v.set=c("set1", "set3"), dir.set="set1")

# calculate profile based on three heights
profile(mast=neubuerg, v.set=c(1,2,3), dir.set=1)

# change the method used for computation
# note: negative alphas!
profile(mast=neubuerg, v.set=c(1,2), dir.set=1, method="loglm")

# change number of direction sectors
profile(mast=neubuerg, v.set=c(1,2), dir.set=1, num.sectors=8)

# data subsets
profile(mast=neubuerg, v.set=1, dir.set=1, 
  subset=c("2009-12-01 00:00:00", "2009-12-31 23:50:00"))
profile(mast=neubuerg, v.set=c(1,2), dir.set=1, 
  subset=c("2010-01-01 00:00:00", NA)) # just 'start' time stamp
profile(mast=neubuerg, v.set=c(1:3), dir.set=1, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# change number of digits and hide results
profile(mast=neubuerg, v.set=1, dir.set=1, digits=2)
profile(mast=neubuerg, v.set=1, dir.set=1, print=FALSE)
}
}
\keyword{methods}
