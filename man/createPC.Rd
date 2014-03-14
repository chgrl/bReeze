\encoding{UTF-8}
\name{createPC}
\alias{createPC}
\alias{pc}
\title{Creation of power curve objects}
\description{Creates power curve objects, characterizing wind turbines.}
\usage{
createPC(v, p, cp, ct, rho=1.225, rated.p, desc)
pc(v, p, cp, ct, rho=1.225, rated.p, desc)
}
\arguments{
  \item{v}{Wind speed in m/s as numeric vector.}
  \item{p}{Corresponding power output in kW as numeric vector of same length as \code{v}.}
  \item{cp}{Power coefficient as numeric vector of same length as \code{v} (optional).}
  \item{ct}{Thrust coefficient as numeric vector of same length as \code{v} (optional).}
  \item{rho}{Air density as numeric value. Default is \code{1.225} kg/m3 according to the International Standard Atmosphere (ISA) at sea level and 15 degrees Celsius.}
  \item{rated.p}{Rated power of wind turbine in kW as numeric value. If not given, the rated power is set to the maximum value of \code{p}.}
  \item{desc}{Plain text information about the wind turbine as string (optional).}
}
\details{
\emph{Power curve}

A power curve characterizes the power production of a wind turbine and gives the amount of generated electrical power output as a function of wind speed. The theoretical power curve of a turbine is defined as:
\tabular{lll}{
	\tab \eqn{P \propto v^3}{P ~ v^3} \tab \eqn{\mbox{for} \ v<v_{rated}}{for v<v_rated} \cr
	\tab \eqn{P = P_{rated}}{P = P_rated} \tab \eqn{\mbox{for} \ v>v_{rated}}{for v>v_rated}}
Hence the generated power is proportional to the wind speed cubed, for wind speeds lower than rated wind speed. For higher wind speeds the generated power is equal to the rated power of the turbine.

Conventionally a power curve consists of pairs of wind speed and power in 0.5 or 1 m/s wind speed bins, starting at 0 m/s or the cut-in wind speed of the turbine and ending with the cut-out wind speed, e.g. at about 25 m/s.

\emph{Coefficients}

The power coefficient \eqn{c_p}{cp} is defined as:
\deqn{c_p = \frac{P_t}{P}}{cp = Pt/P}
where \eqn{P_t}{Pt} is the ratio of the electrical power extracted by the wind turbine and \eqn{P} is the energy available in the wind stream. According to Betz's law, the theoretically achievable power coefficient is approximately 0.59. However, no wind turbine will obtain this value, due to inefficiencies and various losses of the machine.

The thrust coefficient is a turbine specific characteristic and used for the modelling of wake effects. Therefore it is an important parameter for wind farm configuration.
}
\value{Returns a data frame binding the given data.}
\references{
Betz, A. (1966) \emph{Introduction to the Theory of Flow Machines}. Oxford: Pergamon Press

Burton, T., Sharpe, D., Jenkins, N., Bossanyi, E. (2001) \emph{Wind Energy Handbook}. New York: Wiley

International Electrotechnical Commission (2005) IEC 61400-12 Wind Turbines -- Part 12-1: Power Performance Measurements of Electricity Producing Wind Turbines. IEC Standard

Milan, P., Wächter, M., Barth, S., Peinke, J. (2010) Power Curves for Wind Turbines. In: Wei Tong (Ed.), Wind Power Generation and Wind Turbine Design, Chapter 18, p. 595--612, Southampton: WIT Press

Ragheb, M., Ragheb, A.M. (2011) Wind Turbines Theory -- The Betz Equation and Optimal Rotor Tip Speed Ratio. In: Rupp Carriveau (Ed.), Fundamental and Advanced Topics in Wind Power, Chapter 2, p. 19--38, InTech
}
\author{Christian Graul}
\seealso{\code{\link{readPC}}, \code{\link{plotPC}}, \code{\link{printObject}}}
\examples{
\dontrun{
# minimal theoretic power curve
pc <- createPC(v=1:25, p=c(0, 0, seq(0, 1000, length.out=8), 
  rep(1000, 15)))
	
# detailed power curve
v <- seq(3, 25, 0.5)
p <- c(5, 15.5, 32, 52, 71, 98, 136, 182, 230, 285, 345, 419, 497, 594, 
  687, 760, 815, 860, 886, rep(900, 26))
	
cp <- c(0.263, NA, 0.352, NA, 0.423, NA, 0.453, NA, 0.470, NA, 0.478, 
  NA, 0.480, NA, 0.483, NA, 0.470, NA, 0.429, NA, 0.381, NA, 0.329, 
  NA, 0.281, NA, 0.236, NA, 0.199, NA, 0.168, NA, 0.142, NA, 0.122, 
  NA, 0.105, NA, 0.092, NA, 0.080, NA, 0.071, NA, 0.063)
	
ct <- c(0.653, NA, 0.698, NA, 0.705, NA, 0.713, NA, 0.720, NA, 0.723, 
  NA, 0.724, NA, 0.727, NA, 0.730, NA, 0.732, NA, 0.385, NA, 0.301, 
  NA, 0.242, NA, 0.199, NA, 0.168, NA, 0.146, NA, 0.128, NA, 0.115, 
  NA, 0.103, NA, 0.094, NA, 0.086, NA, 0.079, NA, 0.073)
	
pc.2 <- createPC(v=v, p=p, cp=cp, ct=ct, rho=1.195, rated.p=900, 
  desc="PowerWind 56")

# compare power curves
printObject(object=pc)
printObject(object=pc.2)
}
}
\keyword{methods}
