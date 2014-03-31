\encoding{UTF-8}
\name{uncertainty}
\alias{uncertainty}
\alias{uc}
\title{Uncertainty assessment}
\description{Calculates probability of exceedance based of AEP data and given uncertainties of applied methods.}
\usage{
uncertainty(aep, uc.values, uc.names, prob=seq(5,95,5), 
  digits=c(0,0), print=TRUE)
uc(aep, uc.values, uc.names, prob=seq(5,95,5), 
  digits=c(0,0), print=TRUE)
}
\arguments{
  \item{aep}{AEP object created by \code{aep}.}
  \item{uc.values}{Uncertainty values (in percent) for applied methods as numeric vector.}
  \item{uc.names}{Names of the uncertainty components. Normally a string vector of the same length as \code{uc.values}. If \code{uc.names} is a string vector with the length of \code{uc.values} + 1, the calculated total uncertainty is named after the additional name (default is "total"). If \code{uc.names} is \code{NULL}, the methods are numbered consecutively.}
  \item{prob}{Probability values used for the probability of exceedance analysis.}
  \item{digits}{Number of decimal places to be used for results as numeric vector. The first value is used for uncertainties of applied methods, the second for probability of exceedance results. Default is \code{c(0,0)}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
}
\details{
A wind resource assessment, like every statistical analysis, is only complete with an accompanying uncertainty assessment. \code{uncertainty} provides a simple tool to arrange uncertainties of the methods applied and analyse their approximate effects to the energy output of a turbine. The total uncertainty arises from many uncertainty components of different type and impact. Common components are wind measurement (sensor quality/calibration, mast influences, etc.), data analysis (missing data, data selection, simplifications/assumptions etc.), long term data (reference data, length of measuring period, etc.), flow modelling (horizontal and vertical extrapolations, etc.) or power curve (measurement quality, assumptions, etc.).

Assuming all uncertainty components to be independent from each other, the combined standard uncertainty is calculated as follows:
\deqn{U = \sqrt{u_1 + u_2 + ... + u_n}}{U = sqrt(u_1 + u_2 + ... + u_n)}
where \eqn{U} is the total uncertainty and \eqn{u_1} untill \eqn{u_n} are the uncertainty components.
}

\value{
  Returns a list containing:
  \item{uncertainty.meth}{Table of uncertainty components and their uncertainty value.}
  \item{prob.exceedance}{Table of probability values and the related annual energy production.}
}
\references{
Measnet (2009) MEASNET Procedure: Evaluation of Site Specific Wind Conditions, Version 1
}
\author{Christian Graul}
\seealso{\code{\link{aep}}, \code{\link{plotUncertainty}}, \code{\link{printObject}}}
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

# calculate aep
nb.wp <- profile(mast=neubuerg, v.set=c(1,2), dir.set=1, 
  print=FALSE)
pw.56 <- readPC(file="PowerWind_56_900kW.wtg")
nb.aep <- aep(profile=nb.wp, pc=pw.56, hub.h=71, print=FALSE)

# calculate uncertainty
uncertainty(nb.aep, uc.values=c(5,10,5,5), 
  uc.names=c("Wind Measurement Mast", "Long Term Correlation", 
  "Flow Model", "Power Curve"))

# unnamed uncertainty components
uncertainty(nb.aep, uc.values=c(5,10,5,5), 
  uc.names=NULL)

# new name for combined uncertainty
uncertainty(nb.aep, uc.values=c(5,10,5,5), 
  uc.names=c("Wind Measurement Mast", "Long Term Correlation", 
  "Flow Model", "Power Curve", "Total Uncertainty"))

# changed probability values
uncertainty(nb.aep, uc.values=c(5,10,5,5), 
  uc.names=c("Wind Measurement Mast", "Long Term Correlation", 
  "Flow Model", "Power Curve"), prob=seq(50,90,10))

# change number of digits and hide results
uncertainty(nb.aep, uc.values=c(5,10,5,5), 
  uc.names=c("Wind Measurement Mast", "Long Term Correlation", 
  "Flow Model", "Power Curve"), digits=c(1,2), print=FALSE)
}
}
\keyword{methods}
