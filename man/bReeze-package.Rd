\encoding{UTF-8}
\name{bReeze-package}
\alias{bReeze-package}
\alias{bReeze}
\docType{package}
\title{Functions for wind resource assessment}
\description{
A collection of functions to analyse, visualize and interpret wind data and to calculate the potential energy production of wind turbines.
}
\details{
\tabular{lll}{
  \tab Package: \tab bReeze\cr
  \tab Type: \tab Package\cr
  \tab Version: \tab 0.3-2\cr
  \tab Date: \tab 2014-05-08\cr
  \tab License: \tab GPL (>=2)\cr
}
Wind power is a global source of energy which attracts a large share of investments in renewables. Project sites with high wind potential do not only minimise the financial risk but increase the efficiency in carbon footprint reduction. Hence a thorough productivity analysis of a wind project is essential, regarding both economic and environmental aspects. The evaluation of the potential of a site requires a wind resource assessment, which is best based on data gained by a local measurement campaign. A methodology of processing the measured data has been established, resulting in a better understanding of the wind conditions of a site and therefore a more reliable estimation of energy production.

\code{bReeze} is a collection of widely used methods to analyse, visualise and interpret wind data. Wind resource analyses can subsequently be combined with characteristics of wind turbines to estimate the potential energy production on the investigated site.

Usually the data to be analysed are collected by meteorological masts (met masts) and averaged over ten minutes, but other time intervals are also processable. 

\code{bReeze} suggests three packages, written by other developers: \code{RColorBrewer} (by Erich Neuwirth) provides nice colours for the graphics, \code{XML} (by Duncan Temple Lang) provides xml parsing for power curve import and \code{RgoogleMaps} (by Markus Loecher) provides simple site maps.
	
Try the examples below to check if \code{bReeze} has been correctly installed. Any question and feedback is welcome via email to <christian.graul@gmail.com>.
}
\author{
Christian Graul and Carsten Poppinga

Maintainer: Christian Graul <christian.graul@gmail.com>
}
\references{
The following handbook gives a detailed thematic overview and is available online:

Brower, M., Marcus, M., Taylor, M., Bernadett, D., Filippelli, M., Beaucage, P., Hale, E., Elsholz, K., Doane, J., Eberhard, M., Tensen, J., Ryan, D. (2010) Wind Resource Assessment Handbook. http://www.renewablenrgsystems.com/TechSupport/~/media/Files/PDFs/wind_resource_handbook.ashx

Further references are given under the specific functions of the package.
}
\keyword{package}
\examples{
\dontrun{
# load example data
data(neubuerg)

# create two datasets
set40 <- set(height=40, v.avg=neubuerg[,2], v.std=neubuerg[,5],
  dir.avg=neubuerg[,14])
set30 <- set(height=30, v.avg=neubuerg[,6], v.std=neubuerg[,9],
  dir.avg=neubuerg[,16])

# format time stamp
ts <- timestamp(timestamp=neubuerg[,1])

# create met mast object
metmast <- mast(timestamp=ts, set40=set40, set30=set30)

# plot time series of met mast signals
plot(metmast)

# calculate frequency and mean wind speed per wind direction sector
freq <- frequency(mast=metmast, v.set=1)

# plot frequency
plot(freq)

# calculate availability of pairs of wind speed and direction
availability(mast=metmast)

# calculate monthly means of wind speed
month.stats(mast=metmast)

# calculate turbulence intensity
turbulence(mast=metmast, turb.set=1)

# calculate weibull parameters
wb <- weibull(mast=metmast, v.set=1)

# calculate total wind energy content
energy(wb=wb)

# calculate wind profile
pf <- profile(mast=metmast, v.set=c(1,2), dir.set=1)

# import power curve
pc <- pc("Enercon_E126_7.5MW.pow")

# calculate annual energy production
aep <- aep(profile=pf, pc=pc, hub.h=135)

# plot AEP
plot(aep)
}
}
