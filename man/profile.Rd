\encoding{UTF-8}
\name{profile}
\alias{profile}
\alias{pro}
\alias{plot.profile}
\title{Calculate wind profile}
\description{Calculates a wind profile, using on the Hellman exponential law.}
\usage{
profile(mast, v.set, dir.set, num.sectors=12, 
  method=c("hellman", "loglm", "fixed"), alpha=NULL, 
  subset, digits=3, print=TRUE)
pro(mast, v.set, dir.set, num.sectors=12, 
  method=c("hellman", "loglm", "fixed"), alpha=NULL, 
  subset, digits=3, print=TRUE)

\method{plot}{profile}(x, sector, measured=TRUE, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{mast}}.}
  \item{v.set}{Set(s) to be used for wind speed, specified as set number or set name. The first given dataset is used as reference height. If one single dataset is given, the same Hellman exponent is assumed for each sector and can be specified using \code{alpha} (see Details for empirical values). If two or more datasets are given, the Hellman exponent is calculated for each sector. If more than two datasets are given, currently only the first two datasets are used.}
  \item{dir.set}{Set to be used for wind direction, specified as set number or set name.}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{method}{Method to be used for the calculation. One of \code{"hellman"}, \code{"loglm"} or \code{"fixed"} (see Details section). Optional argument: \code{"fixed"} is used if \code{v.set} is a single dataset, \code{"hellman"} is used if \code{v.set} specifies two, \code{"loglm"} if \code{v.set} specifies three datasets.}
  \item{alpha}{Hellman exponent -- one general exponent or a vector of exponents (one per sector). Optional and only used if the choosen \code{method} is \code{"fixed"}. If \code{alpha} is \code{NULL} (the default), \code{0.2} is used as default.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE} (the default), results are printed directly.}
  
  \item{x}{Wind profile object created by \code{profile}.}
  \item{sector}{Direction sector as integer (sector number) or string (sector code). If missing or \code{NULL}, all sectors are plotted. For plotting the general profile only use \code{"all"}.}
  \item{measured}{If \code{TRUE} (the default), measured sector mean wind speeds are added to the plot.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
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
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box.
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Vector of colours, one for each set plotted.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{lty}: Line type(s) of the profile lines -- assigned like \code{col}. See \code{\link{par}} for available line types.
    \item \code{lwd}: Line width(s) of the profile lines -- assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(4, 4, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.5, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Numeric vector of the x limits of the plot.
    \item \code{ylim}: Numeric vector of the y limits.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.8}.
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
\seealso{\code{\link{mast}}}
\examples{
\dontrun{
# load and prepare data
data(neubuerg)
set40 <- set(height=40, v.avg=neubuerg[,2], dir.avg=neubuerg[,14])
set30 <- set(height=30, v.avg=neubuerg[,6], dir.avg=neubuerg[,16])
set20 <- set(height=20, v.avg=neubuerg[,10], v.std=neubuerg[,13])
ts <- timestamp(timestamp=neubuerg[,1])
neubuerg <- mast(timestamp=ts, set40, set30, set20)
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
