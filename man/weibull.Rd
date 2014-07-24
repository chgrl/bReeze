\encoding{UTF-8}
\name{weibull}
\alias{weibull}
\alias{wb}
\alias{plot.weibull}
\title{Calculation of Weibull parameters}
\description{Calculates the shape parameters (k) and scale parameters (A) of the Weibull distribution per direction sector.}
\usage{
weibull(mast, v.set, dir.set, num.sectors=12, 
  subset, digits=3, print=TRUE)
wb(mast, v.set, dir.set, num.sectors=12, 
  subset, digits=3, print=TRUE)

\method{plot}{weibull}(x, show.ak=FALSE, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{createMast}}.}
  \item{v.set}{Set used for wind speed, specified as set number or set name (optional, if dir.set is given).}
  \item{dir.set}{Set used for wind direction, specified as set number or set name (optional, if \code{v.set} is given).}
  \item{num.sectors}{Number of wind direction sectors as integer value greater 1. Default is \code{12}.}
  \item{subset}{Optional start and end time stamp for a data subset, as string vector \code{c(start, end)}. The time stamps format shall follow the rules of ISO 8601 international standard, e.g. "2012-08-08 22:55:00".}
  \item{digits}{Number of decimal places to be used for results as numeric value. Default is \code{3}.}
  \item{print}{If \code{TRUE}, results are printed directly.}
  
  \item{x}{Weibull object, created by \code{weibull}.}
  \item{show.ak}{If \code{TRUE} (the default), the Weibull parameters A and k are added to the legend.}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
}
\details{
To evaluate the potential energy production of a site the observed data of a particular measurement period must be generalized to a wind speed distribution. This is commonly done by fitting the Weibull function to the data. The two-parametered Weibull distribution is expressed mathematically as:
\deqn{f(v) = \frac{k}{A} \left({\frac{v}{A}} \right)^{k-1} e^{-\left({\frac{v}{A}} \right)^k}}{f(v) = k/A*(v/A)^(k-1)*exp(-(v/A)^k))}
where \eqn{f(v)} is the frequency of occurence of wind speed \eqn{v}, \eqn{A} is the scale parameter (measure for the wind speed) and \eqn{k} is the shape parameter (description of the shape of the distribution).

The resulting Weibull distribution characterizes the wind regime on the site and can directly be used for the calculation of the potential energy production of a wind turbine (see \code{\link{aep}}).
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{  
    \item \code{border}: Colour, used for the border around the bars -- default is \code{"white"}.
    \item \code{breaks}: A numeric vector giving the breakpoints between histogram cells.
    \item \code{bty}: Type of box to be drawn around the plot region. Allowed values are \code{"o"} (the default), \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or \code{"]"}. The resulting box resembles the corresponding upper case letter. A value of \code{"n"} suppresses the box. 
    \item \code{bty.leg}: Type of box to be drawn around the legend. Allowed values are \code{"n"} (no box, the default) and \code{"o"}.
    \item \code{cex}: Amount by which text on the plot should be scaled relative to the default (which is \code{1}), as numeric. To be used for scaling of all texts at once.
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{cex.leg}: Amount by which legend text should be scaled, as numeric value.
    \item \code{col}: Colour, used to fill the bars.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{leg.text}: A character or \code{\link{expression}} vector to appear in the legend.
    \item \code{line}: Colour, used for the Weibull fit line.
    \item \code{lty}: Line type of the Weibull fit line -- see \code{\link{par}} for available line types.
    \item \code{lwd}: Line width for the Weibull fit line -- see \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot (only for plots with one dataset) -- default is \code{c(4.5, 4.5, 1, 1)}.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(2.2, 0.7, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
	\item \code{xlim}: Limits of the x axis, as vector of two values.
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
	\item \code{y.intersp}: Vertical interspacing factor for legend text, as numeric -- default is \code{0.8}.
  }
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
\seealso{\code{\link{createMast}}}
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

# data subsets
weibull(mast=neubuerg, v.set=1, 
  subset=c("2009-12-01 00:00:00", "2009-12-31 23:50:00"))
weibull(mast=neubuerg, v.set=1, 
  subset=c("2010-01-01 00:00:00", NA)) # just 'start' time stamp
weibull(mast=neubuerg, v.set=1, 
  subset=c(NA, "2009-12-31 23:50:00")) # just 'end' time stamp

# change number of digits and hide results
weibull(mast=neubuerg, v.set=1, digits=2)
weibull(mast=neubuerg, v.set=1, print=FALSE)
}
}
\keyword{methods}
