\encoding{UTF-8}
\name{pc}
\alias{pc}
\alias{pc.default}
\alias{pc.read}
\alias{plot.pc}
\title{Import power curve from file}
\description{Imports a power curve from a WAsP 'wgt' file or a WindPower program 'pow' file.}
\usage{
pc(x, ...)

\method{pc}{default}(x, p, cp, ct, rho=1.225, rated.p, desc, ...)
\method{pc}{read}(x, ex=FALSE, ...)
\method{plot}{pc}(x, plot.cp=TRUE, plot.ct=TRUE, ...)
}
\arguments{
  \item{x}{Wind speed in m/s as numeric vector. Or the name of, or the path to a 'wgt' or 'pow' file containing power curve data. Or power curve object created by \code{pc}.}
  
  \item{p}{Corresponding power output in kW as numeric vector of same length as \code{v}.}
  \item{cp}{Power coefficient as numeric vector of same length as \code{v} (optional).}
  \item{ct}{Thrust coefficient as numeric vector of same length as \code{v} (optional).}
  \item{rho}{Air density as numeric value. Default is \code{1.225} kg/m3 according to the International Standard Atmosphere (ISA) at sea level and 15 degrees Celsius.}
  \item{rated.p}{Rated power of wind turbine in kW as numeric value. If not given, the rated power is set to the maximum value of \code{p}.}
  \item{desc}{Plain text information about the wind turbine as string (optional).}
  
  \item{ex}{Set to \code{TRUE} (the default), if \code{file} is an external file (not included in the package directory of \code{bReeze}) -- default is \code{FALSE}.}
  
  \item{plot.cp}{If \code{TRUE} (the default), the power coefficient (cp) is added to the plot (if available).}
  \item{plot.ct}{If \code{TRUE} (the default), the thrust coefficient (ct) is added to the plot (if available).}
  \item{\dots}{Arguments to be passed to methods. For optional graphical parameters see below.}
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

\code{bReeze} provides several power curves of common manufacturers, that can be read from the package directory. See examples for usage.

\emph{Available 'wtg' files}
\tabular{llll}{
 \tab Nordex_N80_2.5MW \tab Nordex_N90_2.5MW_HS \tab Nordex_N90_2.5MW_LS \cr
 \tab Nordex_N100_2.5MW \tab PowerWind_56_900kW \tab PowerWind_90_2.5MW \cr
 \tab Vestas_V52_850kW \tab Vestas_V60_850kW \tab Vestas_V80_2.0MW_os \cr
 \tab Vestas_V80_2.0MW \tab Vestas_V82_1650kW \tab Vestas_V90_1.8MW \cr
 \tab Vestas_V90_2.0MW \tab Vestas_V90_3.0MW \tab Vestas_V100_1.8MW_50Hz \cr
 \tab Vestas_V100_1.8MW_60Hz \tab Vestas_V112_3.0MW \tab
}

\emph{Available 'pow' files}
\tabular{llll}{
 \tab Bonus_82.4m_2.3MW \tab Bonus_MKIV_600kW \tab Clipper_LibertyC89_2.5MW \cr
 \tab Clipper_LibertyC93_2.5MW \tab Clipper_LibertyC96_2.5MW \tab Clipper_LibertyC100_2.5MW \cr
 \tab Enercon_E33_330kW \tab Enercon_E40_500kW \tab Enercon_E44_900kW \cr
 \tab Enercon_E48_800kW \tab Enercon_E53_800kW \tab Enercon_E66_1870kW \cr
 \tab Enercon_E66_2000kW \tab Enercon_E70_2.3MW \tab Enercon_E82_2.0MW \cr
 \tab Enercon_E82_2.3MW \tab Enercon_E82_3.0MW \tab Enercon_E101_3.0MW \cr
 \tab Enercon_E126_7.5MW \tab EWT_DW52_500kW \tab EWT_DW54_500kW \cr
 \tab EWT_DW90_2MW \tab EWT_DW96_2MW \tab Gamesa_G52_850kW \cr
 \tab Gamesa_G58_850kW \tab Gamesa_G80_2.0MW \tab Gamesa_G83_2.0MW \cr
 \tab Gamesa_G87_2.0MW \tab Gamesa_G90_2.0MW \tab GE_1.5sl_1.5MW \cr
 \tab GE_1.5sle_1.5MW \tab GE_1.5xle_1.5MW \tab GE_1.6MW \cr
 \tab GE_2.5xl_2.5MW \tab GE_3.6sl_3.6MW \tab Leitwind_LTW70_1.7MW \cr
 \tab Leitwind_LTW70_2.0MW \tab Leitwind_LTW77_1.5MW \tab Leitwind_LTW80_1.5MW \cr
 \tab Leitwind_LTW80_1.8MW \tab Leitwind_LTW101_3.0MW \tab Nordex_N60_1.3MW \cr
 \tab Nordex_N70_1.5MW \tab Nordex_N90_2.5MW \tab Nordex_N100_2.5MW \cr
 \tab Nordex_S70_1.5MW \tab Nordex_S77_1.5MW \tab Nordic_1000_1.0MW \cr
 \tab PowerWind_56_500kW \tab Repower_5M_5.0MW \tab Repower_MM82_2.0MW \cr
 \tab Repower_MM92_2.0MW \tab Siemens_SWT-2.3MW-93m \tab Siemens_SWT-2.3MW-101m \cr
 \tab Siemens_SWT-3.6MW-107m \tab Siemens_SWT-3.6MW-120m \tab Suzlon_S64_1.25MW \cr
 \tab Suzlon_S64_950kW \tab Suzlon_S66_1.25MW \tab Suzlon_S88_2.1MW \cr
 \tab VensysEnergy_77_1.5MW \tab Vensys_82_1.5MW \tab Vensys_100_2.5MW \cr
 \tab Vensys_109_2.5MW \tab Vensys_112_2.5MW \tab Vestas_V27_225kW \cr
 \tab Vestas_V39_500kW \tab Vestas_V52_850kW \tab Vestas_V80_2.0MW_os \cr
 \tab Vestas_V80_2.0MW \tab Vestas_V82_1.65MW \tab Vestas_V90_2.0MW \cr
 \tab Vestas_V90_3.0MW \tab Vestas_V112_3MW \tab Vestas_V164_7.0MW_os
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
    \item \code{col}: Vector of colours. The first colour is used for the power curve. If only one coefficient is available, the second colour is used for this coefficient, if both coefficients are available, the second colour is used for \code{pc} and the third for \code{ct}.
	\item \code{col.axis}: Colour to be used for axis annotations -- default is \code{"black"}.
	\item \code{col.box}: Colour to be used for the box around the plot region (if \code{bty}) -- default is \code{"black"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{col.leg}: Colour to be used for legend text -- default is \code{"black"}.
    \item \code{col.ticks}: Colours for the axis line and the tick marks respectively -- default is \code{"black"}.
    \item \code{las}: Style of axis labels. One of \code{0} (always parallel to the axis, default), \code{1} (always horizontal), \code{2} (always perpendicular to the axis), \code{3} (always vertical).
    \item \code{legend}: If \code{TRUE} (the default) a legend is drawn.
    \item \code{leg.text}: A character or \code{\link{expression}} vector to appear in the legend.
    \item \code{lty}: Vector of line types, assigned like \code{col}. See \code{\link{par}} for available line types.
    \item \code{lwd}: Vector of line widths, assigned like \code{col}. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(5, 5, 1, 5)} for one y axis and \code{c(5, 5, 1, 1)} for two y axes.
    \item \code{mgp}: A numerical vector of the form c(label, annotation, line), which gives the margin line for the axis label, axis annotation and axis line. The default is \code{c(3, 1, 0)}.
    \item \code{pos.leg}: Position of legend -- one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"}. Use \code{NULL} to hide the legend.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative labels for the y axis, as vector of the form c(left axis label, right axis label).
	\item \code{ylim}: Limits of the y axis, as vector of two values.
	\item \code{x.intersp}: Horizontal interspacing factor for legend text, as numeric -- default is \code{0.4}.
    \item \code{y.intersp}: Vertical line distance for legend text, as numeric -- default is \code{0.4}.
  }
}
\value{Returns a data frame binding the given data}
\source{
Wind turbine generator files (*.wtg) are available from the WAsP website:

http://www.wasp.dk/Download/PowerCurves.aspx

Power curve files (*.pow) are available from the WindPower Program website:

http://www.wind-power-program.com/download.htm
}
\references{
Betz, A. (1966) \emph{Introduction to the Theory of Flow Machines}. Oxford: Pergamon Press

Burton, T., Sharpe, D., Jenkins, N., Bossanyi, E. (2001) \emph{Wind Energy Handbook}. New York: Wiley

International Electrotechnical Commission (2005) IEC 61400-12 Wind Turbines -- Part 12-1: Power Performance Measurements of Electricity Producing Wind Turbines. IEC Standard

Milan, P., Wächter, M., Barth, S., Peinke, J. (2010) Power Curves for Wind Turbines. In: Wei Tong (Ed.), Wind Power Generation and Wind Turbine Design, Chapter 18, p. 595--612, Southampton: WIT Press

Ragheb, M., Ragheb, A.M. (2011) Wind Turbines Theory -- The Betz Equation and Optimal Rotor Tip Speed Ratio. In: Rupp Carriveau (Ed.), Fundamental and Advanced Topics in Wind Power, Chapter 2, p. 19--38, InTech
}
\author{Christian Graul}
\note{
All power curves are provided without any warranty of accuracy and timeliness. Reliable data can only be received from the respective manufacturer directly. 
}
\examples{
\dontrun{
# minimal theoretic power curve
pc <- pc(x=1:25, p=c(0, 0, seq(0, 1000, length.out=8), 
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
	
pc.2 <- pc(x=v, p=p, cp=cp, ct=ct, rho=1.195, rated.p=900, 
  desc="PowerWind 56")
}


\dontrun{
# import power curve from bReeze wtg file
vestas.v90 <- pc(x="Vestas_V90_2.0MW.wtg")

# import power curve from bReeze pow file
repower.mm92 <- pc(x="Repower_MM92_2.0MW.pow")

# import power curve from user file
# e.g.: my.pc <- pc(x="~/Projects/bReeze/Sandbox/myPC.wtg", ex=TRUE)
}
}
\keyword{methods}
