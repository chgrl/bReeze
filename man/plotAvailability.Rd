\encoding{UTF-8}
\name{plotAvailability}
\alias{plotAvailability}
\alias{plavail}
\title{Plot availability}
\description{Plots daily availability from an \code{\link{availability}} object in a calendar plot.}
\usage{
plotAvailability(avail, set, ...)
plavail(avail, set, ...)
}
\arguments{
  \item{avail}{Availability object created by \code{\link{availability}}.}
  \item{set}{Number of dataset to plot specified as set number or set name (optional).}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
    \item \code{border}: Colour of the cell borders -- default value is \code{"black"}.
    \item \code{cex}: Numeric value, giving the amount by which text on the plot should be scaled relative to the default (which is 1).
	\item \code{cex.axis}: Amount by which axis annotations should be scaled, as numeric value.
    \item \code{cex.lab}: Amount by which axis labels should be scaled, as numeric value.
    \item \code{col}: Vector of three text colours (or just one colour), where the first indicates 'full availability', the second 'partial availability' and the third 'not available' -- default is \code{"black"}.
	\item \code{col.axis}: Colour to be used for axis -- default is \code{"gray45"}.
    \item \code{col.lab}: Colour to be used for axis labels -- default is \code{"black"}.
    \item \code{fill}: Vector of three fill colours (or just one colour), where the first indicates 'full availability', the second 'partial availability' and the third 'not available'.
    \item \code{lwd}: Line widths of the cell borders. See \code{\link{par}} for usage.
    \item \code{mar}: A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot -- default is \code{c(0, 0, 0, 0)}.
    \item \code{plot.names}: If \code{TRUE} (the default), the names of the datasets is plotted as second label of the y axis.
   	\item \code{xlab}: Alternative label for the x axis.
	\item \code{ylab}: Alternative label for the y axis.
  }
}
\author{Christian Graul}
\seealso{\code{\link{availability}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- createSet(height=40, v.avg=winddata[,2], dir.avg=winddata[,14])
set2 <- createSet(height=30, v.avg=winddata[,6], dir.avg=winddata[,16])
set3 <- createSet(height=20, v.avg=winddata[,10])
ts <- formatTS(time.stamp=winddata[,1])
neubuerg <- createMast(time.stamp=ts, set1, set2, set3)
neubuerg <- clean(mast=neubuerg)

# calculate availability
neubuerg.avail <- availability(mast=neubuerg, print=FALSE)

# plot availability
plotAvailability(avail=neubuerg.avail)
plotAvailability(avail=neubuerg.avail, set=2)	# one dataset
plotAvailability(avail=neubuerg.avail, set="set2")	# same as above
plotAvailability(avail=neubuerg.avail, set=c(1,2))	# several datasets

# customize plot
plotAvailability(avail=neubuerg.avail, border="darkgray", cex.axis=0.7, 
  cex.lab=0.9, col=c("darkgreen", "orange", "red4"), col.axis="blue", 
  col.lab="blue", fill=c("lightgreen", "yellow", "red"), lwd=0.5, 
  mar=c(1,1,1,1), plot.names=FALSE, xlab="jour", ylab="mois")
}
}
\keyword{methods}
