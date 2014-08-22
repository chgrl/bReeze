\encoding{UTF-8}
\name{map.plot}
\alias{map.plot}
\alias{map}
\title{Plot map or satellite image}
\description{Plots a map or satellite image of the met mast location.}
\usage{
map.plot(mast, type=c("satellite", "terrain", "hybrid", "roadmap"), 
  zoom, label, ...)
map(mast, type=c("satellite", "terrain", "hybrid", "roadmap"), 
  zoom, label, ...)
}
\arguments{
  \item{mast}{Met mast object created by \code{\link{mast}}.}
  \item{type}{Type of the map as string. One of \code{"satellite"} (satellite image), \code{"terrain"} (map with terrain information), \code{"hybrid"} (satellite image with map overlay) or \code{"roadmap"} (map).}
  \item{zoom}{Zoom level as integer from \code{1} (small scale) up to \code{18} (large scale) -- default is \code{15}.}
  \item{label}{Label to be placed next to the location symbol as string. Set to \code{NA} to leave the label blank. If ignored, the location coordinates are used as label default.}
  \item{\dots}{Optional graphical parameters, see below for details.}
}
\section{Optional graphical parameters}{
The following graphical parameters can optionally be added to customize the plot:
  \itemize{
  	\item \code{cex}: Numeric value, giving the amount by which location symbol should be scaled relative to the default (which is \code{1.5}).
    \item \code{col}: The colour of the symbol.
    \item \code{pch}: Location symbol, either as integer or as single character. See \code{\link{points}} for possible values and their interpretation --  default is \code{8}.
    \item \code{cex.lab}: Numeric value, giving the amount by which the label should be scaled relative to the default (which is \code{1}).
    \item \code{col.lab}: The colour of the label -- default is same as \code{col}.
    \item \code{pos.lab}: Position specifier for the label. One of \code{1} (below symbol), \code{2} (left of symbol), \code{3} (above symbol) or \code{4} (right of symbol) -- default is \code{4}.
  }
}
\details{
This function is based on the \code{RgoogleMaps} package by Markus Loecher, which uses the Google Static Maps API.
}
\author{Christian Graul}
\seealso{\code{\link{mast}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)
set1 <- set(height=40, v.avg=winddata[,2])
ts <- timestamp(winddata[,1])
neubuerg <- mast(timestamp=ts, set1, loc=c(49.8909,11.4017))

# plot satellite image
map.plot(neubuerg)

# plot terrain map
map.plot(neubuerg, type="terrain")

# change zoom level
map.plot(neubuerg, zoom=1)
map.plot(neubuerg, zoom=18)

# change symbol (and label) 
map.plot(neubuerg, col="white", pch="+", cex=2)

# change label
map.plot(neubuerg, col.lab=3, cex.lab=1.5)
map.plot(neubuerg, pos.lab=1)
map.plot(neubuerg, label="Site #247 - Neubuerg")  # custom label
map.plot(neubuerg, label=NA)  # no label
}
}
\keyword{methods}
