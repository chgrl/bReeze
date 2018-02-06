\encoding{UTF-8}
\name{winddata}
\alias{winddata}
\docType{data}
\title{Example data for bReeze}
\description{
This dataset is provided as part of the \code{bReeze} package for use with the examples in the documantation. 
It contains measured wind speed and wind direction in a 10 minute interval, collected by a meteorological mast.
}
\usage{data("winddata", package="bReeze")}
\format{
  Data frame with 36548 observations on the following 17 variables:
  \describe{
    \item{\code{date_time}}{Date and time of observation as character vector.}
    \item{\code{v1_40m_avg}}{Average wind speed in m/s at a height of 40 m as numeric vector.}
    \item{\code{v1_40m_max}}{Maximum wind speed in m/s at a height of 40 m as numeric vector.}
    \item{\code{v1_40m_min}}{Minimum wind speed in m/s at a height of 40 m as numeric vector.}
    \item{\code{v1_40m_std}}{Standard deviation of wind speed in m/s at a height of 40 m as numeric vector.}
    \item{\code{v2_30m_avg}}{Average wind speed in m/s at a height of 30 m as numeric vector.}
    \item{\code{v2_30m_max}}{Maximum wind speed in m/s at a height of 30 m as numeric vector.}
    \item{\code{v2_30m_min}}{Minimum wind speed in m/s at a height of 30 m as numeric vector.}
    \item{\code{v2_30m_std}}{Standard deviation of wind speed in m/s at a height of 30 m as numeric vector.}
    \item{\code{v3_20m_avg}}{Average wind speed in m/s at a height of 20 m as numeric vector.}
    \item{\code{v3_20m_max}}{Maximum wind speed in m/s at a height of 20 m as numeric vector.}
    \item{\code{v3_20m_min}}{Minimum wind speed in m/s at a height of 20 m as numeric vector.}
    \item{\code{v3_20m_std}}{Standard deviation of wind speed in m/s at a height of 20 m as numeric vector.}
    \item{\code{dir1_40m_avg}}{Average wind direction in degrees from north at a height of 40 m as numeric vector.}
    \item{\code{dir1_40m_std}}{Standard deviation of wind direction in degrees from north at a height of 40 m as numeric vector.}
    \item{\code{dir2_30m_avg}}{Average wind direction in degrees from north at a height of 30 m as numeric vector.}
    \item{\code{dir2_30m_std}}{Standard deviation of wind direction in degrees from north at a height of 30 m as numeric vector.}
  }
}
\examples{
\dontrun{
# load example data
data("winddata", package="bReeze")

# display the structure of the data
str(winddata)
}
}
\keyword{datasets}
