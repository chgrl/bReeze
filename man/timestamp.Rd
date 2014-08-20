\encoding{UTF-8}
\name{timestamp}
\alias{timestamp}
\alias{ts}
\title{Format time stamps}
\description{Converts time stamps from string to \code{POSIXlt}. The conversion specification (pattern) is looked up if not given as argument.}
\usage{
timestamp(timestamp, pattern, tz)
ts(timestamp, pattern, tz)
}
\arguments{
  \item{timestamp}{Time stamp as string vector.}
  \item{pattern}{Conversion specification of time stamp as string (optional). See Details for usage.}
  \item{tz}{Optional character string specifying the time zone to be used for the conversion. System-specific (see \code{\link{as.POSIXlt}}), but \code{""} is the current time zone (used as default). Use \code{"?"} to check \code{timestamp} for time zone abbreviation.}
}
\details{
If the time stamp is already formatted as \code{POSIXlt}, the usage of \code{timestamp} is not necessary. \code{\link{strptime}} can also be used to create an applicable time stamp. Usage of \code{timestamp} is recommeded, since it checks the created time stamp, thus faulty time stamps are avoided.

\emph{Pattern}

A conversion specification is introduced by "\%", usually followed by a single letter. Any character in the format string not part
of a conversion specification is interpreted literally. Widely implemented conversion specifications include:
\itemize{
  \item \code{\%d}: day of the month as decimal number (01--31)
  \item \code{\%m}: month as decimal number (01--12)
  \item \code{\%y}: year without century (00--99), where values 00--68 are prefixed by 20 and 69--99 by 19
  \item \code{\%Y}: year with century
  \item \code{\%H}: hour as decimal number (00--23)
  \item \code{\%M}: minute as decimal number (00--59)
  \item \code{\%S}: second as decimal number (00--61)
}
For details see \code{\link{strptime}}.
}
\value{Returns a \code{POSIXlt} vector.}
\author{Christian Graul}
\seealso{\code{\link{POSIXlt}}, \code{\link{strptime}}, \code{\link{createMast}}}
\examples{
\dontrun{
# load and prepare data
data(winddata)

# format time stamp
timestamp <- timestamp(timestamp=winddata[,1])

# format time stamp with given pattern
timestamp.2 <- timestamp(timestamp=winddata[,1], "\%d.\%m.\%Y \%H:\%M")

# wrong pattern (\%y)
timestamp.2 <- timestamp(timestamp=winddata[,1], "\%d.\%m.\%y \%H:\%M")

# strange time stamp pattern
ts <- c("TS 08/2012-10 8h10m30s", "TS 08/2012-10 8h20m30s", 
  "TS 08/2012-10 8h30m30s")
timestamp.3 <- timestamp(timestamp=ts)	# pattern not found
timestamp.3 <- timestamp(timestamp=ts, "TS \%m/\%Y-\%d \%Hh\%Mm\%Ss")

# time zone
timestamp.4 <- timestamp(timestamp=winddata[,1], tz="CEST")
timestamp.5 <- timestamp(timestamp="2012-08-08 22:55 GMT", tz="?")
}
}
\keyword{methods}
