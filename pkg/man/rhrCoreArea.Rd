\name{rhrCoreArea}
\alias{rhrCoreArea}
\alias{rhrCoreArea.RhrHREstimator}
\title{Calcualte core area}
\usage{
  rhrCoreArea(x, method = "seaman90", ...)

  rhrCoreArea.RhrHREstimator(x, method = "powell90", ...)
}
\arguments{
  \item{x}{a RhrEstimator* object}

  \item{method}{method used to calculate core area. At the
  moment only powell90 is implemented}

  \item{...}{none implemented}
}
\value{
  object of class RhrHRCoreArea
}
\description{
  A function to calculate the core area of an estimated
  home range
}
\note{
  Core areas are areas within an animals home range that
  receive greater intensity of use than other areas. Core
  areas are often used to answer questions of
  territoriality or habitat selection. Oftentimes core
  areas are estimated by calculating home range at a level
  of 20 or 50%, i.e. determine a cut off value of the UD
  Laver 2008. This approach neglects differences by
  individuals.

  Seaman 1990 presented an area independent method for core
  area estimation, that does not rely on a predefined
  isopleth cut off values. This methods estimates the core
  area by estimating two values for each grid cell of the
  study area. First for each grid cell the number of
  relocations within each grid cell is counted. In the next
  step the following two values are calculated for each
  cell: \enumerate{ \item Percentage of maximum relative
  frequency. The grid cell with the highest relative
  frequency of relocations is assigned a value of 100 %
  \item Percent of home range, that is the percentage of
  pixels with a higher relative frequency of relocations.
  The percentage of maximum relative frequency is then
  plotted against the percent of home range (Figure
  [fig:ca1]). The point with the greatest distance to the
  line with intercept 1 and slope -1 is used as threshold.
  }
}
\examples{
\dontrun{
est <- rhrKDE(datSH[, 2:3])
ca <- rhrCoreArea(est)
plot(ca)
}
}
\references{
  Peter N. Laver and Marcella J. Kelly. A critical review
  of home range studies. The Journal of Wildlife
  Management, 72(1):290-298, 2008

  D. Erran Seaman and Roger A. Powell. Identifying patterns
  and intensity of home range use. Bears: their biology and
  management, 243-249, 1990
}

