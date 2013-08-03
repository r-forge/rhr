\name{rhrKDE}
\alias{rhrKDE}
\title{Kernel Density Estimation (KDE)}
\usage{
  rhrKDE(xy, xrange = NA, yrange = NA, res = 100,
    ud = TRUE, cud = TRUE, levels = 95, h = "href",
    lscv.n = 100, tol = 1)
}
\arguments{
  \item{xy}{data.frame with two columns. The first column
  contains x coordinaes and the second column contains y
  coordinates}

  \item{levels}{a vector with the percentage of closest
  points to the centroid that are used to calculated MCP}

  \item{ud}{a logical value, whether or not a utilitisation
  distribution should be calculated}

  \item{cud}{a logical value, whether or not a cumulative
  utilitisation distribution should be calculated}

  \item{xrange}{vector of length 2, with xmin and xmax for
  the UD}

  \item{yrange}{vector of length 2, with ymin and ymax for
  the UD}

  \item{res}{the resolution for the ud.}

  \item{h}{bandwidth, either a string ("href", "lscv") or
  an actual value}

  \item{lscv.n}{number of iterations for lscv}

  \item{tol}{the tolerace for lscv}
}
\value{
  object of class \code{RhrHREstimator}
}
\description{
  Kernel Density Estimation (KDE)
}
\examples{
data(datSH)
\dontrun{
# Kernel with href bandwidth estimation
k1 <- rhrKDE(datSH[, 2:3], h="href", resolution=100)

# what is the actually estimated bandwidth?
k1$h$h

# Return results as an object of class raster
k1 <- rhrKDE(datSH[, 2:3], h="href", resolution=100, return.raster=TRUE)
k1$data$rast
plot(k1$data$rast)

# Kernel with href bandwidth estimation
k2 <- rhrKDE(datSH[, 2:3], h="lscv", resolution=100)

# what is the actually estimated bandwidth?
k2$h$h

# Return results as an object of class raster
k2 <- rhrKDE(datSH[, 2:3], h="lscv", resolution=100, return.raster=TRUE)
k1$data$rast
plot(k1$data$rast)

# Compare
par(mfrow=c(2,1))
plot(k1)
plot(k2)}
}
\author{
  Johannes Signer
}

