\name{rhrKDE}
\alias{rhrKDE}
\title{Kernel Density Estimation (KDE)}
\usage{
  rhrKDE(xy, h = "href", xrange = NA, yrange = NA,
    resolution = 100, lscv.n = 100, tol = 1,
    return.raster = FALSE)
}
\arguments{
  \item{xy}{data.frame with two columns. The first column
  contains x coordinaes and the second column contains y
  coordinates}

  \item{h}{bandwidth, either a string ("href", "lscv") or
  an actual value}

  \item{xrange}{the xrange for the out grid}

  \item{yrange}{the yrange for the out grid}

  \item{resolution}{the resolution for the out grid}

  \item{lscv.n}{number of iterations for lscv}

  \item{tol}{the tolerace for lscv}

  \item{return.raster}{returns the hat matrix with kernel
  densities as an object of class raster}
}
\value{
  object of class \code{RhrEstimatorKDE}
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

