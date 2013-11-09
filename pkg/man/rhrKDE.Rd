\name{rhrKDE}
\alias{rhrKDE}
\title{Kernel Density Estimation (KDE)}
\usage{
  rhrKDE(xy, levels = 95, h = "href", xrange = NULL,
    yrange = NULL, increaseExtent = NULL, trast = NULL,
    buffer = NULL, ud = TRUE, cud = TRUE, res = NULL,
    rescale = "unitvar", lscvSearch = NULL,
    lscvWhichMin = "global", gridsize = NULL)
}
\arguments{
  \item{xy}{\code{data.frame} with two columns: x and y
  coordinates.}

  \item{levels}{numeric vector with the isopleth levels.}

  \item{h}{character ("href" or "hlscv") specifying the
  method to estimate the bandwidth or numeric value
  specifying the bandwidth.}

  \item{xrange}{numeric vector specifying min and max x for
  the output grid.}

  \item{yrange}{numeric vector specifying min and max y for
  the output grid.}

  \item{increaseExtent}{numeric value by which the x and y
  range are extended, see also \code{?extendrange}.}

  \item{trast}{a \code{RasterLayer} used as an template for
  the output grid.}

  \item{buffer}{numeric value to buffer the bounding box of
  points in map units from which the extent of the output
  grid will be retrieved.}

  \item{res}{numeric value specifying the resolution for
  the output grid.}

  \item{gridsize}{a vector of length 2, specifying the
  number of rows and the number of columns for the ouput
  grid.}

  \item{ud}{logical value, indicating whether or not a
  utilization distribution should be calculated.}

  \item{cud}{logical value, indicating whether or not a
  cumulative utilization distribution should be
  calculated.}

  \item{rescale}{character value specifying if the data
  should be rescaled before calculating bandwidth. Possible
  values are: \code{unitvar} to rescale to unit variance,
  \code{unitx} to rescale data to variance of x and
  \code{none}.}

  \item{lscvSearch}{numeric vector of length 2, specifying
  lower and uper bound for candidate bandwidth (as portion
  of reference bandwidth) for estimating bandwidth with
  least squre cross validation.}

  \item{lscvWhichMin}{character value, specifying how
  candidate bandwidths are chosen with least squre cross
  validation. Possible values are: \code{global} or
  \code{local} minimum.}
}
\value{
  object of class \code{RhrHREstimator}
}
\description{
  A function to estimate home ranges with kernel density
  estimation.
}
\details{
  The size and resolution of the resulting utilization
  distribution (UD) grid is influenced by \code{traster,
  xrange, yrange, increaseExtent, buffer, res, gridsize}.
  The size of the grid can be set either through a template
  raster (\code{traster}), \code{xrange} and \code{yrange}
  or \code{increaseExtent}. \code{traster} takes precedence
  over \code{xrange} and \code{yrange}, \code{buffer} and
  \code{grid}. If none of the previous arguments are
  provided, \code{xrange} and \code{yrange} are taken from
  the data.

  The resolution of the resulting UD grid can be set
  through either \code{res} or \code{gridsize}. \code{res}
  takes precedence over \code{gridsize}. If none of the
  previous arguments is provided the grid is set by default
  to a 100 by 100 grid. The bandwidth can be provided by
  the user or estimated through the reference bandwidth
  (this method is often refered to as the ad hoc method),
  plug in the euqtion method or the least square cross
  validation method. Reference bandwidth estimation is
  implemented as suggested by Silverman 1986. Plugin the
  equation method is wrapped from \code{KernSmooth::dpki}
  and a simple binned version Silverman's suggestion for
  least square cross validation is implemented. Kernels
  densities are estimated with \code{KernSmooth::bkde2d}.
  This is a binned approximation of 2D kernel density
  estimates (see \code{?KernSmooth::bkde2d} for more
  details.
}
\examples{
data(datSH)
\dontrun{
# Kernel with href bandwidth estimation
k1 <- rhrKDE(datSH[, 2:3], h="href", res=100)
plot(k1)

# what is the actually estimated bandwidth?
k1$parameters$h

# Kernel with href bandwidth estimation
k2 <- rhrKDE(datSH[, 2:3], h="lscv", res=100)
plot(k2)

# what is the actually estimated bandwidth?
k2$parameters$h
}
}
\author{
  Johannes Signer
}
\seealso{
  \code{KernSmooth::bkde2d}, \code{KernSmooth::dpik},
  \code{rhr::rhrHref}, \code{rhr::rhrHlscv},
  \code{rhr::rhrHpi}
}

