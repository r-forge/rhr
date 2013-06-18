\name{ttestGrob}
\alias{ttestGrob}
\title{ttestGrob}
\usage{
  ttestGrob(x)
}
\arguments{
  \item{x}{a htest}
}
\description{
  Create a grid representation for an object of class htest
}
\examples{
grid.newpage()
pushViewport(viewport())
grid.draw(ttestGrob(t.test(1:10, mu=5)))
popViewport()
}
\author{
  Johannes Signer
}

