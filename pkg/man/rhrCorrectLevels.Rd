\name{rhrCorrectLevels}
\alias{rhrCorrectLevels}
\title{rhrCorrectLevels}
\usage{
  rhrCorrectLevels(str, low = 1, upper = 100,
    default.level = 95)
}
\arguments{
  \item{str}{A string that contains the level, seperated by
  comas (usually provided through the GUI). If there are no
  suitable levels, then the default.level is used.}

  \item{low}{lower bound for levels}

  \item{upper}{upperbound for levels}

  \item{default.level}{is the default level}
}
\description{
  correct levels at which a home range should be calculated
}

