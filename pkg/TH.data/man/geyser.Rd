% original file MASS/man/geyser.Rd
% copyright (C) 1994-2014 W. N. Venables and B. D. Ripley
%
\name{geyser}
\alias{geyser}
\usage{
geyser
}
\title{Old Faithful Geyser Data}
\description{
  A version of the eruptions data from the \sQuote{Old Faithful} geyser
  in Yellowstone National  Park,  Wyoming. This version comes from
  Azzalini and Bowman (1990) and is of continuous measurement from August
  1 to August 15, 1985.

  Some nocturnal duration measurements
  have originally been described as \sQuote{short}, \sQuote{medium}
  or \sQuote{long} and are given as interval censored observations in this
  version of the dataset.
}
\format{A data frame with 299 observations on 2 variables.
  \tabular{lll}{
    \code{duration}  \tab Surv  \tab Eruption time in mins \cr
    \code{waiting}   \tab numeric  \tab Waiting time for this eruption \cr
  }
}
\note{
  Variable \code{duration} was converted to a \code{Surv} object for
  representing interval censored nocturnal observations.
}
\seealso{
  \code{\link{faithful}}, \code{\link[MASS]{geyser}}.
}
\references{
  Azzalini, A. and Bowman, A. W. (1990) A look at some
  data on the Old Faithful geyser.  \emph{Applied Statistics}
  \bold{39}, 357--365.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.

}
\keyword{datasets}
