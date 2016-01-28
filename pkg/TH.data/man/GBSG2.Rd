\name{GBSG2}
\alias{GBSG2}
\non_function{}
\title{ German Breast Cancer Study Group 2 }
\usage{data("GBSG2")}
\description{
  A data frame containing the observations from the  GBSG2 study.
}
\format{
  This data frame contains the observations of 686 women:
  \describe{
    \item{horTh}{hormonal therapy, a factor at two levels \code{no} and
\code{yes}.}
    \item{age}{of the patients in years.}
    \item{menostat}{menopausal status, a factor at two levels \code{pre} 
(premenopausal) and \code{post} (postmenopausal).}
    \item{tsize}{tumor size (in mm).}
    \item{tgrade}{tumor grade, a ordered factor at levels \code{I < II <
III}.}
    \item{pnodes}{number of positive nodes.}
    \item{progrec}{progesterone receptor (in fmol).}
    \item{estrec}{estrogen receptor (in fmol).}
    \item{time}{recurrence free survival time (in days).}
    \item{cens}{censoring indicator (0- censored, 1- event).}
  }
}
\source{
  W. Sauerbrei and P. Royston (1999). Building multivariable prognostic
  and diagnostic models: transformation of the predictors by using 
  fractional polynomials. \emph{Journal of the Royal Statistics Society
  Series A}, Volume \bold{162}(1), 71--94.
}
\references{
  M. Schumacher, G. Basert, H. Bojar,  K. Huebner, M. Olschewski, 
  W. Sauerbrei, C. Schmoor, C. Beyerle, R.L.A. Neumann and H.F. Rauschecker
  for the German Breast Cancer Study Group (1994),
  Randomized \eqn{2\times2} trial evaluating hormonal treatment
  and the duration of chemotherapy in node-positive breast cancer patients.
  \emph{Journal of Clinical Oncology}, \bold{12}, 2086--2093.
}
\examples{
data(GBSG2)

thsum <- function(x) {
  ret <- c(median(x), quantile(x, 0.25), quantile(x,0.75))
  names(ret)[1] <- "Median"
  ret
}

t(apply(GBSG2[,c("age", "tsize", "pnodes", 
                 "progrec", "estrec")], 2, thsum))

table(GBSG2$menostat)
table(GBSG2$tgrade)
table(GBSG2$horTh)
}
\keyword{datasets}
