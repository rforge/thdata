\name{GlaucomaM}
\alias{GlaucomaM}
\non_function{}
\title{ Glaucoma Database }
\usage{data("GlaucomaM")}
\description{
The \code{GlaucomaM} data has 196 observations in two classes. 
62 variables are derived from a confocal laser scanning image of the 
optic nerve head, describing its morphology. Observations are from 
normal and glaucomatous eyes, respectively. 
}
\format{
  This data frame contains the following predictors describing the
  morphology of the optic nerve head and a membership variable:
  \describe{
    \item{ag}{area global.}
    \item{at}{area temporal.}
    \item{as}{area superior.}
    \item{an}{area nasal.}
    \item{ai}{area inferior.}
    \item{eag}{effective area global.}
    \item{eat}{effective area temporal.}
    \item{eas}{effective area superior.}
    \item{ean}{effective area nasal.}
    \item{eai}{effective area inferior.}
    \item{abrg}{area below reference global.}
    \item{abrt}{area below reference temporal.}
    \item{abrs}{area below reference superior.}
    \item{abrn}{area below reference nasal.}
    \item{abri}{area below reference inferior.}
    \item{hic}{height in contour.}
    \item{mhcg}{mean height contour global.}
    \item{mhct}{mean height contour temporal.}
    \item{mhcs}{mean height contour superior.}
    \item{mhcn}{mean height contour nasal.}
    \item{mhci}{mean height contour inferior.}
    \item{phcg}{peak height contour.}
    \item{phct}{peak height contour temporal.}
    \item{phcs}{peak height contour superior.}
    \item{phcn}{peak height contour nasal.}
    \item{phci}{peak height contour inferior.}
    \item{hvc}{height variation contour.}
    \item{vbsg}{volume below surface global.}
    \item{vbst}{volume below surface temporal.}
    \item{vbss}{volume below surface superior.}
    \item{vbsn}{volume below surface nasal.}
    \item{vbsi}{volume below surface inferior.}
    \item{vasg}{volume above surface global.}
    \item{vast}{volume above surface temporal.}
    \item{vass}{volume above surface superior.}
    \item{vasn}{volume above surface nasal.}
    \item{vasi}{volume above surface inferior.}
    \item{vbrg}{volume below reference global.}
    \item{vbrt}{volume below reference temporal.}
    \item{vbrs}{volume below reference superior.}
    \item{vbrn}{volume below reference nasal.}
    \item{vbri}{volume below reference inferior.}
    \item{varg}{volume above reference global.}
    \item{vart}{volume above reference temporal.}
    \item{vars}{volume above reference superior.}
    \item{varn}{volume above reference nasal.}
    \item{vari}{volume above reference inferior.}
    \item{mdg}{mean depth global.}
    \item{mdt}{mean depth temporal.}
    \item{mds}{mean depth superior.}
    \item{mdn}{mean depth nasal.}
    \item{mdi}{mean depth inferior.}
    \item{tmg}{third moment global.}
    \item{tmt}{third moment temporal.}
    \item{tms}{third moment superior.}
    \item{tmn}{third moment nasal.}
    \item{tmi}{third moment inferior.}
    \item{mr}{mean radius.}
    \item{rnf}{retinal nerve fiber thickness.}
    \item{mdic}{mean depth in contour.}
    \item{emd}{effective mean depth.}
    \item{mv}{mean variability.}
    \item{Class}{a factor with levels \code{glaucoma} and \code{normal}.}
  }
}
\details{
  All variables are derived from a laser scanning image of the eye background
taken by the Heidelberg Retina Tomograph. Most of the variables describe
either the area or volume in certain parts of the papilla and are measured
in four sectors (temporal, superior, nasal and inferior) as well as for the
whole papilla (global). The global measurement is, roughly, the sum of the
measurements taken in the four sector.

  The observations in both groups are matched by age and sex to prevent any
bias.
}
\source{
 Torsten Hothorn and Berthold Lausen (2003), Double-Bagging: Combining
classifiers by bootstrap aggregation. \emph{Pattern Recognition},
\bold{36}(6), 1303--1309. 

}
\keyword{datasets}
