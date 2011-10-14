\docType{package}
\name{EMLasso-package}
\alias{"EMLasso-package"}
\alias{EMLasso}
\alias{EMLasso-package}
\title{EM (logistic) LASSO}
\description{
  Fit (logistic) LASSO when there is missing data in the
  predictors
}
\details{
  \tabular{ll}{ Package: \tab EMLasso\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2011-10-07\cr
  License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr
  Depends: Matrix, glmnet, snowfall, addendum, NumDfr,
  GLoMo\cr} \enumerate{ \item Important classes \enumerate{
  \item \code{\link{cv.1l.emlasso-class}} \item
  \code{\link{cv.1l.emlasso.reduced-class}} \item
  \code{\link{cv.emlasso-class}} } \item Helpers
  \enumerate{ \item
  \code{\link{calculateCriteria.EMLasso.1l.lognet}} \item
  \code{\link{checkConvergence.lognet}} \item
  \code{\link{EMLasso.1l.lognet.cv.param}} \item
  \code{\link{findReasonableLambdaHelper}} \item
  \code{\link{fit.lognet}} \item \code{\link{fit.logreg}}
  \item \code{\link{getBeta.cv.emlasso}} \item
  \code{\link{getSortedReducedResultList}} \item
  \code{\link{length.EMLasso.1l.lognet.cv.param}} \item
  \code{\link{lognetProbWrapper}} \item
  \code{\link{LognetValidationData}} \item
  \code{\link{mimic_cv_lognet}} \item
  \code{\link{postprocess.reduce.1l.emlasso}} \item
  \code{\link{predictProb.lognetProbWrapper}} \item
  \code{\link{predictProb}} \item
  \code{\link{refreshCriteria.cv.1l.emlasso}} \item
  \code{\link{refreshCriteria.cvpart.emlasso}} \item
  \code{\link{refreshCriteria}} \item
  \code{\link{setDebugmodeEMLasso}} \item
  \code{\link{simpleplot.cv.emlasso}} \item
  \code{\link{sortReducedResultList}} \item
  \code{\link{validateFunction.lognet}} \item
  \code{\link{validationOutAndPredProb.cv.1l.emlasso}}
  \item
  \code{\link{validationOutAndPredProb.cvpart.emlasso}}
  \item \code{\link{validationOutAndPredProb}} } \item
  Fitting / use \enumerate{ \item
  \code{\link{coef.cv.1l.emlasso}} \item
  \code{\link{coef.cvpart.emlasso}} \item
  \code{\link{EMLasso.1l.lognet.cv}} \item
  \code{\link{EMLasso.1l.lognet}} \item
  \code{\link{EMLasso.lognet.cv}} \item
  \code{\link{reduce.cv.1l.emlasso}} \item
  \code{\link{reduce.EMLasso.lognet.cv}} } } Most important
  part of the call stack: EMLasso.lognet.cv
  ("EMLasso.lognet.cv") EMLasso.1l.lognet.cv.param
  run.parallel do.parallel EMLasso.1l.lognet.cv
  ("cv.1l.emlasso") EMLasso.1l.lognet.onefold
  ("cvpart.emlasso") EMLasso.1l.lognet
  ("EMLasso.1l.lognet") ->fitinfo rCatsAndCntInDfr
  fit.lognet ("cv.glmnet") GLoMo ("GLoMo")
  predict.conditional.allrows.GLoMo checkConvergence.lognet
  fit.logreg ("cv.glmnet") predict.GLoMo mimic_cv_lognet
  postprocess.reduce.1l.emlasso reduce.cv.1l.emlasso
  ("cv.1l.emlasso.reduced") reduce.EMLasso.lognet.cv
  ("cv.emlasso", "cv.glmnet")
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Maintainer: Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\keyword{package}

