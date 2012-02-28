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
  Package\cr Version: \tab 1.1.2\cr Date: \tab
  2012-02-28\cr License: \tab GPL (>= 2)\cr LazyLoad: \tab
  yes\cr Depends: Matrix, glmnet, snowfall, addendum,
  NumDfr, GLoMo\cr} \itemize{ \item Important classes
  \itemize{ \item \code{\link{cv.1l.emlasso}} \item
  \code{\link{cv.1l.emlasso.reduced}} \item
  \code{\link{cv.emlasso}} } \item data objects for
  demonstrational purposes \itemize{ \item
  \code{\link{aDfr}} \item \code{\link{aDfr.MD}} \item
  \code{\link{eml17}} \item \code{\link{eml20}} \item
  \code{\link{emlcvfit}} \item \code{\link{y}} } \item
  Helpers \itemize{ \item
  \code{\link{calculateCriteria.EMLasso.1l.lognet}} \item
  \code{\link{checkConvergence.lognet}} \item
  \code{\link{EMLasso.1l.lognet.cv.param}} \item
  \code{\link{findReasonableLambdaHelper}} \item
  \code{\link{fit.lognet}} \item \code{\link{fit.logreg}}
  \item \code{\link{cv.logreg}} \item
  \code{\link{getBeta.cv.emlasso}} \item
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
  \item \code{\link{validationOutAndPredProb}} \item
  \code{\link{getMinMaxPosLikeGlmnet}} } \item Fitting /
  use \itemize{ \item \code{\link{coef.cv.1l.emlasso}}
  \item \code{\link{coef.cvpart.emlasso}} \item
  \code{\link{EMLasso.1l.lognet.cv}} \item
  \code{\link{EMLasso.1l.lognet}} \item
  \code{\link{EMLasso.lognet.cv}} \item
  \code{\link{reduce.cv.1l.emlasso}} \item
  \code{\link{reduce.EMLasso.lognet.cv}} \item
  \code{\link{fullDataEMLassoAndGLoMo}} } \item Diagnostics
  \itemize{ \item \code{\link{convergenceDiagnostics}}
  \item \code{\link{plotCoefConvergence}} \item
  \code{\link{differentModels}} } } Most important part of
  the call stack: \code{ \link{EMLasso.lognet.cv}
  ("EMLasso.lognet.cv") \link{EMLasso.1l.lognet.cv.param}
  \link{run.parallel} \link{do.parallel}
  \link{EMLasso.1l.lognet.cv} ("\link{cv.1l.emlasso}")
  \link{EMLasso.1l.lognet.onefold} ("cvpart.emlasso")
  \link{EMLasso.1l.lognet} ("EMLasso.1l.lognet") ->fitinfo
  \link{rCatsAndCntInDfr} \link{fit.lognet}
  ("\link{cv.glmnet}") \link{GLoMo} ("\link{GLoMo}")
  \link{predict.conditional.allrows.GLoMo}
  \link{checkConvergence.lognet} \link{fit.logreg}
  ("\link{cv.glmnet}") \link{predict.GLoMo}
  \link{mimic_cv_lognet}
  \link{postprocess.reduce.1l.emlasso}
  \link{reduce.cv.1l.emlasso}
  ("\link{cv.1l.emlasso.reduced}")
  \link{reduce.EMLasso.lognet.cv} ("\link{cv.emlasso}",
  "\link{cv.glmnet}") }
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Maintainer: Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\keyword{package}

