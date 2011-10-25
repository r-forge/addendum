

#' EM (logistic) LASSO
#' 
#' Fit (logistic) LASSO when there is missing data in the predictors
#' 
#' \tabular{ll}{ Package: \tab EMLasso\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2011-10-07\cr License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr Depends: Matrix, glmnet, snowfall, addendum, NumDfr, GLoMo\cr} 
#' \enumerate{
#'	\item Important classes \enumerate{
#'		\item \code{\link{cv.1l.emlasso}}
#'		\item \code{\link{cv.1l.emlasso.reduced}}
#'		\item \code{\link{cv.emlasso}}
#'	}
#'	\item Helpers \enumerate{
#'		\item \code{\link{calculateCriteria.EMLasso.1l.lognet}}
#'		\item \code{\link{checkConvergence.lognet}}
#'		\item \code{\link{EMLasso.1l.lognet.cv.param}}
#'		\item \code{\link{findReasonableLambdaHelper}}
#'		\item \code{\link{fit.lognet}}
#'		\item \code{\link{fit.logreg}}
#'		\item \code{\link{cv.logreg}}
#'		\item \code{\link{getBeta.cv.emlasso}}
#'		\item \code{\link{getSortedReducedResultList}}
#'		\item \code{\link{length.EMLasso.1l.lognet.cv.param}}
#'		\item \code{\link{lognetProbWrapper}}
#'		\item \code{\link{LognetValidationData}}
#'		\item \code{\link{mimic_cv_lognet}}
#'		\item \code{\link{postprocess.reduce.1l.emlasso}}
#'		\item \code{\link{predictProb.lognetProbWrapper}}
#'		\item \code{\link{predictProb}}
#'		\item \code{\link{refreshCriteria.cv.1l.emlasso}}
#'		\item \code{\link{refreshCriteria.cvpart.emlasso}}
#'		\item \code{\link{refreshCriteria}}
#'		\item \code{\link{setDebugmodeEMLasso}}
#'		\item \code{\link{simpleplot.cv.emlasso}}
#'		\item \code{\link{sortReducedResultList}}
#'		\item \code{\link{validateFunction.lognet}}
#'		\item \code{\link{validationOutAndPredProb.cv.1l.emlasso}}
#'		\item \code{\link{validationOutAndPredProb.cvpart.emlasso}}
#'		\item \code{\link{validationOutAndPredProb}}
#'		\item \code{\link{getMinMaxPosLikeGlmnet}}
#'	}
#'	\item Fitting / use \enumerate{
#'		\item \code{\link{coef.cv.1l.emlasso}}
#'		\item \code{\link{coef.cvpart.emlasso}}
#'		\item \code{\link{EMLasso.1l.lognet.cv}}
#'		\item \code{\link{EMLasso.1l.lognet}}
#'		\item \code{\link{EMLasso.lognet.cv}}
#'		\item \code{\link{reduce.cv.1l.emlasso}}
#'		\item \code{\link{reduce.EMLasso.lognet.cv}}
#'		\item \code{\link{fullDataEMLassoAndGLoMo}}
#'	}
#'}
#' Most important part of the call stack:
#' \itemize{
#' \item \code{\link{EMLasso.lognet.cv}} ("EMLasso.lognet.cv")\itemize{
#'   \item \code{\link{EMLasso.1l.lognet.cv.param}}
#'   \item \code{\link{run.parallel}}\itemize{
#'     \item \code{\link{do.parallel}}\itemize{
#'       \item \code{\link{EMLasso.1l.lognet.cv}} ("\code{\link{cv.1l.emlasso}}")\itemize{
#'         \item \code{\link{EMLasso.1l.lognet.onefold}} ("cvpart.emlasso")\itemize{
#'           \item \code{\link{EMLasso.1l.lognet}} ("EMLasso.1l.lognet") ->fitinfo\itemize{
#'             \item \code{\link{rCatsAndCntInDfr}}
#'             \item \code{\link{fit.lognet}} ("\code{\link{cv.glmnet}}")
#'             \item \code{\link{GLoMo}} ("\code{\link{GLoMo}}")
#'             \item \code{\link{predict.conditional.allrows.GLoMo}}
#'             \item \code{\link{checkConvergence.lognet}}
#'             \item \code{\link{fit.logreg}} ("\code{\link{cv.glmnet}}")
#'             }
#'           \item \code{\link{predict.GLoMo}}
#'           }
#'         \item \code{\link{mimic_cv_lognet}}
#'         }
#'       \item \code{\link{postprocess.reduce.1l.emlasso}}\itemize{
#'         \item \code{\link{reduce.cv.1l.emlasso}} ("\code{\link{cv.1l.emlasso.reduced}}")
#'         }
#'       }
#'     }
#'   }
#' \item \code{\link{reduce.EMLasso.lognet.cv}} ("\code{\link{cv.emlasso}}", "\code{\link{cv.glmnet}}")
#' }
#' @name EMLasso-package
#' @aliases EMLasso-package EMLasso
#' @docType package
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' 
#' Maintainer: Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @references [PENDING]
#' @keywords package
#' @import Matrix glmnet addendum NumDfr GLoMo 
NULL



