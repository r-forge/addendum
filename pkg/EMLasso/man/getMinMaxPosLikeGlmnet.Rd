\name{getMinMaxPosLikeGlmnet}
\alias{getMinMaxPosLikeGlmnet}
\alias{getMinMaxPosLikeGlmnet.cv.glmnet}
\alias{getMinMaxPosLikeGlmnet.default}
\title{Find a region of interest in a set of lambdas given criteria and their SE}
\usage{
  getMinMaxPosLikeGlmnet(object, ...)

  \method{getMinMaxPosLikeGlmnet}{default}(object, cvm,
  cvsd, type.measure,...)

  \method{getMinMaxPosLikeGlmnet}{cv.glmnet} (object, ...)
}
\arguments{
  \item{object}{(main) object to get the region information
  from (for \code{default}: vector of lambda values)}

  \item{\dots}{for flexibility in 'derived' implementation}

  \item{cvm}{criteria value (see \code{\link{cv.glmnet}})}

  \item{cvsd}{criteria value standard errors (see
  \code{\link{cv.glmnet}})}

  \item{type.measure}{see \code{\link{cv.glmnet}}. If this
  is "auc", the \code{cvm} are negated before continuing.}
}
\value{
  list of class "lambdaregion": \item{pos.optimum
  }{position of the optimum criterion in the set of
  lambdas} \item{pos.higherlambda }{position in the set of
  lambdas of the highest lambda with criterion within one
  se of the optimum} \item{pos.lowerlambda1 }{position in
  the set of lambdas of the lambda below that of the
  optimum, that has the inverted optimum}
  \item{pos.lowerlambda2 }{position in the set of lambdas
  of the highest lambda below that of the optimum that does
  not have its criterion within one se of the optimum}
  \item{lambda }{vector of lambda values (as passed in)}
}
\description{
  Find a region of interest in a set of lambdas given
  criteria and their SE
}
\examples{
y<-sample(0:1, nrow(iris), replace=TRUE)
cvl<-fit.lognet(dfr=iris, resp=y, lambda=NULL, verbosity=10, type.measure="auc")
getMinMaxPosLikeGlmnet(cvl$lambda, cvl$cvm, cvl$cvsd, type.measure="auc")
y<-sample(0:1, nrow(iris), replace=TRUE)
cvl<-fit.lognet(dfr=iris, resp=y, lambda=NULL, verbosity=10, type.measure="auc")
getMinMaxPosLikeGlmnet(cvl)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{cv.glmnet}
\keyword{lambda}

