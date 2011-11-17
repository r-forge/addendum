\name{fit.lognet}
\alias{fit.lognet}
\title{Tailormade weighted fitting of logistic glmnet}
\usage{
  fit.lognet(dfr, resp, lambda, weights = (rep(1,
  dim(dfr)[1])), verbosity = 0, standardize = FALSE,
  type.measure = NULL, dfrConvData, ...)
}
\arguments{
  \item{dfr}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}})}

  \item{resp}{outcome vector}

  \item{lambda}{(single) lambda to use}

  \item{weights}{weight vector per observation (does not
  have to sum to 1, and defaults to equal weights)}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{standardize}{if \code{TRUE} (NOT the default), all
  variables are standardized before fitting.}

  \item{type.measure}{one of the crossvalidating measures
  provided by \code{\link{glmnet}} or \code{NULL} (default)
  to not do crossvalidation}

  \item{dfrConvData}{premade return value of
  \code{\link{dfrConversionProbs}} for that \code{glmnet}
  and dataset}

  \item{\dots}{passed on to \code{\link{glmnet}} or
  \code{\link{cv.glmnet}}.}
}
\value{
  depending on \code{type.measure} being \code{NULL}, a
  \code{\link{glmnet}} or \code{\link{cv.glmnet}} object.
}
\description{
  Do dummy coding on dataset and fit logistic
  (crossvalidated) glmnet
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
lnet<-fit.lognet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1)
cv.lnet<-fit.lognet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1, type.measure="auc")
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{glmnet}}
}
\keyword{glmnet}
\keyword{LASSO}
\keyword{logistic}

