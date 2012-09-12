\name{fit.glmnet}
\alias{fit.glmnet}
\title{Tailormade weighted fitting of logistic glmnet}
\usage{
  fit.glmnet(ds, out, lambda,
    weights = (rep(1, dim(ds)[1])), verbosity = 0,
    standardize = FALSE, type.measure = NULL,
    imputeDs2FitDsProperties = normalImputationConversion(),
    family = "binomial", ..., reRestrictIfRelevant = TRUE)
}
\arguments{
  \item{ds}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}})}

  \item{out}{outcome vector}

  \item{lambda}{(single) lambda to use}

  \item{weights}{weight vector per observation (does not
  have to sum to 1, and defaults to equal weights)}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{standardize}{if \code{TRUE} (NOT the default), all
  variables are standardized before fitting.}

  \item{type.measure}{one of the crossvalidating measures
  provided by \code{\link{cv.glmnet}} or \code{NULL}
  (default) to not do crossvalidation}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}}

  \item{family}{see \code{\link{glmnet}}. Defaults to
  binomial (logistic regression)}

  \item{\dots}{passed on to \code{\link{glmnet}} or
  \code{\link{cv.glmnet}}.}

  \item{reRestrictIfRelevant}{\code{\link{cv.glmnet}}does
  not support passng a single \code{lambda} value.  If this
  was requested, a dummy extra \code{lambda} is added. When
  this value is \code{TRUE} (default), the results are
  afterwards restricted to the original \code{lambda}.}
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
lnet<-fit.glmnet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1)
cv.lnet<-fit.glmnet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1, type.measure="auc")
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{glmnet}}, \code{\link{imputeDs2FitDs}},
  \code{\link{EMLasso}}
}
\keyword{LASSO}
\keyword{glmnet}
\keyword{logistic}

