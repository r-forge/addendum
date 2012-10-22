\name{fit.logreg}
\alias{fit.logreg}
\alias{logregLikeGlmnet}
\title{Fit logistic regression by using logistic LASSO with lambda=0}
\usage{
  fit.logreg(ds, out, wts = rep(1, nrow(ds)),
    verbosity = 0, useCols = NULL, fakeLam,
    imputeDs2FitDsProperties = normalImputationConversion(),
    ..., type.measure = NULL, nfolds = 10, foldid)

  logregLikeGlmnet(x, y, useLambda = Inf, verbosity = 0,
    ...)
}
\arguments{
  \item{ds}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}})}

  \item{out}{outcome vector}

  \item{wts}{weight vector per observation (does not have
  to sum to 1, and defaults to equal weights)}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{useCols}{restrict the regression to only include
  these columns as predictors}

  \item{fakeLam}{optionally pass in the lambdas here}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}}

  \item{\dots}{passed on to
  \code{\link{glmnet}}/\code{\link{glm.fit}}. Not allowed:
  "x", "y", "family", ("weights", "lambda"), "standardize"}

  \item{type.measure}{see \code{\link{cv.glmnet}} if
  \code{NULL} (the default) no crossvalidation occurs
  similar to what happens in fit.glmnet}

  \item{nfolds}{see \code{\link{cv.glmnet}}}

  \item{foldid}{see \code{\link{cv.glmnet}}}

  \item{x}{model matrix (as supported by \code{glm})}

  \item{y}{outcomes (as supported by \code{glm})}

  \item{useLambda}{lambda item of the return value}
}
\value{
  \code{\link{glmnet}} object

  similar to the return value of \code{\link{glmnet}}
}
\description{
  Fit logistic regression by using logistic LASSO with
  lambda=0
}
\note{
  The warning in the old function pointed me to the fact
  that the weights are _not_ probability weights in a
  binomial glm!! So, as an alternative, we use glmnet with
  lambda=0 !!!
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
lreg<-fit.logreg(iris, y, wts=runif(nrow(iris)), verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{fit}
\keyword{glmnet}
\keyword{logistic}
\keyword{regression}

