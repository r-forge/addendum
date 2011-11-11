\name{fit.logreg}
\alias{fit.logreg}
\alias{logregLikeGlmnet}
\title{Fit logistic regression by using logistic LASSO with lambda=0}
\usage{
  fit.logreg(dfr, resp, wts = rep(1, nrow(dfr)), verbosity
  = 0, useCols = NULL, outName = "out", dfrConvData, ...)

  logregLikeGlmnet(x, y, outName = "out", useLambda = Inf,
  verbosity = 0, ...)
}
\arguments{
  \item{dfr}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}})}

  \item{resp}{outcome vector}

  \item{wts}{weight vector per observation (does not have
  to sum to 1, and defaults to equal weights)}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{useCols}{restrict the regression to only include
  these columns as predictors}

  \item{outName}{name that can be used safely for the
  outcome (i.e. a column name not present in \code{dfr})}

  \item{dfrConvData}{premade return value of
  \code{\link{dfrConversionProbs}} for that \code{glmnet}
  and dataset}

  \item{\dots}{passed on to
  \code{\link{glmnet}}/\code{\link{glm.fit}}. Not allowed:
  "x", "y", "family", ("weights", "lambda"), "standardize"}

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

