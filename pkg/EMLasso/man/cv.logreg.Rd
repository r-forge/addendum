\name{cv.logreg}
\alias{cv.logreg}
\title{Crossvalidate logistic regression by using logistic LASSO with lambda=0}
\usage{
  cv.logreg(dfr, resp, wts = rep(1, nrow(dfr)), verbosity =
  0, useCols = NULL, outName = "out", dfrConvData, nfolds =
  10, type.measure = "auc")
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

  \item{nfolds}{number of folds}

  \item{type.measure}{see \code{\link{cv.glmnet}}}
}
\value{
  \code{\link{cv.glmnet}} object
}
\description{
  Crossvalidate logistic regression by using logistic LASSO
  with lambda=0
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
cvlreg<-cv.logreg(iris, y, wts=runif(nrow(iris)), verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{fit}
\keyword{logistic}
\keyword{regression}

