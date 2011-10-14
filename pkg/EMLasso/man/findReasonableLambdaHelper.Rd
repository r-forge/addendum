\name{findReasonableLambdaHelper}
\alias{"[.LambdaHelper"}
\alias{[.LambdaHelper}
\alias{findReasonableLambdaHelper}
\title{Function to run on a dataset with not too much missing data to identify a set of reasonable lambda values}
\usage{
  findReasonableLambdaHelper(ds, out, showFirst = 20,
  showPlot = TRUE, type.measure = "auc",
  repsNeededForFirstOccurrence = 3, verbosity = 0)

  \method{[}{LambdaHelper}(object, i, j, drop = TRUE)
}
\arguments{
  \item{ds}{dataset to investigate}

  \item{out}{outcome vector}

  \item{showFirst}{show the top coefficients (first
  \code{showFirst} occurring)}

  \item{showPlot}{if \code{TRUE} (the default), visually
  supports the decision}

  \item{type.measure}{see \code{\link{cv.glmnet}}}

  \item{repsNeededForFirstOccurrence}{How many times (i.e.
  for how many lambda values) must a coefficient be
  consecutively nonzero before we count it as "occurring"}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{object}{\code{LambdaHelper}}

  \item{i}{row index}

  \item{j}{column index. If this is missing, the \code{i}th
  lambda is returned}

  \item{drop}{if \code{TRUE} the result is coerced to the
  simplest structure possible}
}
\value{
  list of class "LambdaHelper": \item{topres
  }{\code{data.frame} with \code{showFirst} rows, and
  columns: \code{variable} (name),
  \code{lambda},\code{critl} (lower bound of criterion),
  \code{crit} (estimate of criterion), \code{critu} (upper
  bound of criterion), \code{critsd} (sd of criterion),
  \code{index} (at which lambda index does this variable
  first occur)} \item{allLambda }{vector of lambda values}

  depends on the parameters
}
\description{
  Expects a singly imputed dataset and fits a logistic
  LASSO so the user can pick a set that wil probably be
  interesting.
}
\note{
  EMLasso is pretty heavy and has to be run per lambda.
  This functions helps preselect some lambda values, and
  can typically avoid useless calculations for
  non-interesting lambda values.
}
\examples{
data(emlcvfit, package="EMLasso")
rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
data(emlcvfit, package="EMLasso")
rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
rlh[1]
rlh[1:5, NULL]
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{glmnet}
\keyword{lambda}
