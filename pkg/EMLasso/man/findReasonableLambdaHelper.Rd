\name{findReasonableLambdaHelper}
\alias{[.LambdaHelper}
\alias{findReasonableLambdaHelper}
\alias{getLambdas}
\alias{getLambdas.LambdaHelper}
\alias{getLambdas.lambdaregion}
\title{Function to run on a dataset with not too much missing data to identify a set of reasonable lambda values}
\usage{
  findReasonableLambdaHelper(ds, out, family = "binomial",
    showFirst = 20, showPlot = TRUE, type.measure = "auc",
    repsNeededForFirstOccurrence = 3, ..., verbosity = 0,
    minNumHigher = 20, minNumLower = 20, maxNumLower = 30,
    imputeDs2FitDsProperties = normalImputationConversion(),
    standardize = FALSE)

  \method{[}{LambdaHelper}(object, i, j, drop = TRUE)

  getLambdas(x, ...)

  \method{getLambdas}{lambdaregion}(x, ...)

  \method{getLambdas}{LambdaHelper}(x, ...)
}
\arguments{
  \item{ds}{dataset to investigate}

  \item{out}{outcome vector}

  \item{family}{see \code{\link{glmnet}}. Defaults to
  "binomial" (i.e. lasso penalized logistic regression).}

  \item{showFirst}{show the top coefficients (first
  \code{showFirst} occurring)}

  \item{showPlot}{if \code{TRUE} (the default), visually
  supports the decision}

  \item{type.measure}{see \code{\link{cv.glmnet}}}

  \item{repsNeededForFirstOccurrence}{How many times (i.e.
  for how many lambda values) must a coefficient be
  consecutively nonzero before we count it as "occurring"}

  \item{\dots}{passed on to \code{\link{plotex}} (if
  relevant)}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{minNumHigher}{How many lambdas higher than the
  optimum do you minimally want (if available)}

  \item{minNumLower}{How many lambdas lower than the
  optimum do you minimally want (if available)}

  \item{maxNumLower}{How many lambdas lower than the
  optimum do you maximally want}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}}

  \item{standardize}{see \code{\link{glmnet}}. Defaults to
  FALSE.}

  \item{object}{\code{LambdaHelper}}

  \item{i}{row index}

  \item{j}{column index. If this is missing, the \code{i}th
  lambda is returned}

  \item{drop}{if \code{TRUE} the result is coerced to the
  simplest structure possible}

  \item{x}{object to find 'interesting' set of lambdas for}
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
  \item{regionDfr }{\code{data.frame} w 3 rows 3 columns:
  \code{name} (values: "lower lambda", "optimum", and
  "higher lambda"), \code{idx} and \code{lambda}}
  \item{regionOfInterestData }{see
  \code{\link{getMinMaxPosLikeGlmnet}}}

  depends on the parameters

  vector of lambda values, normally high to low
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
aDfr<-generateTypicalIndependentDfr(numCat=10, numCnt=10, numObs=100, catProbs=rep(1/3,3),
rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=1)

outlins<- -mean(aDfr$cnt1)+aDfr$cnt1+2*(aDfr$cat1=="b")
outprobs<-expit(outlins)
y<-factor(sapply(outprobs, function(prob){sample(c("no", "yes"), 1, prob=c(1-prob,prob))}))

rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
data(emlcvfit, package="EMLasso")
rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
rlh[1]
rlh[1:5, NULL]
data(emlcvfit, package="EMLasso")
rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
getLambdas(rlh$regionOfInterestData)
data(emlcvfit, package="EMLasso")
rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
getLambdas(rlh)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{glmnet}
\keyword{lambda}

