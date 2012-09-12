\name{EMLasso.1l}
\alias{EMLasso.1l}
\alias{EMLasso1l}
\alias{EMLasso1l-class}
\title{Fit EMLasso for 1 lambda}
\usage{
  EMLasso.1l(ds, out, lambda, nrOfSamplesPerMDRow = 10,
    rowsToUseForFit = seq(nrow(ds)),
    firstTimeCompleter = marginalCompleter(),
    imputeDs2FitDsProperties = normalImputationConversion(),
    fitPredictor = GLoMo, family = "binomial",
    convergenceChecker = convergenceCheckCreator(),
    postProcess = postProcessEMLasso1l, verbosity = 0,
    extraLog = function(...) { })
}
\arguments{
  \item{ds}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}}) to fit it to}

  \item{out}{outcome vector}

  \item{lambda}{the single lambda}

  \item{nrOfSamplesPerMDRow}{For rows with missing data,
  how many rows to sample. Defaults to 10.}

  \item{rowsToUseForFit}{Which of the rows of dfr/out can
  be used for fitting the LASSO (int vector). Defaults to
  all rows.}

  \item{firstTimeCompleter}{object supported by
  \code{\link{completeMarginal}} or of similar form that
  will complete the dataset the first time. Defaults to
  \code{\link{marginalCompleter}} with default parameters.}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} object that will provide the
  conversion from imputed dataset to one that is ready for
  fitting the predictor model}

  \item{fitPredictor}{function that will perform the
  predictor model fit. Defaults to \code{\link{GLoMo}} and
  should be of this signature.}

  \item{family}{see \code{\link{glmnet}}. Defaults to
  "binomial" (i.e. lasso penalized logistic regression).}

  \item{convergenceChecker}{function that will check based
  on the coefficients over the repeats whether convergence
  has been achieved.  Defaults to
  \code{\link{convergenceCheckCreator}()}, so a wrapper
  around \code{\link{checkConvergence.glmnet}}. The
  function should have parameters \code{coefs} and
  \code{verbosity=0}}

  \item{postProcess}{function, like
  \code{\link{postProcessEMLasso1l}} (its default) and of
  that signature, to do some extra work before returning
  the result of \code{EMLasso.1l}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{extraLog}{A function (by default one that does
  nothing) that will be used in each iteration to perform
  extra logging}
}
\value{
  An object of class EMLasso1l. This is a list with the
  following items: \itemize{ \item \code{lasso.fit}: glmnet
  object \item \code{predictorModel}: final predictor fit
  (typically of class GLoMo) \item \code{coefs}: coefs for
  all iterations (rows) and dummycoded columnames
  (columns), incl. (intercept).  \item \code{ds}: original
  dataset passed along \item \code{out}: outcome variable
  (1 for each row in ds) \item \code{lambda}: 1 lambda
  value \item \code{nrOfSamplesPerMDRow}: how many
  imputations per row with missing data \item
  \code{convergence}: return value of last call to
  \code{convergenceChecker}. Will typically contain
  information like \code{minIt} and \code{maxIt}.  \item
  \code{rowsToUseForFit}: which of the rows in ds was used
  to fit the lasso \item \code{iterCount}: how many
  iterations occurred before convergence / maxIt \item
  \code{imputeDs2FitDsProperties}: as passed in, but first
  fed to \code{\link{imputeDs2FitDsProps}} \item any other
  items added by \code{postProcess} }
}
\description{
  Fit EMLasso for 1 lambda
}
\examples{
aDfr<-generateTypicalIndependentDfr(numCat=10, numCnt=10, numObs=100, catProbs=rep(1/3,3),
rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=1)

outlins<- -mean(aDfr$cnt1)+aDfr$cnt1+2*(aDfr$cat1=="b")
outprobs<-expit(outlins)
y<-factor(sapply(outprobs, function(prob){sample(c("no", "yes"), 1, prob=c(1-prob,prob))}))

aDfr.MD<-randomNA(aDfr, 0.01, verbosity=1)
rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
aLam<-rlh$regionDfr[2,"lambda"]
emlfit1<-EMLasso.1l(aDfr.MD, y, lambda=aLam, nrOfSamplesPerMDRow=7,
	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10), verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\keyword{EMLasso}
\keyword{fit}

