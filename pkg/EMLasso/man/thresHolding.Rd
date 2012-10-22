\name{thresHolding}
\alias{thresHolding}
\title{Simple threshold selection for some sets of variables selected by EMLasso}
\usage{
  thresHolding(cvob, coefsUsed = NULL, reps = 500,
    checkThres = seq(0, 1, length.out = 101),
    ds = cvob$glmnet.fit$result[[1]]$ds,
    out = cvob$glmnet.fit$result[[1]]$out,
    wts = rep(1, nrow(ds)),
    imputeDs2FitDsProperties = cvob$glmnet.fit$imputeDs2FitDsProperties,
    ..., niceNames = c(names(coefsUsed)),
    newWindow = FALSE, showWanted = c(0.5, 0.75),
    showThres = 10, verbosity = 0)
}
\arguments{
  \item{cvob}{result of a call to
  \code{\link{crossValidate.EMLassoGLoMo}}}

  \item{coefsUsed}{list holding vectors of names of columns
  (after conversion). Thresholds are selected for each of
  the sets of variables.}

  \item{reps}{number of repeats of imputation /
  crossvalidation}

  \item{checkThres}{set of values between 0 and 1 where the
  TPR / FPR are evaluated}

  \item{ds}{dataset (defaults to the one passed to
  \code{\link{EMLasso}})}

  \item{out}{outcome vector (defaults to the one passed to
  \code{\link{EMLasso}})}

  \item{wts}{weight vector per observation (defaults to the
  one passed to \code{\link{EMLasso}})}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}}

  \item{\dots}{passed on to
  \code{\link{collectImputationModels}}}

  \item{niceNames}{display names for each of the sets of
  variabels, to be used n the titles of the plots}

  \item{newWindow}{if \code{TRUE} (not the default), a new
  window is created for the plots}

  \item{showWanted}{if a vector of two numbers, the first
  is indicated in a green vertical line, and the second as
  a green horizontal line}

  \item{showThres}{see
  \code{\link{plotROCFromRepPredProb}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  a list, holding the following items:
}
\description{
  Simple threshold selection for some sets of variables
  selected by EMLasso
}
\note{
  Thresholds are selected by finding the highest TPR with a
  maximal FPR of 50%
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
iris.cpy<-randomNA(iris, n=0.1)
iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,
	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=0,
	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
sfStop()
iris.cv.emlognet<-crossValidate(iris.emlognet, verbosity=0)
thresHolding(iris.cv.emlognet, coefsUsed=list(onlySepL=c("Sepal.Length"), onlySpecies=c("Speciesversicolor", "Speciesvirginica")), reps=10, verbosity=2)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{fit}
\keyword{logistic}
\keyword{regression}

