\name{convergenceDiagnostics}
\alias{convergenceDiagnostics}
\title{Diagnose convergence of EMLasso.lognet objects}
\usage{
  convergenceDiagnostics(model, minIt = 10,
    checkConvergence = checkConvergence.glmnet, ...,
    verbosity = 0)
}
\arguments{
  \item{model}{model fit (should be the result of
  \code{\link{EMLasso}})}

  \item{minIt}{check convergence from this iteration on}

  \item{checkConvergence}{method, normally
  \code{\link{checkConvergence.glmnet}}, that checks
  convergence.}

  \item{\dots}{passed on to \code{checkConvergence}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  list with one item per lambda. Each item is a list with
  items: \item{numit }{number of iterations until
  convergence for this lambda} \item{simpleconv }{for all
  iterations from minIt to the last one used, whether or
  not convergence occurred} \item{changedNonZeroness }{for
  all iterations from minIt to the last one used, the
  variable names for which zeroness is different from the
  last iteration}
}
\description{
  Diagnose convergence of \code{\link{EMLasso.lognet}}
  objects
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
require(addendum)
require(NumDfr)
require(GLoMo)
require(snowfall)
require(EMLasso)
sfInit(parallel = FALSE, cpus = 1)
sfLibrary(addendum)
sfLibrary(NumDfr)
sfLibrary(GLoMo)
sfLibrary(EMLasso)
iris.cpy<-randomNA(iris, n=0.1)
iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,
	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
sfStop()
convergenceDiagnostics(iris.emlognet, minIt=2, verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{crossvalidate}
\keyword{model}

