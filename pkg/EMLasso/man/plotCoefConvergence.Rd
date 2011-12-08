\name{plotCoefConvergence}
\alias{plotCoefConvergence}
\title{Plot coefficient evolution of EMLasso.lognet objects}
\usage{
  plotCoefConvergence(cofs, skipForNZ = 5, zeroThres =
  1e-04, colors = neatColorSet(), maxleg = 10, leg.cex =
  0.5, verbosity = 0, ...)
}
\arguments{
  \item{cofs}{matrix (or (sparse) \code{\link{Matrix}}) of
  coefficients: rows=iterations, cols=variables}

  \item{skipForNZ}{when checking if a coefficient is ever
  nonzero, the first \code{skipForNZ} (run-in) iterations
  are skipped}

  \item{zeroThres}{how high does a value have to be (in
  absolute value) to be nonzero.}

  \item{colors}{colors used for the \code{\link{matplot}}}

  \item{maxleg}{show legend for (at most) the first
  \code{maxleg} variables}

  \item{leg.cex}{\code{cex} passed to \code{\link{legend}}}

  \item{\dots}{passed on to \code{\link{matplot}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  nothing
}
\description{
  Plot coefficient evolution of
  \code{\link{EMLasso.lognet}} objects over iterations
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
iris.emlognet<-fullDataEMLassoAndGLoMo(ds=numdfr(iris.cpy), out=y,
showPlot=FALSE, type.measure="auc", lambdas=c(0.03,0.002,0.0003),
nrOfSamplesPerMDRow=10, verbosity=2,
minIt=5, maxIt=150, precalcGuidData=FALSE)
sfStop()
plotCoefConvergence(iris.emlognet$result[[1]]$coefs, lty=1, xlab="iteration", ylab="coefficient")
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{crossvalidate}
\keyword{model}

