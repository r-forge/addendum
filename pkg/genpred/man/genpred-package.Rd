\name{genpred-package}
\alias{genpred-package}
\alias{genpred}
\docType{package}
\title{
Generalized prediction with classification or regression
}
\description{
Enables to run a lot of different classification/regression mechanisms and compare the results.
}
\details{
\tabular{ll}{
Package: \tab genpred\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-08-24\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}
	Generalized binary classification/regression \enumerate{
		\item \code{\link{displayPreds}}
		\item \code{\link{doFormulaContCV}}
		\item \code{\link{crossPredictContinuous}}
		\item \code{\link{fitAndPredictContinuous.General}}
		\item \code{\link{createCVResult}}
		\item \code{\link{evaluatePredictions.lms}}
		\item \code{\link{getFormulaCVDescription}}
		\item \code{\link{print.CVRes}}
		\item \code{\link{doFormulaCV}}
		\item \code{\link{crossPredictBinary}}
		\item \code{\link{fitAndPredictBinary.General}}
	}
}
\author{
Nick Sabbe

Maintainer: <nick.sabbe@ugent.be>
}
\keyword{ package }
\examples{
doBagging<-function(dfr, outcol, verbosity=0,...)#,nbagg=100
{
	require(ipred)
	doFormulaCV(dfr, outcol, fitFunc=bagging, fitAndPredictBinary="class",
		verbosity=verbosity, ...)
}
iriscopy<-iris
iriscopy$sthing<-factor(rbinom(dim(iris)[1], 1, 0.5))
doBagging(iriscopy, outcol="sthing", verbosity=5,nbagg=100)
}
