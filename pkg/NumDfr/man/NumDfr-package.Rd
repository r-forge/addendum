\name{NumDfr-package}
\alias{NumDfr-package}
\alias{NumDfr}
\docType{package}
\title{
Provide a performant alternative to data.frame wrt factors
}
\description{
This package is created with GLoMo as its main goal, but contains interesting
options. For some applications, especially involving mostly numeric columns and
some categorical ones, this will be a speedy alternative.
}
\details{
\tabular{ll}{
Package: \tab NumDfr\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-08-22\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}
Add list of functions here later on (similar to addendum)
#\enumerate{
#	\item \code{\link{glmnet}} extensions \enumerate{
#		\item \code{\link{addCVPlot}}
#		\item \code{\link{addLamIndexAxis}}
#		\item \code{\link{factorsToDummyVariables}}
#		\item \code{\link{originalColumnNamesFromDummyVars}}
#		\item \code{\link{makeFactorsAndSetFrontal}}
#		\item \code{\link{plot2.cv.glmnet}}
#		\item \code{\link{showNonZeroCoef}}
#	}
#	\item plotting \enumerate{
#		\item \code{\link{getColorRange}}
#		\item \code{\link{histAndDensity}}
#		\item \code{\link{plot4d}}
#		\item \code{\link{plotAllScatters}}
#		\item \code{\link{plotAndSmooth}}
#		\item \code{\link{qqWithPred}}
#		\item \code{\link{smartpairs}}
#		\item \code{\link{showNumericalHeatMap}}
#	}
#}
}
\author{
Nick Sabbe

Maintainer: <nick.sabbe@ugent.be>
}
\keyword{ package }
\examples{
iris.nd<-numdfr(iris)
}
