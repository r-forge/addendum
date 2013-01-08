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
Version: \tab 1.7.6\cr
Date: \tab 2013-01-08\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}
Add list of functions here later on (similar to addendum)
\enumerate{
	\item Debugging \enumerate{
		\item \code{\link{setDebugmodeNumDfr}}
		\item \code{\link{str.numdfr}}
	}
	\item Coercion \enumerate{
		\item \code{\link{numdfr}}
		\item \code{\link{as.data.frame.numdfr}}
		\item \code{\link{as.double.numdfr}}
		\item \code{\link{as.list.numdfr}}
		\item \code{\link{is.na.numdfr}}
		\item \code{\link{as.matrix.numdfr}}
		\item \code{\link{as.nummatrix}}
		\item \code{\link{matBack2OrgClass}}
	}
	\item Properties/utility \enumerate{
		\item \code{\link{dim.numdfr}}
		\item \code{\link{str.numdfr}}
		\item \code{\link{dimnames.numdfr}}
		\item \code{\link{factorsToDummyVariables.numdfr}}
		\item \code{\link{findCatColNums.numdfr}}
		\item \code{\link{is.numdfr}}
		\item \code{\link{length.numdfr}}
		\item \code{\link{names.numdfr}}
		\item \code{\link{[.numdfr}}
		\item \code{\link{[<-.numdfr}}
		\item \code{\link{[[.numdfr}}
		\item \code{\link{$.numdfr}}
		\item \code{\link{[[<-.numdfr}}
		\item \code{\link{$<-.numdfr}}
		\item \code{\link{display.numdfr}}
		\item \code{\link{allLevels}}
	}
	\item Properties/utility \enumerate{
		\item \code{\link{numdfr.rep}}
	}
}
}
\author{
Nick Sabbe

Maintainer: <nick.sabbe@ugent.be>
}
\keyword{ package }
\examples{
iris.nd<-numdfr(iris)
}
