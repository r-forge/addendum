\name{GLoMo-package}
\alias{GLoMo-package}
\docType{package}
\title{
Naive General Location Model
}
\description{
General Location Model as e.g. found in "Statistical Analysis with Missing Data".
This version can be fit to a data.frame or numdfr without missing data, and
supports prediction of new data and 'completion prediction' of data with missing
values. It also supports conditional completion.
}
\details{
\tabular{ll}{
Package: \tab GLoMo\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-08-30\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}
\enumerate{
	\item Helpers \enumerate{
		\item \code{\link{getGuidData}}
		\item \code{\link{str.numdfr}}
	}
	\item Fitting \enumerate{
		\item \code{\link{numdfr}}
		\item \code{\link{as.data.frame.numdfr}}
		\item \code{\link{as.double.numdfr}}
		\item \code{\link{as.list.numdfr}}
		\item \code{\link{is.na.numdfr}}
	}
	\item Prediction \enumerate{
		\item \code{\link{dim.numdfr}}
		\item \code{\link{dimnames.numdfr}}
		\item \code{\link{factorsToDummyVariables.numdfr}}
		\item \code{\link{findCatColNums.numdfr}}
		\item \code{\link{is.numdfr}}
		\item \code{\link{length.numdfr}}
		\item \code{\link{names.numdfr}}
		\item \code{\link{[.numdfr}}
		\item \code{\link{display.numdfr}}
		\item \code{\link{allLevels}}
	}
}
}
\author{
Nick Sabbe

Maintainer: <nick.sabbe@ugent.be>
}
\keyword{ package }
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd<-numdfr(iris)
iris.nd.rnd<-rCatsAndCntInDfr(iris.nd, orgriName=NULL, verbosity=1)
iris.weights<-unlist(iris.nd.rnd[,"weights", drop=TRUE])
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
iris.nsamplesperrow<-sample.int(10, size=nrow(iris.md.nd), replace=TRUE)
iris.pred<-predict(iris.glomo,nobs=iris.nsamplesperrow, newdata=iris.md.nd,
	returnRepeats=TRUE, returnSelectedGlomoRows=TRUE, verbosity=10)
}
