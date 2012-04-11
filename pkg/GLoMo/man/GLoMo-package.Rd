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
Version: \tab 1.2.3\cr
Date: \tab 2012-03-30\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}
Main class: \code{\link{GLoMo-class}}
\enumerate{
	\item Helpers \enumerate{
		\item \code{\link{getGuidData}}
		\item \code{\link{updateGuidData}}
		\item \code{\link{randomFillAndRepeatDataRow}}
		\item \code{\link{rCatsAndCntInDfr}}
		\item \code{\link{rCatsInDfr}}
		\item \code{\link{reusableDataForGLoMoSampling}}
	}
	\item Fitting \enumerate{
		\item \code{\link{GLoMo}}
	}
	\item Prediction \enumerate{
		\item \code{\link{predict.GLoMo}}
		\item \code{\link{predict.conditional.allrows.GLoMo}}
		\item \code{\link{predict.conditional.GLoMo}}
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
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
iris.nsamplesperrow<-sample.int(10, size=nrow(iris.md.nd), replace=TRUE)
iris.pred<-predict(iris.glomo,nobs=iris.nsamplesperrow, newdata=iris.md.nd,
	returnRepeats=TRUE, returnSelectedGlomoRows=TRUE, verbosity=10)
}
