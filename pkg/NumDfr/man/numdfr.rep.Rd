\name{numdfr.rep}
\alias{numdfr.rep}
\alias{[.numdfr.rep}
\alias{[<-.numdfr.rep}
\alias{length.numdfr.rep}
\alias{dimnames.numdfr.rep}
\alias{dimnames<-.numdfr.rep}
\alias{dim.numdfr.rep}
\alias{names.numdfr.rep}
\alias{names<-.numdfr.rep}
\alias{is.numdfr.rep}
\alias{as.double.numdfr.rep}
\alias{is.na.numdfr.rep}
\alias{str.numdfr.rep}
\alias{as.list.numdfr.rep}
\alias{as.data.frame.numdfr.rep}
\alias{findCatColNums.numdfr.rep}
\alias{rbind.numdfr.rep}
\alias{print.numdfr.rep}
\alias{display.numdfr.rep}
\alias{as.matrix.numdfr.rep}
\alias{allLevels.numdfr.rep}
\alias{[[.numdfr.rep}
\alias{$.numdfr.rep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for \code{numdfr.rep} objects
}
\description{
All functions (S3) that are provided for \code{\link{numdfr}} are also provided
for \code{numdfr.rep}
}
\usage{
\method{[}{numdfr.rep}(x, i, j, returnAsMatrix = drop, drop = FALSE)
\method{[}{numdfr.rep}(x, i, j) <- value
\method{length}{numdfr.rep}(x)
\method{dimnames}{numdfr.rep}(x)
\method{dimnames}{numdfr.rep}(x) <- value
\method{dim}{numdfr.rep}(x)
\method{names}{numdfr.rep}(x)
\method{names}{numdfr.rep}(x) <- value
is.numdfr.rep(x)
\method{as.double}{numdfr.rep}(x,...)
\method{is.na}{numdfr.rep}(x)
\method{str}{numdfr.rep}(object,...)
\method{as.list}{numdfr.rep}(x, returnFactors=TRUE,...)
\method{as.data.frame}{numdfr.rep}(x, row.names = NULL, optional = FALSE, ...)
\method{findCatColNums}{numdfr.rep}(dfr)
\method{rbind}{numdfr.rep}(..., original.data, ensure.unique.rownames=FALSE, separator=".", postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
\method{print}{numdfr.rep}(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE)
\method{display}{numdfr.rep}(dfr)
\method{as.matrix}{numdfr.rep}(x, ...)
\method{allLevels}{numdfr.rep}(x, onlyNonEmpty=FALSE)
\method{[[}{numdfr.rep}(x, ..., exact=TRUE)
\method{$}{numdfr.rep}(x, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,i,j,returnAsMatrix, drop, value, object, returnFactors, row.names,
		optional, dfr, ensure.unique.rownames, separator, postfixcol,
		allowemptypostfix, deparse.level, digits, quote, right, onlyNonEmpty,
		exact, name}{
see equally named method for \code{\link{numdfr}}
}
	\item{original.data}{
		for \code{rbind}: if the original data is shared anyway, pass it along
		again if possible to avoid some calculations + to ensure memory reuse (!)
}
  \item{\dots}{
(typically) ignored
}
}
\note{
	Current implementation needs work (optimization, special case handling,...)
	
	As of today, the assignments have as their only implementation to fail!
}
\value{
see equally named method for \code{\link{numdfr}}
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{[}}, \code{\link{numdfr}},
\code{\link{[.numdfr}}, \code{\link{[<-.numdfr}},
\code{\link{length.numdfr}}, \code{\link{dimnames.numdfr}},
\code{\link{dimnames<-.numdfr}}, \code{\link{dim.numdfr}},
\code{\link{names.numdfr}}, \code{\link{names<-.numdfr}},
\code{\link{is.numdfr}}, \code{\link{as.double.numdfr}},
\code{\link{is.na.numdfr}}, \code{\link{str.numdfr}},
\code{\link{as.list.numdfr}}, \code{\link{as.data.frame.numdfr}},
\code{\link{findCatColNums.numdfr}}, \code{\link{rbind.numdfr}},
\code{\link{print.numdfr}}, \code{\link{display.numdfr}},
\code{\link{as.matrix.numdfr}}, \code{\link{allLevels.numdfr}},
\code{\link{[[.numdfr}}, \code{\link{$.numdfr}},
\code{\link{reduce.numdfr}}, \code{\link{as.numdfr}}, \code{\link{as.numdfr.rep}}
}
\examples{
	iris.cpy<-iris
	rownames(iris.cpy)<-paste(seq(nrow(iris.cpy)), "_", sep="")
	iris.nd<-numdfr(iris.cpy)
	samplesomerows1<-sort(sample(nrow(iris.nd), 50, replace=TRUE))
	samplesomerows2<-sort(sample(nrow(iris.nd), 50, replace=TRUE))
	iris.na<-numdfr(randomNA(iris.cpy, n=0.1))
	
	iris.nd1<-iris.nd[samplesomerows1,]
	iris.nd2<-iris.nd[samplesomerows1,]
	
	iris.red1<-reduce(iris.nd1, iris.na)
	iris.red2<-reduce(iris.nd2, iris.na)
	
	str(iris.red1)
	dimnames(iris.red1)
	
	iris.red<-rbind(iris.red1, iris.red2)
	dimnames(iris.red)
	str(iris.red)
	
	\dontrun{display(iris.red)}
	#this should give the same results
	iris.subred<-iris.red[51:53,]
	print(iris.red[51:53,])
	print(iris.red2[1:3,])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numdfr }
