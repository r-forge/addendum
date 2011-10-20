\name{data.frame.rep}
\alias{data.frame.rep}
\alias{[.data.frame.rep}
\alias{[<-.data.frame.rep}
\alias{length.data.frame.rep}
\alias{dimnames.data.frame.rep}
\alias{dimnames<-.data.frame.rep}
\alias{dim.data.frame.rep}
\alias{names.data.frame.rep}
\alias{names<-.data.frame.rep}
\alias{is.data.frame.rep}
\alias{as.double.data.frame.rep}
\alias{is.na.data.frame.rep}
\alias{str.data.frame.rep}
\alias{as.list.data.frame.rep}
\alias{as.data.frame.data.frame.rep}
\alias{findCatColNums.data.frame.rep}
\alias{rbind.data.frame.rep}
\alias{print.data.frame.rep}
\alias{display.data.frame.rep}
\alias{as.matrix.data.frame.rep}
\alias{allLevels.data.frame.rep}
\alias{[[.data.frame.rep}
\alias{$.data.frame.rep}
\alias{factorsToDummyVariables.data.frame.rep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for \code{data.frame.rep} objects
}
\description{
All functions (S3) that are provided for \code{\link{data.frame}} are also provided
for \code{data.frame.rep}
}
\usage{
\method{[}{data.frame.rep}(x, i, j, drop = FALSE)
\method{[}{data.frame.rep}(x, i, j) <- value
\method{length}{data.frame.rep}(x)
\method{dimnames}{data.frame.rep}(x)
\method{dimnames}{data.frame.rep}(x) <- value
\method{dim}{data.frame.rep}(x)
\method{names}{data.frame.rep}(x)
\method{names}{data.frame.rep}(x) <- value
is.data.frame.rep(x)
\method{as.double}{data.frame.rep}(x,...)
\method{is.na}{data.frame.rep}(x)
\method{str}{data.frame.rep}(object,...)
\method{as.list}{data.frame.rep}(x, returnFactors=TRUE,...)
\method{as.data.frame}{data.frame.rep}(x, row.names = NULL, optional = FALSE, ...)
\method{findCatColNums}{data.frame.rep}(dfr)
\method{rbind}{data.frame.rep}(..., original.data, ensure.unique.rownames=FALSE, separator=".", postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
\method{print}{data.frame.rep}(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE)
\method{display}{data.frame.rep}(dfr)
\method{as.matrix}{data.frame.rep}(x, ...)
\method{allLevels}{data.frame.rep}(x, onlyNonEmpty=FALSE)
\method{[[}{data.frame.rep}(x, ..., exact=TRUE)
\method{$}{data.frame.rep}(x, name)
\method{factorsToDummyVariables}{data.frame.rep}(dfr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,i,j, drop, value, object, returnFactors, row.names,
		optional, dfr, ensure.unique.rownames, separator, postfixcol,
		allowemptypostfix, deparse.level, digits, quote, right, onlyNonEmpty,
		exact, name}{
see equally named method for \code{\link{data.frame}}
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
see equally named method for \code{\link{data.frame}}
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{data.frame}}
}
\examples{
	iris.cpy<-iris
	rownames(iris.cpy)<-paste(seq(nrow(iris.cpy)), "_", sep="")
	samplesomerows1<-sort(sample(nrow(iris.cpy), 50, replace=TRUE))
	samplesomerows2<-sort(sample(nrow(iris.cpy), 50, replace=TRUE))
	iris.na<-randomNA(iris.cpy, n=0.1)
	
	iris.cpy1<-iris.cpy[samplesomerows1,]
	iris.cpy2<-iris.cpy[samplesomerows1,]
	
	iris.red1<-reduce(iris.cpy1, iris.na)
	iris.red2<-reduce(iris.cpy2, iris.na)
	
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
\keyword{ data.frame }
