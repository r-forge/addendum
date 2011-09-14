\name{reduce}
\alias{reduce}
\alias{reduce.default}
\alias{unreduce}
\alias{unreduce.default}
\alias{reduce.data.frame}
\alias{unreduce.data.frame.rep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create less memory-heavy version of an object (typically also less informative)
}
\description{
Meant as a placeholder function to be extended in other packages. The default
implemntation simply returns the original object. But for some purposes, 'simpler'
versions of an object can be created, reducing memory footprint. Obviously,
\code{unreduce} is supposed to revert the operation (may not be possible).
}
\usage{
\method{reduce}{data.frame}(object, orgdfr, repsperrow=NULL, keeponlyusedrows=FALSE, ...)
\method{unreduce}{data.frame.rep}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object that you want to memory-reduce.
}
  \item{orgdfr}{
\code{\link{data.frame}} for which \code{object} is an extension.
}
  \item{repsperrow}{
object like the return value of \code{\link{findRepsPerRow}}. If not provided, it
is calculated.
}
  \item{keeponlyusedrows}{
if \code{TRUE} (default \code{FALSE}), the rows from \code{orgdfr} that aren't
extended by \code{object} are dropped from the object.
}
  \item{\dots}{
implementation-specific (ignored in default)
}
}
\note{
	During the creation of the lower footprint object, typically, the bigger one
	will still be in memory! So make sure you have enough memory up front!
}
\value{
For the default: the original object. In derived implementations, may take any form!
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	tst<-reduce(c("a", "z", "e"))
	print(tst)
	
	iris.cpy<-randomNA(iris, n=0.1)
	rownames(iris.cpy)<-paste(seq(nrow(iris.cpy)), "_", sep="")
	takesomerows<-sort(sample(nrow(iris.cpy), 50, replace=TRUE))
	iris.ex<-randomFillDS(iris.cpy[takesomerows,])
	str(iris.ex)
	iris.red<-reduce(iris.ex, iris.cpy)
	str(iris.red)
	iris.back<-unreduce(iris.red)
	str(iris.back)
	
	iris.red2<-reduce(iris.ex, iris.cpy, keeponlyusedrows=TRUE)
	object.size(iris.ex)
	object.size(iris.red)
	object.size(iris.red2)
	str(iris.red2)

	iris.back2<-unreduce(iris.red2)
	str(iris.back2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reduce }
\keyword{ memory }
