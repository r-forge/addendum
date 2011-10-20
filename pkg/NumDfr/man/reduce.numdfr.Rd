\name{reduce}
\alias{reduce.numdfr}
\alias{unreduce.numdfr.rep}
\alias{as.numdfr}
\alias{as.numdfr.default}
\alias{as.numdfr.data.frame}
\alias{as.numdfr.data.frame.rep}
\alias{as.numdfr.numdfr}
\alias{as.numdfr.numdfr.rep}
\alias{as.numdfr.rep}
\alias{as.numdfr.rep.default}
\alias{as.numdfr.rep.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create less memory-heavy version of an \code{\link{numdfr}} object, retaining all the information (!)
}
\description{
When a \code{\link{numdfr}} is really a set of rows from another one, this
function can be used to make a new object (of class "numdfr.rep") that holds the same
information in more condensed format. Disadvantage is that the data is less
easily manipulated.
}
\usage{
\method{reduce}{numdfr}(object, orgdfr, repsperrow=NULL, keeponlyusedrows=FALSE, ...)
\method{unreduce}{numdfr.rep}(object, ...)
\method{as.numdfr}{data.frame}(object, ...)
\method{as.numdfr}{data.frame.rep}(object, ...)
\method{as.numdfr}{numdfr}(object, ...)
\method{as.numdfr}{numdfr.rep}(object, ...)
\method{as.numdfr.rep}{numdfr}(object, orgdfr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object that you want to memory-reduce.
}
  \item{orgdfr}{
\code{\link{numdfr}} for which \code{object} is an extension.
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
ignored
}
}
\note{
	Contrary to what the docs of \code{\link{reduce}} say, this method keeps all
	the information in the original object!
}
\value{
For \code{reduce} and \code{as.numdfr.rep}, an object of class "numdfr.rep". For
the others, a \code{\link{numdfr}} object.
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	iris.cpy<-as.numdfr(randomNA(iris, n=0.1))
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
