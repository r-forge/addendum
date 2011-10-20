\name{originalDataset}
\alias{originalDataset}
\alias{originalDataset.default}
\alias{originalDataset.data.frame.rep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Original dataset for a repeated object
}
\description{
Original dataset for a repeated object
}
\usage{
originalDataset(x)
\method{originalDataset}{data.frame.rep}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
reduced object to get the original, 'repeated' dataset from
}
}
\value{
object of the same class as \code{x}
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	iris.cpy<-iris
	rownames(iris.cpy)<-paste(seq(nrow(iris.cpy)), "_", sep="")
	samplesomerows<-sort(sample(nrow(iris.cpy), 50, replace=TRUE))
	iris.na<-randomNA(iris.cpy, n=0.1)
	
	iris.cpy<-iris.cpy[samplesomerows,]
	
	iris.red<-reduce(iris.cpy, iris.na)
	
	whatWasRepeated<-originalDataset(iris.red)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reduce }
