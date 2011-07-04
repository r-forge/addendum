\name{naPos}
\alias{naPos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find row/column combinations where NAs are in a dataframe
}
\description{
Finds the coordinates where NAs are in the given dataframe
}
\usage{
naPos(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
data.frame to be investigated
}
}
\value{
a matrix with  one row for every NA in the data.frame and two columns :
  \item{cl }{Column holding NA}
  \item{rw }{Row holding NA}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iriscopy<-iris
coldel<-sample(dim(iriscopy)[2], 10, replace=TRUE)
rowdel<-sample(dim(iriscopy)[1], 10, replace=TRUE)
for(i in 1:10){iriscopy[rowdel[i],coldel[i]]<-NA}
naPos(iriscopy)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~NA }
