\name{findCatColNums}
\alias{findCatColNums}
\alias{findCatColNums.data.frame}
\alias{findOrderedColNums}
\alias{findOrderedColNums.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Which columns are categorical (factors) / ordered factors
}
\description{
Which columns are categorical (factors) / ordered factors
}
\usage{
findCatColNums(dfr)
\method{findCatColNums}{data.frame}(dfr)
findOrderedColNums(dfr)
\method{findOrderedColNums}{data.frame}(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
data.frame or other to investigate the columns of.
}
}
\value{
vector holding the column numbers that are considered categorical
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iristmp<-iris
iristmp$tst<-factor(sample.int(5, nrow(iris), replace=TRUE), ordered=TRUE)
findCatColNums(iristmp)
findOrderedColNums(iristmp)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ factor }
\keyword{ ordered }
