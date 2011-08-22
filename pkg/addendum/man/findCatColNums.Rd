\name{findCatColNums}
\alias{findCatColNums}
\alias{findCatColNums.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Which columns are categorical (factors)
}
\description{
Which columns are categorical (factors)
}
\usage{
findCatColNums(dfr)
\method{findCatColNums}{data.frame}(dfr)
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
findCatColNums(iris)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~factor }
