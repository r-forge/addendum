\name{findCatColNums.numdfr}
\alias{findCatColNums.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Which columns are categorical (factors)
}
\description{
Which columns are categorical (factors), implemented for \code{\link{numdfr}}.
}
\usage{
\method{findCatColNums}{numdfr}(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{\link{numdfr}} object to find the indices of the cetagorical columns for.
}
}
\value{
vector holding the column numbers that are considered categorical
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{addendum}}, \code{\link{findCatColNums}}
}
\examples{
iris.nd<-numdfr(iris)
findCatColNums(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ factor }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
