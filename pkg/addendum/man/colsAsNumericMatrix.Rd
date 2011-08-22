\name{colsAsNumericMatrix}
\alias{colsAsNumericMatrix}
\alias{colsAsNumericMatrix.data.frame}
\alias{colsAsNumericMatrix.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert to a matrix of numerical columns
}
\description{
converts a data.frame or other object into a matrix of numerical columns
}
\usage{
colsAsNumericMatrix(dfr)
\method{colsAsNumericMatrix}{data.frame}(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
data.frame or other to be converted.
}
}
\value{
matrix
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
colsAsNumericMatrix(iris)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~numeric }
\keyword{ ~matrix }
