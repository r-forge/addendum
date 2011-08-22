\name{is.na.numdfr}
\alias{is.na.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
‘Not Available’ / Missing Values
}
\description{
finds out which of the items are \code{NA}
}
\usage{
\method{is.na}{numdfr}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to investigate.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Array of the same dimensions as \code{x}, holding \code{boolean} values.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{is.na}}
}
\examples{
library(addendum)
iris.nd<-numdfr(randomNA(iris, n=20))
is.na(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NA }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
