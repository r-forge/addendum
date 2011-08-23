\name{numdfr}
\alias{numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructor for \code{numdfr} class
}
\description{
Turna \code{\link{data.frame}} into a \code{numdfr}
}
\usage{
numdfr(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{\link{data.frame}} to coerce
}
}
\details{
Preserves as much of the information as possible from the \code{\link{data.frame}}
into the \code{numdfr}. Order of factors and some other details may be lost in the process.
}
\value{
returns an object of class \code{numdfr}. Althoug you are not supposed to use this
directly, this is a list containing two items:
  \item{mat }{Numerical matrix representation of the data}
  \item{lvls }{list	with for each column a character vector of the levels (length zero if the column wasn't a factor)}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iris.nd<-numdfr(iris)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data.frame }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
