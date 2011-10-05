\name{invwhich}
\alias{invwhich}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse function of \code{which}
}
\description{
Converts a set of numeric indices to a vector of logicals
}
\usage{
invwhich(indices, outlength, useNames = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indices}{
numerical indices of '\code{TRUE}' in the resulting vector
}
  \item{outlength}{
length of the resulting vector (note: this cannot be derived from the indices argument!)
}
  \item{useNames}{
if \code{TRUE}, the names of the indices are used in the result as best possible
}
}
\value{
logical vector of length \code{outlength}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
l<-100
someindices<-sample(l, l/2, replace=FALSE)
invwhich(someindices, l)
}
\keyword{ which }
