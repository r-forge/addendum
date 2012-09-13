\name{sampleOrdered}
\alias{sampleOrdered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Like sample, but returns the results in the same order they were in.
}
\description{
Takes a sample of the specified size from the elements of x using either with or without replacement, and 
returns the items in the same order as they were in \code{x}
}
\usage{
sampleOrdered(x, size, replace = FALSE, prob = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, size, replace, prob}{
see \code{\link{sample}}
}
}
\value{
A vector of length \code{size} with elements drawn from either \code{x} or from the integers \code{1:x}.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sample}}
}
\examples{
sample(letters, 10)
sampleOrdered(letters, 10)
sample(5, 20, replace=TRUE)
sampleOrdered(5, 20, replace=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sample }
\keyword{ sort }
