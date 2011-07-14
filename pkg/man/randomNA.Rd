\name{randomNA}
\alias{randomNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
set NA at random places in a data.frame
}
\description{
Return a data.frame, where the given number/fraction of items is randomly set to NA.
}
\usage{
randomNA(dfr, n, atMost=FALSE, tolerance=0.0001, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{data.frame} to start from.
}
  \item{n}{
how many (if it is an integer) or which fraction (if it is between 0 and 1) is
to be set to \code{NA}.
}
  \item{atMost}{
If \code{FALSE} (the default), care is taken so that \code{n} different items
are set to \code{NA} (but note the details section). Otherwise, it is possible
that the same item is set to \code{NA} twice.
}
  \item{tolerance}{
Value used for the check whether \code{n} is an integer. Defaults to
\code{0.0001} and best left unaltered.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
A data.frame with mainly the same form and content as \code{nrow(dfr)}, but with
some items set to \code{NA}
}
\author{
Nick Sabbe
}
\details{
If the original data.frame already contained \code{NA}s, these could be re-set
to \code{NA} (and thus counted as 'now' set to \code{NA}).

This method can be interpreted to generate MCAR Missing Data.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{randomFillDS}}
}
\examples{
randomNA(iris, 50, verbosity=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~random }
\keyword{ ~NA }
\keyword{ ~data.frame }
\keyword{ ~missing }
