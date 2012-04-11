\name{safe2Numeric}
\alias{safe2Numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Safely convert a vector to numeric
}
\description{
Safely convert a vector to numeric
}
\usage{
safe2Numeric(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector to be converted. If this is already a numeric vector, it will be returned unaltered
}
}
\value{
character vector
}
\author{
Nick Sabbe
}
\details{
If \code{x} was a character vector, first an attempt is made to convert it immediately to
numeric. If some values could not be converted, the character vector is first converted to
a factor, after which the level numbers are returned.

If \code{x} was a factor, the level numbers are returned.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
tst1<-c("1", "2.5", "-1")
safe2Numeric(tst1)
tst2<-c("a", "2.5", "-1") #at least one inconvertible
safe2Numeric(tst2)
tst3<-c("1", "2.5", NA, "-1") #NAs are no problem
safe2Numeric(tst3)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ numeric }
\keyword{ convert }
