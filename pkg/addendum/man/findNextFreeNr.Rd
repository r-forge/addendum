\name{findNextFreeNr}
\alias{findNextFreeNr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
In a set of names possibly following a pattern, find the next free nummer
}
\description{
In a set of names possibly following a pattern, find the next free nummer
}
\usage{
findNextFreeNr(nms, pattern="^\\\\.([[:digit:]]+)$", reppattern="\\\\1", default=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nms}{
names to be investigated (should be a character vector)
}
  \item{pattern}{
pattern to look for (regular expression). Should contain at least one set of round braces for the replacement to work. It is advised to include "^" and "$" to only mathc complete names.
}
  \item{reppattern}{
replacement pattern. Will typically include (or even be equal to) "\\1". When using this as replacement in \code{\link{sub}}, it should result in only the integer part of the names that match the \code{pattern}
}
  \item{default}{
value that is returned if none of the \code{names} matched the \code{pattern}.
}
}
\value{
integer
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
findNextFreeNr(c("123abc", ".5", "4.60"))
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ numeric }
\keyword{ convert }
