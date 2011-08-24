\name{postfixToMakeUnique}
\alias{postfixToMakeUnique}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ensure unique values in character vector by postfixing
}
\description{
Postfix the non-unique values in a character vector. The result is a unique chcarcter vector.
}
\usage{
postfixToMakeUnique(cvect, separator=".", postfixcol=NULL, allowemptypostfix=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvect}{
character vector. Maybe coerced to such (by \code{\link{paste}}).
}
  \item{separator}{
character(s) between original value and the postfix. Can hold multiple values,
reused per postfix (not per item of \code{cvect}!)
}
  \item{postfixcol}{
Collection of postfixes that can be used. If there are not enough, or if
\code{NULL} is specified (default), integers are added until enough unique
values exist.
}
  \item{allowemptypostfix}{
if \code{TRUE} (the default), empty postfixes (\code{""}) are allowed and are 
assumed to imply that no separator is needed in front of that empty postfix.
}
}
\details{
If \code{cvect} only holds unique values, it is immediately returned unaltered!!

If \code{allowemptypostfix} is \code{TRUE}, and \code{postfixcol} is \code{NULL},
the first postfix will be empty and the rest will be integer numbers starting
from 1.

When \code{postfixcol} is provided, but does not hold enough unique values,
integers are added to the list, but the original values are prefered, and as
much as possible in the same order (in as much as \code{unique} retains order).

Finally, when \code{cvect} contained only one copy of a character string, it
could still be postfixed (especially if \code{allowemptypostfix} is \code{FALSE}).
}
\value{
A character vector of the same length as \code{cvect}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
Note that, with some combinations of parameters, the result may not hold all
unique values (e.g. \code{c("a", "a", "a.1")} with default parameters will
result in \code{c("a", "a.1", "a.1")})
}
\examples{
tst<-c("a", "a", "b", "c", "b", "a", "a", "d")
postfixToMakeUnique(tst)
postfixToMakeUnique(tst, allowemptypostfix=FALSE)
postfixToMakeUnique(tst, postfixcol=c("mississippi", "missouri"), allowemptypostfix=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ unique }
\keyword{ postfix }
