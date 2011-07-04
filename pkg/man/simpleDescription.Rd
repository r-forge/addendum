\name{simpleDescription}
\alias{simpleDescription}
\alias{showParList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show list in compact printable form
}
\description{
Convert a list (tyically a set of parameters to a function) to proper display format.

\code{showParList} actually displays it.
}
\usage{
simpleDescription(xparlist, depth = -1)
showParList(xparlist, depth=-1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xparlist}{
list to be formatted
}
  \item{depth}{
recurse through how many levels of sub-items
}
}
\value{
string holding the formatted version
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
aList<-list(a=iris, b=c("a", "b", "c"), c=5:36, d=list(t1=1:3, t2="hello"))
sdesc<-simpleDescription(aList, 0)
showParList(aList, 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~list }
\keyword{ ~display }
