\name{showMemoryUsage}
\alias{showMemoryUsage}
\alias{showMemoryUsageList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Memory sizes of all non-function objects present in global environment
}
\description{
Show the names and sizes of all objects taking more than \code{threshold} Kb.

If any of these objects have sub-items (e.g. lists), they are also shown up to the passed on depth.
}
\usage{
showMemoryUsage(threshold, depth = 1)
showMemoryUsageList(lst, threshold, depth=1, indent="  ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{threshold}{
Only objects whose memory footprint is greater than this \code{threshold} are returned
}
  \item{depth}{
How deep should nesting the objects go (default: 1).
}
  \item{lst}{
Display sizes of items of this list.
}
  \item{indent}{
prepend every displayed line with this string.
}
}
\details{
\code{showMemoryUsageList} is a helper function, but can also be used immediately
to find the size of (sub)items in a list.
}
\value{
Though there is an (invisible) return value, it should be ignored.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
showMemoryUsage(10,1)
showMemoryUsageList(iris, 10,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~memory }
\keyword{ ~object }
