\name{removeItems}
\alias{removeItems}
\alias{removeCols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
remove items from vector
}
\description{
\code{removeItems}: Similar to \code{setdiff} but allows to show feedback on what is removed.

\code{removeCols}: Removes named columns from the data.frame
}
\usage{
removeItems(removeWhat, fromWhat, verbose = FALSE)
removeCols(dfr, colsToRemove, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{removeWhat}{
Vector of items to remove
}
  \item{fromWhat}{
Vector ot items from which some may be removed
}
  \item{verbose}{
if TRUE, display feedback on what was removed
}
  \item{dfr}{
Data.frame from which columns are removed
}
  \item{colsToRemove}{
Vector of column names to remove
}
}
\value{
\code{removeItems}: Vector with elements of \code{fromWhat} not in \code{removeWhat}

\code{removeCols}: data.frame with the columns of \code{dfr} whose names are not in \code{colsToRemove}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{setdiff}}
}
\examples{
removeItems(c("apple", "pear", "orange", "grape"), c("grape", "banana", "pear"), verbose=TRUE)
removeCols(iris, c("Sepal.Length", "Petal.Length"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~vector }
\keyword{ ~remove }% __ONLY ONE__ keyword per line
