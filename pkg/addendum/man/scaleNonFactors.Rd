\name{scaleNonFactors}
\alias{scaleNonFactors}
\title{
scale the non-factor columns of a data.frame
}
\description{
Apply a \code{\link{scale}} to the non-factor columns. If needed, groups of
columns can be scaled together.
}
\usage{
scaleNonFactors(dfr, colgroups=NULL, checkunique=FALSE, returnAttributes=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
data.frame or other to investigate the columns of.
}
  \item{colgroups}{
list of character vectors. Each item is considered a set of columns that is scaled 
according to their common center and variance.
}
  \item{checkunique}{
if \code{TRUE}, a check is performed to see that no columns are in two groups in
\code{colgroups}
}
  \item{returnAttributes}{
if \code{TRUE}, a list is returned holding not only the altered \code{\link{data.frame}},
but also a matrix holding the centers and scale factors that were used.
}
}
\value{
if \code{returnAttributes=TRUE}, a ist with an item \code{dfr}, the altered 
\code{\link{data.frame}}, and an item \code{scaleattributes} that is a matrix
with a column per column in \code{dfr}, and two rows, (\code{cnter} and 
\code{scale}) that hold the means and standard deviations that were used
to rescale each column.

If \code{returnAttributes=FALSE}, simply the altered \code{\link{data.frame}}.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
scaleNonFactors(iris, checkunique=TRUE, returnAttributes=TRUE)
scaleNonFactors(iris, colgroups=list(c("Sepal.Width", "Petal.Length")), returnAttributes=TRUE)
}
\keyword{ scale }
