\name{str.numdfr}
\alias{str.numdfr}
\alias{print.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display contents/structure of a \code{\link{numdfr}} object
}
\description{
Display contents of a \code{\link{numdfr}} object
}
\usage{
\method{str}{numdfr}(object, short=TRUE, ...)
\method{print}{numdfr}(x, ..., digits = NULL, quote = FALSE, right = TRUE,
    row.names = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{\link{numdfr}} object
}
  \item{short}{
if \code{TRUE} (the default), only max 10 rownames, colnames and factors are displayed
}
  \item{\dots}{
ignored for now for \code{str}. Passed on to printing the matrix in \code{print}
}
  \item{x}{
\code{\link{numdfr}} object
}
  \item{digits, quote, right, row.names}{
see \code{\link{print.data.frame}}: they are simply passed on
}
}
\value{
\code{str} returns nothing (and invisibly so), \code{print} returns \code{x} invisibly.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{str}}, \code{\link{print}}, \code{\link{print.data.frame}}
}
\examples{
iris.nd<-numdfr(iris)
str(iris.nd)
str(iris.nd, short=FALSE)
print(iris.nd[1:5,])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ structure }
\keyword{ print }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
