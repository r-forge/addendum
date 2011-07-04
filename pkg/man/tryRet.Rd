\name{tryRet}
\alias{tryRet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Try expression with graceful fail
}
\description{
Tries to execute the given expression.

If it fails, return an object of class \code{"try-error"}, holding the error message}
\usage{
tryRet(expr, silent = FALSE, errRet = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{
Expression to execute. If multiple statements: wrap in curly brackets
}
  \item{silent}{
if TRUE, an error occurring is not imediately displayed to the user. If errRet is NULL, the error is
returned from the function
}
  \item{errRet}{
if not NULL and the expression gives an error, this value is returned
}
}
\value{
the result of the last call in the expression, or if there was an error: if \code{errRet} was NULL,
an object representing the error. If \code{errRet} was not NULL, errRet is returned.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
err1<-tryRet(stop("intentional stop"), silent = TRUE, errRet = NULL)
err2<-tryRet(stop("intentional stop"), silent = TRUE, errRet = "Act as if nothing happened")
err3<-tryRet(stop("intentional stop"), silent = FALSE, errRet = "show error but return this")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~error }
\keyword{ ~try }
