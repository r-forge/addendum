\name{subsetFirstDim}
\alias{subsetFirstDim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Do subset selection on the first dimension
}
\description{
Do subset selection on the first dimension. If the object does not have a dimension, then simple subset selection is applied.
}
\usage{
subsetFirstDim(object, ssExpr,drop=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
the object that subsetting will be applied to. Typically a vector or matrix
}
  \item{ssExpr}{
Subsetting expression. What is possible depends upon object, but typically logical vectors and numericals are allowed.
}
  \item{drop}{
return object of the lowest dimension possible? Typically, when using this method, you will not want this, so the default is \code{FALSE}
}
}
\value{
When \code{drop=FALSE}, (normally) an object of the same dimension of \code{object}.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
tstMat<-matrix(1:6, ncol=2)
subsetFirstDim(tstMat, c(1,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ subset }
\keyword{ dimension }
