\name{invertSymmetric}
\alias{invertSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Invert a symmetric matrix quickly
}
\description{
\code{solve} works fine in general, but this method is a lot faster for symmetric positive definite matrices to calculate their inverse.
}
\usage{
invertSymmetric(symMat, careful = FALSE, silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symMat}{
Matrix to invert (should be square)
}
  \item{careful}{
if you're not entirely sure the matrix is positive definite (or other
		problems with the matrix set this to TRUE. If cholesky decomposition fails,
		it will use solve next (which might also fail though). Probably slower.
}
  \item{silent}{
if TRUE, errors occurring in cholesky decomposition are hidden from the user, and
		\code{solve} is silently called
}
}
\details{
Simply uses \code{chol2inv} and \code{chol} behind the scenes with some error checking.
}
\value{
Square matrix of the same dimension of \code{symMat}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x<-round(upper.tri(matrix(1, 3, 3), diag = TRUE))
sm<-x \%*\% t(x)
invertSymmetric(sm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~symmetric }
\keyword{ ~matrix }
\keyword{ ~inverse }
