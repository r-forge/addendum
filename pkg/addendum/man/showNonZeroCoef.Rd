\name{showNonZeroCoef}
\alias{showNonZeroCoef}
\alias{showNonZeroCoef.default}
\alias{showNonZeroCoef.cv.glmnet}
\alias{showNonZeroCoef.glmnet}
\alias{showNonZeroCoef.Matrix}
\alias{coef.Matrix}
\alias{displayLinearFromNZC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show nonzero glmnet coefficients in a standardized way
}
\description{
Show nonzero glmnet coefficients in a standardized way. \code{displayLinearFromNZC} converts
the result of a \code{showNonZeroCoef} call to a string like the right hand side of a formula.
}
\usage{
showNonZeroCoef(x, ...)
\method{showNonZeroCoef}{default}(x, atLeast=1, thres=0.00001,s=NULL,...)
\method{showNonZeroCoef}{cv.glmnet}(x, atLeast=1, thres=0.00001,s="lambda.1se",...)
\method{showNonZeroCoef}{glmnet}(x, atLeast=1, thres=0.00001,...)
\method{showNonZeroCoef}{Matrix}(x, atLeast=1, thres=0.00001,...)
displayLinearFromNZC(nZeroCoef, SEs, roundDigits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
coefficient \code{Matrix}, object of class \code{glmnet} or \code{cv.glmnet} or a list thereof
}
  \item{atLeast}{
show only those where the coefficient is nonzero at least \code{atLeast} times within \code{x}. Defaults to 1.
}
  \item{thres}{
threshold below which the absolute value is considered to be 'zero'. Defaults to 0.00001.
}
  \item{s}{
if \code{x} is a list, this parameter is passed to \code{coef} applied to each item of the list. Defaults to \code{"lambda.1se"}.
}
  \item{\dots}{
extra parameters passed on to \code{coef} if necessary
}
  \item{nZeroCoef}{
result of a \code{showNonZeroCoef} call
}
  \item{SEs}{
Standard errors matching the \code{nZeroCoef}. If \code{NULL}, the standard error part is left out of the resulting string.
}
  \item{roundDigits}{
round numbers in the string up to this many digits.
}
}
\value{ 
Typically a \code{dgCMatrix} (package \code{Matrix}) with rows representing the
(intercept) + covariates, and columns for every case present in \code{x}
}
\author{
Nick Sabbe
}
\note{
\code{coef.Matrix} is only provided to simplify implementation.

The typical output of \code{displayLinearFromNZC} is something like \code{-1.23 + 8.79 Petal.Length}
or \code{-1.23 (pm 0.06) + 8.79 (pm 0.65) Petal.Length}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{showInterestingCoef}}
}
\examples{
dat<-matrix(rnorm(1000), ncol=20)
y1<-rnorm(50)
y2<-rnorm(50)
cv1<-cv.glmnet(dat, y1)
cv2<-cv.glmnet(dat, y2, lambda=cv1$lambda)
showNonZeroCoef(cv1)
showNonZeroCoef(coef(cv1))
showNonZeroCoef(cv1$glmnet.fit)
showNonZeroCoef(list(cv1$glmnet.fit, cv2$glmnet.fit))
displayLinearFromNZC(showNonZeroCoef(cv1), SEs=NULL, roundDigits=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~glmnet }
\keyword{ ~coef }
