\name{restrictForLambda}
\alias{restrictForLambda}
\alias{restrictForLambda.glmnet}
\alias{restrictForLambda.cv.glmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a copy of the (\code{cv.})\code{glmnet} object for a subset of the lambdas
}
\description{
Create a copy of the (\code{cv.})\code{\link{glmnet}} object for a subset of the lambdas
}
\usage{
\method{restrictForLambda}{glmnet}(fit, lambdaindices)
\method{restrictForLambda}{cv.glmnet}(fit, lambdaindices)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
\code{\link{glmnet}} or \code{\link{cv.glmnet}} object
}
  \item{lambdaindices}{
restrict to only the lambdas at these indices
}
}
\value{
object of the same class and form as the one passed in.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
data(iris)
y<-rbinom(nrow(iris), 1, 0.5)
irismat<-factorsToDummyVariables(dfr=iris , betweenColAndLevel="")
cv.lnet<-cv.glmnet(irismat, y, family="binomial", standardize=FALSE, type.measure="auc")
cv.lnet.res<-restrictForLambda(cv.lnet, lambdaindices=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ restrict }
\keyword{ lambda }
