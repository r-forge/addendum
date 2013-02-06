\name{restrictForVars}
\alias{restrictForVars}
\alias{restrictForVars.glmnet}
\alias{restrictForVars.cv.glmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a copy of the (\code{cv.})\code{glmnet} object for a subset of the variables
}
\description{
Create a copy of the (\code{cv.})\code{\link{glmnet}} object for a subset of the vriables
}
\usage{
\method{restrictForVars}{glmnet}(fit, vars, repsNeededForFirstOccurrence=3,...)
\method{restrictForVars}{cv.glmnet}(fit, vars, repsNeededForFirstOccurrence=3,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
\code{\link{glmnet}} or \code{\link{cv.glmnet}} object
}
  \item{vars}{
if \code{numeric} and of \code{length} 1, the number of variables (in order of occurrence). Otherwise, the row indexes (in \code{beta}) of the variables you want included. The default is 5.
}
  \item{repsNeededForFirstOccurrence}{
see among others  \code{\link{getOrderOfAppearance}}.
}
  \item{\dots}{
ignored for now
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
cv.lnet.red<-restrictForVars(cv.lnet, vars=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ restrict }
\keyword{ variable }
