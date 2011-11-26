\name{plot.cv.glmnet}
\alias{plot.cv.glmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extended plotting of cv.glmnet objects
}
\description{
Extended plotting of \code{\link{cv.glmnet}} objects
}
\usage{
\method{plot}{cv.glmnet}(x, sign.lambda = 1, ..., add=FALSE, errbarcol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, sign.lambda, \dots}{
see the original version of this method in \code{glmnet}.
}
  \item{add}{
if \code{TRUE}, the current plot is added to an existing one. Useful for comparing several
\code{\link{cv.glmnet}} objects' performance
}
  \item{errbarcol}{
color that is used for the error bars. If missing and \code{col} was specified in \dots, a version of
this color with alpha=50 is created, otherwise "darkgrey" is used.
}
}
\details{
\code{simpleplot} is really just a helper to allow extension to \code{\link{glmnet}}-
like objects.
}
\value{
The result of the \code{\link{addCVPlot}} call is returned invisibly.

\code{getOrderOfAppearance} returns a list with items \code{legendForVars} (variable names ordered
by occurrence), \code{whereAppearing} (at which lambda index does this variable occur for the first 
time), \code{whereAsInPlotCoef} (which variable indices as used in \code{\link{plotCoef}} --- note
the variables that are always zero do not occur there), \code{orderOfFirstAppearance} (which 
variable indices in the set of all indices)
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{addCVPlot}}
}
\examples{
y1<-as.factor(rbinom(n=nrow(iris),prob=0.5,size=1))
y2<-as.factor(rbinom(n=nrow(iris),prob=0.5,size=1))
x<-factorsToDummyVariables(iris)
cv1<-cv.glmnet(x=x, y=y1, family="binomial")
cv2<-cv.glmnet(x=x, y=y2, family="binomial", lambda=cv1$glmnet.fit$lambda)
plot(cv1, col="red")
plot(cv2, col="green", add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cv.glmnet }
\keyword{ plot }
\keyword{ add }
