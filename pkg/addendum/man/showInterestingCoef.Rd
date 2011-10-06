\name{showInterestingCoef}
\alias{showInterestingCoef}
\alias{showInterestingCoef.default}
\alias{showInterestingCoef.Matrix}
\alias{showInterestingCoef.matrix}
\alias{showInterestingCoef.list}
\alias{showInterestingCoef.cv.glmnet}
\alias{showInterestingCoef.glmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get/count nonzero coefficients
}
\description{
Get/count nonzero coefficients, especially for 1 or more \code{\link{glmnet}} or
\code{\link{cv.glmnet}} objects. If several objects are presented in a list, only
the coefficients that are nonzero more than a given number of times are returned.
}
\usage{
showInterestingCoef(object,...)
\method{showInterestingCoef}{matrix}(object, whichCol,...)
\method{showInterestingCoef}{Matrix}(object, whichCol,...)
\method{showInterestingCoef}{list}(object, atLeast=1,...)
\method{showInterestingCoef}{cv.glmnet}(object, whichLambda="lambda.1se", ...)
\method{showInterestingCoef}{glmnet}(object, whichLambda=object$lambda[1], ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object for which the nonzero coefficients need to be found/counted
}
  \item{whichCol}{
interesting coefficients of which column. If missing or \code{NA}, the columns are 
coerced to a list of vectors and \code{showInterestingCoef.list} is used on that.
}
  \item{atLeast}{
How often must the coefficient be nonzero in the list to be returned
}
  \item{whichLambda}{
see parameter \code{s} for \code{\link{predict.glmnet}} and \code{\link{predict.cv.glmnet}}
}
  \item{\dots}{
typically passed on to lower level \code{showInterestingCoef}
}
}
\value{
In case of a list, a \code{\link{table}} object is returned, holding the counts. Otherwise,
a named vector of the nonzero coefficients (including "(intercept)")
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{showNonZeroCoef}}
}
\examples{
dat<-matrix(rnorm(1000), ncol=20)
y1<-rnorm(50)
y2<-rnorm(50)
cv1<-cv.glmnet(dat, y1)
cv2<-cv.glmnet(dat, y2, lambda=cv1$lambda)
showInterestingCoef(cv1, whichLambda="lambda.1se")
showInterestingCoef(cv1, whichLambda="lambda.min")
showInterestingCoef(cv1, whichLambda=NULL)
showInterestingCoef(cv1$glmnet.fit, whichLambda=cv1$glmnet.fit$lambda[10])
showInterestingCoef(cv1$glmnet.fit)

showInterestingCoef(list(cv1, cv2), atLeast=1, whichLambda="lambda.min")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ coefficient }
\keyword{ nonzero }
