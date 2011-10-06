\name{internalMissClassificationPerLambdaLognet}
\alias{internalMissClassificationPerLambdaLognet}
\alias{smallestInternallyPerfectLognet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Investigate performance of lognet on the original data (without crossvalidation)
}
\description{
Get/count nonzero coefficients, especially for 1 or more \code{\link{glmnet}} or
\code{\link{cv.glmnet}} objects. If several objects are presented in a list, only
the coefficients that are nonzero more than a given number of times are returned.
}
\usage{
internalMissClassificationPerLambdaLognet(x, y, family="binomial", probthres=0.5, ...) 
smallestInternallyPerfectLognet(x, y, family="binomial", probthres=0.5, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
data to fit the lognet to -- see \code{\link{glmnet}} for the requirements
}
  \item{family}{
Simply there to avoid it being present in \dots It's value will be ignored, because
these methods only work for binomial.
}
  \item{probthres}{
threshold above which the predicted probability must be to be classified as a positive.
}
  \item{\dots}{
passed on to \code{\link{glmnet}}
}
}
\value{
For \code{internalMissClassificationPerLambdaLognet}, a list:
\itemize{
\item \code{lambda}: vector of lambdas from the \code{\link{glmnet}} fit.
\item \code{mcls}: matrix with 3 rows and a column per \code{lambda}. The three rows hold the \code{falsepos} , \code{falseneg} and \code{missclass} numbers per \code{lambda}
\item \code{numpos}: how many of the original \code{y} were positive (=2nd level)
\item \code{numneg}: how many of the original \code{y} were negative (=1st level)
}

For \code{smallestInternallyPerfectLognet}, a list:
\itemize{
\item \code{lambdaIndex}: which of the lambdas of the \code{\link{glmnet}} fit is the biggest 
\code{lambda} for which the prediction of the original data is perfect. \code{NA} if there was 
no such \code{lambda}.
\item \code{lambda}: \code{lambda} of the \code{\link{glmnet}} fit that is the biggest 
\code{lambda} for which the prediction of the original data is perfect. \code{NA} if there was 
no such \code{lambda}.
\item \code{coefs}: result of \code{\link{showInterestingCoef}} for the resulting \code{lambda},
or \code{NA} if no \code{lambda} was found.
\item \code{lgnet}: the \code{\link{glmnet}}.
}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{showInterestingCoef}}
}
\examples{
dat<-matrix(rnorm(1000), ncol=50)
y1<-as.factor(rbinom(20,1,0.5))
y2<-as.factor(rbinom(20,1,0.5))

internalMissClassificationPerLambdaLognet(dat, y1) 
smallestInternallyPerfectLognet(dat, y1) 
internalMissClassificationPerLambdaLognet(dat, y2) 
smallestInternallyPerfectLognet(dat, y2) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ coefficient }
\keyword{ nonzero }
