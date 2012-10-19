\name{combineGlmnets}
\alias{combineGlmnets}
\alias{glmnetNoPredictors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine a list of glmnet models to one / create a glmnet with no predictors
}
\description{
Combine a list of glmnet models to one / create a glmnet with no predictors
}
\usage{
combineGlmnets(mdls, allVarNames, lambdas, useCall, verbosity=0)
glmnetNoPredictors(y, xvarnames, family=c("gaussian","binomial","poisson","multinomial","cox","mgaussian"), weights, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mdls}{
list holding \code{\link{glmnet}}-like models
}
  \item{allVarNames}{
optionally pass in all possible variables names here
}
  \item{lambdas}{
optionally pass in all lambda values here. these are expected to be in the same order as they occur in the models
}
  \item{useCall}{
optionally pass in the call object here
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{y}{
outcome. Is treated similarly as in \code{\link{glmnet}}
}
  \item{xvarnames}{
fake the glmnet model for this set of variable names. Should not be empty.
}
  \item{family, weights, lambda}{
see \code{\link{glmnet}}. Note that you have to specify the lambdas! Also: currently: some families are not properly supported
}
}
\value{
A glmnet object (technically, it has the same class as the first item of \code{mdls})
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

\seealso{
\code{\link{glmnet}}
}
\examples{
tsty<-sample(2,20,replace=TRUE)
tstg<-glmnetNoPredictors(y=tsty, xvarnames=c("aVar", "anotherVar"), family="binomial", lambda=c(0.02,0.03,0.04,0.05))
tstx<-matrix(runif(40), ncol=2, dimnames=list(character(), c("aVar", "anotherVar")))
predict(tstg, newx=tstx, type="response")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ combine }
