\name{lognetUnbias}
\alias{lognetUnbias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit simple logistic regression with the selected variables
}
\description{
Fit simple logistic regression with the selected variables, and return this as a fake \code{\link{glmnet}} object.
}
\usage{
lognetUnbias(lnet, orgx, orgy, outname="out", verbosity=0, methodForNonConvergence=c("ridge", "brglm", "none"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lnet}{
\code{\link{glmnet}} object, supposedly logistic
}
  \item{orgx, orgy}{
data to which the logistic regression must be fit. Typically, this is the same data as was passed
to \code{\link{glmnet}} in the first place
}
  \item{outname}{
Column name that is not in \code{orgx}, so it will not conflict.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{methodForNonConvergence}{
For big models, simple logistic regression may not converge. This holds the alternatives for that case: "ridge" (logistic regression with the selected variables + very light ridge), "brglm" (\code{brglm} w selected variables), "none" (simple
logistic regression)
}
}
\value{
A \code{\link{glmnet}} object that looks exactly like \code{lnet}. In the current implementation, only the
following items are altered: \code{beta}, \code{a0}, \code{df} (so NOT \code{dev.ratio} or \code{nulldev} --- 
this may have consequences to be investigated later). In addition, it holds an extra item \code{convergencePerLambda}
that holds a boolean per lambda, indicating whether or not the model converged / had the same number of variables
as the original. For easy reference, the original values of the above are also present in items \code{orgbeta}, 
\code{orga0}, \code{orgdf}.
}
\note{
Perhaps some more fields need to be altered.

Theoretically, all \code{\link{glmnet}} functions (like \code{\link{plot.glmnet}}) can be
applied to this new object.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

\seealso{
\code{\link{glmnet}}, \code{\link{glm}}
}
\examples{
y<-as.factor(rbinom(n=nrow(iris),prob=0.5,size=1))
x<-factorsToDummyVariables(iris)
orgmdl<-glmnet(x=x, y=y, family="binomial")
unbmdl<-lognetUnbias(orgmdl, orgx=x, orgy=y, verbosity=1, methodForNonConvergence="ridge")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ logistic }
\keyword{ bias }
