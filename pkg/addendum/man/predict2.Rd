\name{predict2}
\alias{predict2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
predict unbiased based on glmnet objects
}
\description{
predict unbiased based on \code{\link{glmnet}} objects (i.e. by fitting the matching
logistic regression)
}
\usage{
predict2(object, orgx, orgy, outname="out", newx, methodForNonConvergence=c("ridge", "brglm", "none"), s = NULL, type=c("link","response","coefficients","nonzero","class"), exact = FALSE, offset, ..., verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{\link{glmnet}} object to predict from.
}
  \item{orgx, orgy, outname, methodForNonConvergence}{
see \code{\link{lognetUnbias}}
}
  \item{newx, s, type, exact, offset, \dots}{
see \code{\link{predict.glmnet}}
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\details{
essentially \code{\link{lognetUnbias}} and then \code{\link{predict.glmnet}}.
}
\value{
similar to \code{\link{predict.glmnet}}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lognetUnbias}}, \code{\link{predict.glmnet}}
}
\examples{
y<-as.factor(rbinom(n=nrow(iris),prob=0.5,size=1))
x<-factorsToDummyVariables(iris)
mdl<-glmnet(x=x, y=y, family="binomial")
predict2(mdl, orgx=x, orgy=y, newx=x, type="response")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ predict }
\keyword{ bias }
