\name{aucLikeLognet}
\alias{aucLikeLognet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate AUC and SD the way it is done in cv.glmnet
}
\description{
Calculate AUC and SD the way it is done in cv.glmnet
}
\usage{
aucLikeLognet(predictedprobabilities, out, wts=rep(1, length(out)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predictedprobabilities}{
vector of predicted probabilities for each observation
}
  \item{out}{
vector of true outcomes for each observation
}
  \item{wts}{
vector of weights for each observation
}
}
\value{
A list holding items cvm and cvs (similar to \code{\link{cv.glmnet}})
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

\seealso{
\code{\link{cv.glmnet}}
}
\examples{
pp<-runif(100)
out<-sample(2, 100, replace=TRUE)
aucLikeLognet(pp, out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ AUC }
\keyword{ cv.glmnet }
\keyword{ glmnet }
