\name{calcAUC.Binary}
\alias{calcAUC.Binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate AUC from predicted probabilities
}
\description{
Given the predicted probabilities and vectors indictaing the 'truth', calculates
the AUC and its variance, either through bootstrap or from a formula
}
\usage{
calcAUC.Binary(probs, trueOnes, trueZeroes, bootStrap = 0, includeSE = FALSE, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probs}{
Predicted probabilities for each outcome
}
  \item{trueOnes}{
Observations that are 'true ones', i.e. for which ideally the predicted probability should be 1.
Can be passed as a logical vector (of the same length of \code{probs}) or vector of indices within \code{probs}.
}
  \item{trueZeroes}{
Observations that are 'true zeroes', i.e. for which ideally the predicted probability should be 0.
If no provided, the observations not in \code{trueOnes}.
}
  \item{bootStrap}{
Use bootstrap methods (if this parameter is greater than zero: the number of bootstrap iterations) or not.
}
  \item{includeSE}{
If true, the variance of the AUC is added to the return value
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\details{
Look at the code for the details on the used formulas.
}
\value{
When \code{includeSE} is \code{false}, simply returns the AUC as one number.
Otherwise, a list with items:
  \item{AUC }{the estimated AUC}
  \item{varAUC }{its estimated variance}
}
\references{
Hanley en McNeil 1982
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{seAUC}}
}
\examples{
dat<-data.frame(x=rbinom(100, 1, 0.5), y=rnorm(100))
bfit<-glm(x~y, data=dat, family=binomial)
pprob<-predict(bfit, type="response")
calcAUC.Binary(probs=pprob, trueOnes=as.logical(dat$x), bootStrap=1000, includeSE=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~AUC }
\keyword{ ~ROC }
\keyword{ ~bootstrap }
\keyword{ ~variance }
