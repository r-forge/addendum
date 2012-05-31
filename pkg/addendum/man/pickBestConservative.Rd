\name{pickBestConservative}
\alias{pickBestConservative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use the knee principle to find a conservative best value
}
\description{
Given a set of 'criterion' estimate and their variances, find the index where the
first value below the maximum + a multiple of its SD is.
}
\usage{
pickBestConservative(crit, varcrit, findMaxOf = crit, timesSE = 1, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{crit}{
vector of criterion values (e.g. AUCs)
}
  \item{varcrit}{
known/estimated variance for each of the criterion values
}
  \item{findMaxOf}{
for each item in crit, after subsetting for valid values based on
   crit, this is the value that is maximized within the remaining set. e.g. in
   \code{glmnet} find the smallest \code{lambda} that has AUC below maxAUC-seAUC. Note: if
   you want the smallest value just negate the values...
}
  \item{timesSE}{
how many times should the SE be subtracted from the highest crit
   (could)be zero!!
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\details{
If no values smaller than the maximum minus the multiple of its SD are found, the index of the minimum is returned.
}
\value{
The index where the 'best' value is found
}
\author{
Nick Sabbe
}
\note{
The criterion is actually not exactly the same as in \code{glmnet}: it appears they use the last observations that is _within_
one SE from the maximum, while we use the first one that is just outside one SE from the maximum...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[glmnet]{cv.glmnet}}
}
\examples{
covs<-matrix(rnorm(1000), ncol=20)
colnames(covs)<-paste("X", 1:20, sep="")
lin<-5+10*covs[,1]
probs<-exp(lin)/(1+exp(lin))
y<-sapply(probs, function(pr){rbinom(1,1,pr)})
cvg<-cv.glmnet(covs, y, family="binomial")
#to get nearly the same lambda as cvg$lambda.1se
ind1se<-pickBestConservative(-cvg$cvm, cvg$cvsd^2, -cvg$lambda, verbosity=10)
cvg$lambda[ind1se]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ knee }
\keyword{ elbow }
\keyword{ criterion }
