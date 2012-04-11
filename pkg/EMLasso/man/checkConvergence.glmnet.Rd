\name{checkConvergence.glmnet}
\alias{checkConvergence.glmnet}
\alias{convergenceCheckCreator}
\title{Check whether convergence has occurred in the zeroness of coefficients}
\usage{
  checkConvergence.glmnet(coefs, minIt, maxIt,
    verbosity = 0)

  convergenceCheckCreator(minIt, maxIt,
    basicCheckFunction=checkConvergence.glmnet)
}
\arguments{
  \item{coefs}{(sparse) matrix of coefficients
  (cols=variables, rows=iterations)}

  \item{minIt}{minimum number of iterations before
  convergence is possible}

  \item{maxIt}{maximum number of iterations before
  convergence is automatically assumed}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{basicCheckFunction}{function (like
  \code{checkConvergence.glmnet}) that will do the actual
  work}
}
\value{
  a single logical (\code{TRUE} is convergence has happened
  or maxIt passed)

  the result of \code{basicCheckFunction}
}
\description{
  Check whether convergence has occurred in the zeroness of
  coefficients
}
\note{
  the first row (initial estimates) and column (intercept)
  are skipped in the checks
}
\examples{
curset<-rbinom(11, 1, 0.5)
ret<-matrix(curset, nrow=1)
for(i in 1:10)
{
	chg<-rbinom(11, 1, 0.05)
	curset[chg==1] <- 1-curset[chg==1]
	ret<-rbind(ret, curset)
}
print(ret)
checkConvergence.glmnet(ret, 2, 20, verbosity = 2)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso}}
}
\keyword{convergence}
\keyword{zeroness}

