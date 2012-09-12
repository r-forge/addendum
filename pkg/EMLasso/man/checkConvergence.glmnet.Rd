\name{checkConvergence.glmnet}
\alias{checkConvergence.glmnet}
\alias{convergenceCheckCreator}
\title{Check whether convergence has occurred in the zeroness of coefficients}
\usage{
  checkConvergence.glmnet(coefs, minIt, maxIt, burnIn = 0,
    verbosity = 0)

  convergenceCheckCreator(minIt=20, maxIt=30, burnIn=0,
    basicCheckFunction=checkConvergence.glmnet)
}
\arguments{
  \item{coefs}{(sparse) matrix of coefficients
  (cols=variables, rows=iterations)}

  \item{minIt}{minimum number of iterations before
  convergence is possible}

  \item{maxIt}{maximum number of iterations before
  convergence is automatically assumed}

  \item{burnIn}{number of iterations that will be skipped
  for convergence checking}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{basicCheckFunction}{function (like
  \code{checkConvergence.glmnet}) that will do the actual
  work}
}
\value{
  a list holding items: \enumerate{ \item \code{converged}:
  a single logical (\code{TRUE} if convergence has happened
  or maxIt passed) \item \code{minIt}: as passed in \item
  \code{maxIt}: as passed in \item \code{burnIn}: as passed
  in \item \code{iterCount}: number of iterations that have
  already occurred \item \code{usedFunction}: characters,
  holding "checkConvergence.glmnet" }

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

