\name{retrieveLambdas}
\alias{retrieveLambdas}
\alias{retrieveLambdas.default}
\alias{retrieveLambdas.smartLambdaRetriever}
\alias{smartLambdaRetriever}
\alias{smartLambdaRetriever-class}
\title{Retrieve lambdas}
\usage{
  retrieveLambdas(lambdas, ds, out, ...)

  \method{retrieveLambdas}{default}(lambdas,ds,out,...)

  \method{retrieveLambdas}{default}(lambdas,ds,out,...)

  smartLambdaRetriever(...)
}
\arguments{
  \item{lambdas}{simple set of lambda values for the
  default, or an instance of a class like
  \code{smartLambdaRetriever} for which a specific version
  of the method is implemented.}

  \item{ds}{dataset for which the lambdas need to be found}

  \item{out}{outcome variable}

  \item{\dots}{For specific implementations, e.g. passed on
  to \code{\link{findReasonableLambdaHelper}} in
  \code{retrieveLambdas.smartLambdaRetriever}}
}
\value{
  vector of lambda values - in fact, \code{lambdas}

  vector of lambda values through
  \code{\link{findReasonableLambdaHelper}}
}
\description{
  Retrieve lambdas
}
\note{
  abstracts away selection of lambdas: see
  \code{\link{EMLasso}}

  Constructor for \code{smartLambdaRetriever} objects. See
  \code{\link{findReasonableLambdaHelper}} for what could
  be passed through \dots
}
\seealso{
  \code{\link{EMLasso}}

  \code{\link{EMLasso}}

  \code{\link{EMLasso}},
  \code{\link{findReasonableLambdaHelper}}
}
\keyword{lambda}
\keyword{selection}

