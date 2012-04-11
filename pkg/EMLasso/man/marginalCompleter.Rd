\name{marginalCompleter}
\alias{completeMarginal}
\alias{completeMarginal.default}
\alias{completeMarginal.function}
\alias{marginalCompleted}
\alias{marginalCompleted-class}
\alias{marginalCompleter}
\title{Wrapper to complete a dataset based on (univariate) marginal distributions}
\usage{
  marginalCompleter(weightsName = "weights",
    orgriName = "orgri", maxFullNACatCols = 6,
    howManyIfTooMany = 1000)

  completeMarginal(object,ds, out, rowsToUse, verbosity=0)

  \method{completeMarginal}{function}(object,ds, out,
    rowsToUse, verbosity=0)

  \method{completeMarginal}{default}(object,ds, out,
    rowsToUse, verbosity=0)
}
\arguments{
  \item{weightsName}{object that holds information to
  convert an imputed dataset like
  \code{\link{dfrConversionProps}} or a custom
  implementation.}

  \item{orgriName}{dataset for which the lambdas need to be
  found}

  \item{maxFullNACatCols}{see
  \code{\link{rCatsAndCntInDfr}}}

  \item{howManyIfTooMany}{see
  \code{\link{rCatsAndCntInDfr}}}

  \item{object}{object/function that will perform the
  actual imputation}

  \item{ds}{dataset that needs completion}

  \item{out}{outcomes}

  \item{rowsToUse}{rowindices of the rows that can actually
  be used}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  function that can actually perform the conversion -- used
  in \code{\link{EMLasso.1l}}

  object of class \code{marginalCompleted}, holding items:
  \enumerate{ \item \code{ds}: completed dataset \item
  \code{out}: outcome variable \item \code{weights}:
  weights to apply to each row of \code{ds} \item
  \code{completedUse}: rowindices within \code{ds} to be
  actually used }
}
\description{
  Wrapper to complete a dataset based on (univariate)
  marginal distributions
}
\note{
  abstracts away creation of marginal completion: see
  \code{\link{EMLasso.1l}}
}
\seealso{
  \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}
}
\keyword{dataset}
\keyword{impute}
\keyword{marginal}

