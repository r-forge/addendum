\name{reduce.cv.1l.emlasso}
\alias{"cv.1l.emlasso.reduced-class"}
\alias{cv.1l.emlasso.reduced}
\alias{reduce.cv.1l.emlasso}
\title{Reduce memory footprint of cv.1l.emlasso object}
\usage{
  \method{reduce}{cv.1l.emlasso} (object, orgdfr, ...,
  verbosity = 0)
}
\arguments{
  \item{object}{\code{\link{cv.1l.emlasso}} object to
  reduce}

  \item{orgdfr}{original dataset that was used to fit it
  to}

  \item{\dots}{provided for conformance to S3
  \code{\link{reduce}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  An object of class "cv.1l.emlasso.reduced":"
}
\description{
  Reduce memory footprint of \code{\link{cv.1l.emlasso}}
  object
}
\note{
  mostly uses class \code{\link{numdfr.rep}}. (where I
  write dataset, this could either be a
  \code{\link{data.frame}} or \code{\link{numdfr}} object)
  \enumerate{ \item \code{lambda} : 1 lambda value \item
  \code{cvm}: mean criterion \item \code{cvsd}: sd of
  criterion \item \code{cvup}: upper limit of criterion
  \item \code{cvlo}: lower limit of criterion \item
  \code{name}: name of criterion \item \code{type.measure}:
  passed along name of criterion \item \code{grouped}:
  passed along grouped (see cv.glmnet) \item
  \code{coefHistory}: list of three items:\enumerate{ \item
  \code{coefs}: all coefficients (including zeroes) over
  all folds and all iterations, in sparse format. Rows=
  folds and iterations, Cols=(intercept) of parameter \item
  \code{fold}: for each row in coefs, from which fold does
  it come \item \code{iter}: for each row in coefs, which
  iteration is it from } \item \code{orgRowsPerFold}: list
  with an item per fold, each time a number vector holding
  which rows of the original dfr were used in this fold
  \item \code{valsample}: combined validation sets in
  numdfr.rep format:\enumerate{ \item \code{predicted}:
  validation dataset (note: typically will contain multiple
  rows per org row) \item \code{numRepPerRow}: named
  integer vector: names are row indices in dfr, values are
  how many times this row is reused in predicted \item
  \code{resp}: (TRUE) responses for each row in predicted
  \item \code{predProb}: predicted probability from the
  lasso of the relevant fold \item \code{fromfold}: for
  each row, which fold does it belong to } \item
  \code{lasso.fit}: lasso refit to the full validation set.
  \item \code{glomo}: combined version of the glomos of
  every fold (see combineGLoMos) }
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlcvfit<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlcvfit<-eml17
emlcvfit.red<-reduce(emlcvfit, verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{memory}
\keyword{reduce}

