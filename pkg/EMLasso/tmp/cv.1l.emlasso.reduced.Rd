\docType{S3class}
\name{cv.1l.emlasso.reduced}
\alias{cv.1l.emlasso.reduced}
\title{Structure of cv.1l.emlasso.reduced object}
\description{
  Structure of cv.1l.emlasso.reduced object
}
\details{
  NOTE:

  where I write dataset, this could either be a
  \code{\link{data.frame}} or \code{\link{numdfr}} object.
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

