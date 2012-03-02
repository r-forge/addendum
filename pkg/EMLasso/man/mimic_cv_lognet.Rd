\name{mimic_cv_lognet}
\alias{mimic_cv_lognet}
\title{Calculate criteria similarly as is done in glmnet, but for EMLasso}
\usage{
  mimic_cv_lognet(cv.eml, type.measure = "auc",
    grouped = TRUE, verbosity = 0,
    useAsFitObject = "lasso.fit")
}
\arguments{
  \item{cv.eml}{list of \code{\link{cvpart.emlasso}}
  objects (return value of
  \code{\link{EMLasso.1l.lognet.onefold}}). These are
  supposedly the results for fitting the different 'folds'
  of a crossvalidation}

  \item{type.measure}{see \code{\link{cv.glmnet}}}

  \item{grouped}{see \code{\link{cv.glmnet}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{useAsFitObject}{either "lasso.fit" or "logreg.fit".
  The second option uses a potentially less biased fit: it
  is the re-fit of a simple logistic regression with only
  the nonzero variables. See also
  \code{\link{cv.1l.emlasso}}}
}
\value{
  see \code{\link{cv.glmnet}}
}
\description{
  Calculate criteria similarly as is done in glmnet, but
  for EMLasso
}
\note{
  used internally in \code{\link{EMLasso.1l.lognet.cv}}
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l.lognet.cv}}
}
\keyword{crossvalidate}
\keyword{glmnet}

