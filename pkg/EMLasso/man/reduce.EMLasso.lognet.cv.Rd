\name{reduce.EMLasso.lognet.cv}
\alias{"cv.emlasso-class"}
\alias{cv.emlasso}
\alias{reduce.EMLasso.lognet.cv}
\title{Reduce memory footprint of a list of \code{\link{cv.1l.emlasso}} objects and recalculates some criteria}
\usage{
  \method{reduce}{EMLasso.lognet.cv} (object, orgdfr,
  orgresp, samplesForFinalFit = 10, ..., verbosity = 0,
  splitPatternLastPart = "/", keepReducedResultList =
  FALSE)
}
\arguments{
  \item{object}{list of \code{\link{cv.1l.emlasso}} objects
  or object of class "reducedResultList"}

  \item{orgdfr}{original dataset used. If missing, the
  first one found is used}

  \item{orgresp}{original outcome vector used. If missing,
  the first one found is used}

  \item{samplesForFinalFit}{how many samples must be used
  in the final GLoMo to make a dataset for the final refit}

  \item{\dots}{passed on to
  \code{\link{getSortedReducedResultList}} if needed}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{splitPatternLastPart}{see
  \code{\link{getSortedReducedResultList}}}

  \item{keepReducedResultList}{if \code{TRUE}, the
  resulting "reducedResultList" object is kept as an item
  of the return value}
}
\value{
  object of class "cv.emlasso" (and
  \code{\link{cv.glmnet}}, to be able to abuse some of
  \code{\link{glmnet}}'s code). (where I write dataset,
  this could either be a \code{\link{data.frame}} or
  \code{\link{numdfr}} object) Very similarly shaped to
  \code{\link{cv.glmnet}} objects (in fact, all objects of
  this class are also \code{\link{cv.glmnet}}) \enumerate{
  \item \code{lambda} : vector of lambda values \item
  \code{cvm}: mean criterion per lambda \item \code{cvsd}:
  sd of criterion per lambda \item \code{cvup}: upper limit
  of criterion per lambda \item \code{cvlo}: lower limit of
  criterion per lambda \item \code{cvlo}: number of
  nonzeroes per lambda \item \code{name}: name of criterion
  \item \code{glmnet.fit}: the actual final lasso fit \item
  \code{lambda.min}: optimum lambda \item
  \code{lambda.1se}: biggest lambda with criterion within 1
  se of best criterion \item \code{orgdfr}: dataset
  originally used \item \code{orgresp}: outcome vector
  originally used \item \code{glomo}: \code{\link{GLoMo}}
  object at convergence \item \code{valsample}: dataset
  used for validating (=by applying prediction from
  \code{glomo} to \code{orgdfr}) \item \code{orgcoef}:
  coefficients before refitting glmnet.fit \item
  \code{reducedResultList}: (may not be present): list that
  was used to create the results (see
  \code{\link{getSortedReducedResultList}}) }
}
\description{
  Reduce memory footprint of a list of
  \code{\link{cv.1l.emlasso}} objects and recalculates some
  criteria
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{reduce}}
}
\keyword{reduce}

