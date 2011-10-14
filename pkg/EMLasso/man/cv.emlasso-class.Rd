\docType{S3class}
\name{cv.emlasso-class}
\alias{"cv.emlasso-class"}
\alias{cv.emlasso}
\alias{cv.emlasso-class}
\title{Structure of cv.emlasso object}
\description{
  Structure of cv.emlasso object
}
\details{
  NOTE:

  where I write dataset, this could either be a
  \code{\link{data.frame}} or \code{\link{numdfr}} object.
  Very similarly shaped to \code{\link{cv.glmnet}} objects
  (in fact, all objects of this class are also
  \code{\link{cv.glmnet}}) \enumerate{ \item \code{lambda}
  : vector of lambda values \item \code{cvm}: mean
  criterion per lambda \item \code{cvsd}: sd of criterion
  per lambda \item \code{cvup}: upper limit of criterion
  per lambda \item \code{cvlo}: lower limit of criterion
  per lambda \item \code{cvlo}: number of nonzeroes per
  lambda \item \code{name}: name of criterion \item
  \code{glmnet.fit}: the actual final lasso fit \item
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

