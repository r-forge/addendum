\docType{S3class}
\name{cv.1l.emlasso-class}
\alias{"cv.1l.emlasso-class"}
\alias{"cvpart.emlasso-class"}
\alias{cv.1l.emlasso}
\alias{cv.1l.emlasso-class}
\alias{cvpart.emlasso}
\title{Structure of cv.1l.emlasso object}
\description{
  Structure of cv.1l.emlasso object
}
\details{
  NOTE:

  where I write dataset, this could either be a
  \code{\link{data.frame}} or \code{\link{numdfr}} object.
  \enumerate{ \item \code{lambda} : 1 lambda value \item
  \code{cvm}: mean criterion \item \code{cvsd}: sd of
  criterion \item \code{cvup}: upper limit of criterion
  \item \code{cvlo}: lower limit of criterion \item
  \code{name}: name of criterion \item \code{actualfits}:
  list of fits per fold, each of class "cvpart.emlasso"
  \enumerate{ \item \code{fitInfo}: result of
  EMLasso.1l.lognet (of class
  EMLasso.1l.lognet):\enumerate{ \item \code{lasso.fit}:
  glmnet object \item \code{glomo}: final predictor fit (of
  class GLoMo) \item \code{coefs}: coefs for all iterations
  (rows) and dummycoded columnames (columns), incl.
  (intercept). \item \code{dfr}: original dataset passed
  along \item \code{resp}: outcome variable (1 for each row
  in dfr) \item \code{lambda}: 1 lambda value \item
  \code{nrOfSamplesPerMDRow}: how many imputations per row
  with missing data \item \code{maxIt}: maximum number of
  iterations until convergence \item \code{minIt}: minimum
  number of iterations before convergence is checked \item
  \code{rowsToUseForFit}: which of the rows in dfr was used
  to fit the lasso \item \code{iterCount}: how many
  iterations occurred before convergence / maxIt \item
  \code{logreg.fit}: simple logistic regression fit within
  the columns selected in lasso.fit, applied to the same
  dataset that lasso.fit was obtained from ((fold-1)/fold
  part of the data, but imputed). Note: this is achieved
  through glmnet with lambda=0, so this object is of class
  glmnet! } \item \code{valsample}: result of
  predict.GLoMo(returnRepeats=TRUE) (essentially imputed
  dataset of the validation part of the original data) +
  extra item resp:\enumerate{ \item \code{predicted}:
  dataset used for validation \item \code{numRepPerRow}:
  named integer vector: names are row indices in dfr,
  values are how many times this row is reused in predicted
  \item \code{resp}: outcome variable from the original
  dfr, but properly subsetted and extended so there is one
  (correct) outcome for each row of \item \code{criteria}:
  result of calculateCriteria(.EMLasso.1l.lognet): \item
  \code{predictionSummary}: rows with mcrate, fposrate,
  fnegrate and mcrate.se over a set of fixed thresholds
  (columns) from 0.05 to 0.95 by 0.05 \item \code{AUC} +
  \code{varAUC} \item \code{AUCbs} + \code{varAUCbs}: AUC +
  its variance estimated by bootstrap \item \code{N}:
  number of observations used for this validation \item
  \code{predProb}: predicted probabilities per observation
  (of valsample) of the last lasso.fit \item
  \code{predProb_logreg}: similar, but now of logreg.fit }
  } \item \code{type.measure}: passed along name of
  criterion \item \code{grouped}: passed along grouped (see
  cv.glmnet) \item \code{nzero}: mean coefficients for all
  coefficients that were at most maxFracZeroesAllowed times
  0 in the final fit for each of the folds }
}

