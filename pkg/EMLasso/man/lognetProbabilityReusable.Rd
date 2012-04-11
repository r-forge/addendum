\name{lognetProbabilityReusable}
\alias{lognetProbabilityReusable}
\title{Function to help calculate predicted probability of a lognet fit}
\usage{
  lognetProbabilityReusable(lognet, ds,
    imputeDs2FitDsProperties = normalImputationConversion(),
    usecol, verbosity = 0)
}
\arguments{
  \item{lognet}{binomial \code{\link{glmnet}} fit to use
  for prediction}

  \item{ds}{\code{\link{numdfr}} or
  \code{\link{data.frame}} that holds the structure from
  which dataset predictions will need to happen.}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} object that will provide the
  conversion from imputed dataset to one that is ready for
  fitting the predictor model}

  \item{usecol}{which of the column of beta must be used.
  Must be provided if \code{lognet} is not the result of a
  1-lambda \code{\link{glmnet}} call.}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  object of class "lognetProbabilityReusable": list with
  items: \item{conversionProps }{\code{data.frame} with
  columns \code{fromCols} (original column number),
  \code{newNames} (new column name), \code{mustEqual}
  (which value must it equal for this level of the factor),
  \code{useBeta} (coefficient)} \item{originalLognet
  }{\code{lognet} that was passed in} \item{usedcol
  }{usecol that was passed in (or 1 if only it was
  missing)}
}
\description{
  Method that provides information that can be reused to
  predict probabilities from the same lognet several times
}
\examples{
data(iris)
	iris.nd2<-numdfr(iris)
	y2<-rbinom(nrow(iris), 1, 0.5)
	iris.nic2<-normalImputationConversion(
		scalingParams=typicalScaleAndCenter(),
		transformParams=typicalTransformations())
	iris.cp2<-imputeDs2FitDsProps(iris.nic2,iris.nd2,verbosity=1)

	iris.cvtd2<-imputeDs2FitDs(iris.cp2,ds=iris.nd2,verbosity=3)

	lnet<-glmnet(iris.cvtd2, y2, family="binomial")
	lognetProbabilityReusable(lnet, iris.nd2, imputeDs2FitDsProperties=iris.cp2, usecol=5, verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{dfrConversionProps}}
}
\keyword{dummy}
\keyword{glmnet}
\keyword{lognet}
\keyword{predict}

