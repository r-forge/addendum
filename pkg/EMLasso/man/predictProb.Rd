\name{predictProb}
\alias{predictProb}
\alias{predictProb.lognetProbabilityReusable}
\title{predict probabilities}
\usage{
  predictProb(object, newdata, ..., verbosity = 0)

  \method{predictProb}{lognetProbabilityReusable} (object,
    newdata, ..., verbosity = 0)
}
\arguments{
  \item{object}{object that is used to predict the
  probabilities}

  \item{newdata}{dataset for which the probabilities must
  be predicted}

  \item{\dots}{provided for extensibility (S3)}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  vector of probabilities of the same length as
  \code{nrow(newdata)}

  vector of probabilities of the same length as
  \code{nrow(newdata)}
}
\description{
  predict probabilities
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
	lpw<-lognetProbabilityReusable(lnet, imputeDs2FitDsProperties=iris.cp2, iris.nd2, usecol=5, verbosity=1)
predictProb(lpw, iris.nd2[seq(20),], verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{predict}
\keyword{probability}

