\name{predictProb}
\alias{predictProb}
\alias{predictProb.lognetProbWrapper}
\title{predict probabilities}
\usage{
  predictProb(object, newdata, ...)

  \method{predictProb}{lognetProbWrapper} (object, newdata,
    ..., verbosity = 0)
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
y<-rbinom(nrow(iris), 1, 0.5)
irisdummy<-factorsToDummyVariables(iris, verbosity=10)
lnet<-glmnet(irisdummy, y, family="binomial")
lpw<-lognetProbWrapper(lnet, iris, betweenColAndLevel="", usecol=5, verbosity=1)
predictProb(lpw, iris[seq(20),], verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{predict}
\keyword{probability}

