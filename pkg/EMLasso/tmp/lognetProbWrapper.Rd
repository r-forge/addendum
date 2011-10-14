\name{lognetProbWrapper}
\alias{lognetProbWrapper}
\title{Function to calculate predicted probability of a lognet fit}
\usage{
  lognetProbWrapper(lognet, dfr, betweenColAndLevel,
  usecol, verbosity = 0)
}
\arguments{
  \item{lognet}{binomial \code{\link{glmnet}} fit to use
  for prediction}

  \item{dfr}{\code{\link{numdfr}} or
  \code{\link{data.frame}} that holds the structure from
  which dataset predictions will need to happen; better
  yet, object of class "dfrConversionProbs" (see
  \code{\link{dfrConversionProbs}})}

  \item{betweenColAndLevel}{separator in dummy name coding}

  \item{usecol}{which of the column of beta must be used.
  Must be provided if \code{lognet} is not the result of a
  1-lambda \code\link{glmnet}} call.}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  object of class "lognetProbWrapper": list with items:
  \item{conversionProps }{\code{data.frame} with columns
  \code{fromCols} (original column number), \code{newNames}
  (new column name), \code{mustEqual} (which value must it
  equal for this level of the factor), \code{useBeta}
  (coefficient)} \item{originalLognet }{\code{lognet} that
  was passed in} \item{usedcol }{usecol that was passed in
  (or 1 if only it was missing)}
}
\description{
  Method that provides information that can be reused to
  predict probabilities from the same lognet several times
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
irisdummy<-factorsToDummyVariables(iris, verbosity=10)
lnet<-glmnet(irisdummy, y, family="binomial")
lognetProbWrapper(lnet, iris, betweenColAndLevel="", usecol=5, verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{dfrConversionProbs}}
}
\keyword{dummy}
\keyword{glmnet}
\keyword{lognet}
\keyword{predict}

