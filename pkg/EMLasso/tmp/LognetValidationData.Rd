\name{LognetValidationData}
\alias{LognetValidationData}
\title{Collect validation data for lognet}
\usage{
  LognetValidationData(glmnetmodel, dfr,
  betweenColAndLevel, outcomes, verbosity = 0)
}
\arguments{
  \item{glmnetmodel}{\code{\link{glmnet}} binomial model
  fit}

  \item{dfr}{dataset for which validation needs to occur}

  \item{betweenColAndLevel}{separator for dummy column
  naming}

  \item{outcomes}{true outcome vector}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  list of class "LognetValidationData":
  \item{valModelWrapper }{result of
  \code{\link{lognetProbWrapper}}} \item{outcomes }{as
  passed in}
}
\description{
  Collect reusable validation data for lognet
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
irisdummy<-factorsToDummyVariables(iris, verbosity=10)
lnet<-glmnet(irisdummy, y, family="binomial")
LognetValidationData<-function(glmnetmodel=lnet, dfr=iris, betweenColAndLevel="", outcomes=y, verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{lognetProbWrapper}}
}

