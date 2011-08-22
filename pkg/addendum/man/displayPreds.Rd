\name{displayPreds}
\alias{displayPreds}
\alias{displayPreds.default}
\alias{displayPreds.CVRes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display results of doFormulaContCV
}
\description{
Simply displays the results of several \code{\link{doFormulaContCV}} type of anlyses, so they can easily be compared.
}
\usage{
displayPreds(res, dfr, outcol, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
(list of) results of one ore more analyses.
}
  \item{dfr}{
Dataframe on which the analyses were performed.
}
  \item{outcol}{
Name of the outcome column
}
  \item{\dots}{
Extra parameters (not really used)
}
}
\value{
A Dataframe with one row for each observation and three columns:
  \item{trueVal }{original outcome value}
  \item{predicted }{predicted outcome value}
  \item{ssq }{Square difference}
}
\author{
Nick Sabbe
}
\note{
This function sidesteps any \code{evaluatePredictions} used in the \code{\link{doFormulaContCV}} call(s), and
always displays squared error.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{doFormulaContCV}}
}
\examples{
require(randomForest)
rfres<-doFormulaContCV(iris, outcol="Sepal.Length", fitFunc=randomForest,
  fitAndPredictContinuous="",	verbosity=5,ntree=500)
\dontrun{displayPreds(rfres, iris, "Sepal.Length")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~continuous }
\keyword{ ~prediction }
\keyword{ ~crossvalidation }
