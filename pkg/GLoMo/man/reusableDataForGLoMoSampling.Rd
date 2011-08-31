\name{reusableDataForGLoMoSampling}
\alias{reusableDataForGLoMoSampling}
\alias{ReusableDataForGLoMoSampling}
\alias{ReusableDataForGLoMoSamplingForOneRow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate GLoMo data that is usable for repeated 'completing' of the same data row.
}
\description{
Calculate GLoMo data that is usable for repeated 'completing' of the same data row.
}
\usage{
reusableDataForGLoMoSampling(glomo, dfr, forrows = seq(nrow(dfr)), guiddata = NULL, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glomo}{
\code{\link{GLoMo}} object
}
  \item{dfr}{
\code{\link{data.frame}} or \code{\link{numdfr}} that needs to be 'completed'.
}
  \item{forrows}{
which of the rows in \code{dfr} need completion (defaults to all of them)
}
  \item{guiddata}{
"GuidData" class object for the rows in question, or character vector of uids
for those rows, or \code{NULL}
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
A list of class "ReusableDataForGLoMoSampling", holding items:
  \item{guiddata }{like to the return value of \code{\link{getGuidData}}}
  \item{forrows }{copy of the \code{forrows} that was passed in the function}
  \item{perrow }{list that holds one item of class "ReusableDataForGLoMoSamplingForOneRow" per element of \code{forrows}}
  
The structure of lists of class "ReusableDataForGLoMoSamplingForOneRow" is like this:
  \item{a }{See "Analysis of Incomplete Multivariate Data" around p349}
  \item{useSigma }{covariance for missing continuous values of that row}
  \item{sigLeft }{See "Analysis of Incomplete Multivariate Data" around p349}
  \item{probs }{conditional probabilities of each 'cell' (see the members of \code{guiddata})}
  \item{whichCntColNotNA }{within the continuous columns, the how manieth of them was not NA for this row}
  \item{whichCntColNA }{within the continuous columns, the how manieth of them was NA for this row}
  \item{presentCntColsInDfr }{column indices within \code{dfr} of the continuous columns that did not have NA for this row}
  \item{missingCntColsInDfr }{column indices within \code{dfr} of the continuous columns that had NA for this row}
}
\references{
"Analysis of Incomplete Multivariate Data" around p349, and also "Statistical Analysis with Missing Values"
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}
\note{
This method is mainly present because of \code{\link{predict.conditional.GLoMo}},
where there is repeated sampling from the same row.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{predict.conditional.GLoMo}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
mdrow<-min(which(apply(iris.md, 1, function(currow){any(is.na(currow))})))
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
reusableDataForGLoMoSampling(glomo=iris.glomo, dfr=iris.md.nd, forrows = mdrow,
	verbosity = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sample }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line
