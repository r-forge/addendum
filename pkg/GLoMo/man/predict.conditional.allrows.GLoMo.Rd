\name{predict.conditional.allrows.GLoMo}
\alias{predict.conditional.allrows.GLoMo}
\alias{predict.conditional.GLoMo}
\alias{validateFunction.acceptall}
\alias{validateFunction.useprob}
\alias{validateFunction.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict from GLoMo model with conditional rejection
}
\description{
The method \code{\link{predict.GLoMo}} can sample to fill out missing values in
a dataset. There, this happens with only the GLoMo in mind. This function allows
to provide an extra function that might reject sampled data based on other
criteria. The non-allrows version does so for 1 row at a time.
}
\usage{
predict.conditional.allrows.GLoMo(object, nobs = 1, dfr, forrows = seq(nrow(dfr)), validateFunction = validateFunction.default, guiddata = NULL, otherData = NULL, initialSuccessRateGuess = 0.5, verbosity = 0, minimumSuccessRate=0.001,...)
predict.conditional.GLoMo(object, nobs=1, dfr, forrow, validateFunction=validateFunction.default, guiddata=NULL, otherData=NULL, initialSuccessRateGuess=0.5, verbosity=0, minimumSuccessRate=0.001,...)
validateFunction.acceptall(attempts, otherData, forrow, verbosity = 0)
validateFunction.useprob(attempts, otherData, forrow, verbosity = 0)
validateFunction.default(attempts, otherData, forrow, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{\link{GLoMo}} object
}
  \item{nobs}{
number of observations to sample. Can be a single integer or (for
\code{predict.conditional.allrows.GLoMo}) a vector of the same length as \code{forrows}.
}
  \item{dfr}{
\code{\link{data.frame}} or \code{\link{numdfr}} to sample observations for
}
  \item{forrows,forrow}{
Which of the row(s) from \code{dfr} should be considered
}
  \item{validateFunction}{
After the standard sampling of \code{\link{predict.GLoMo}}, the rows and
\code{otherData} are fed to this function that should return the row indices of
the accepted rows. The default is \code{\link{validateFunction.default}}, which
randomly accepts about half of the sampled rows.
}
  \item{guiddata}{
see \code{\link{getGuidData}}. If not provided, it is calculated.
}
  \item{otherData}{
Passed on to \code{validateFunction}. Typically contains an item per row in \code{dfr}
}
  \item{initialSuccessRateGuess}{
Used to sample too many rows with \code{\link{predict.GLoMo}}, based on how many the user expects to fail. Default is 0.5.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
	\item{minimumSuccessRate}{
To prevent conditional prediction to run 'forever' because all observations are
simply unlikely, you can pass along a minimum success rate (between 0 and 1): if
the attained success rate goes below this, one more attempt is done, and, if need
be, predictions are accepted randomly to get enough of them.
}
  \item{\dots}{
Ignored for now
}
  \item{attempts}{
\code{\link{data.frame}} or \code{\link{numdfr}} with unconditionally sampled data.
}
}
\details{
This function is mostly provided with the MCMC of EMLasso in mind (i.e. reject
based on a glmnet fit and matching true outcomes for each row in
\code{dfr}.

Typically, other \code{validateFunction}s will have to be created for this to
make sense. It is then up to the creator/user to make sure \code{otherData} is
consistent with what this specific \code{validateFunction} expects.

The signature of a \code{validateFunction} can be easily spied from
\code{validateFunction.default} (and is not repeated here to avoid maintenance
issues).

Specifically, \code{validateFunction.acceptall} accepts all rows,
\code{validateFunction.useprob} expects a passed along probability per row in
\code{otherData} and rejects with this probability, while
\code{validateFunction.default} does the same, but always with probability 0.5.

}
\value{
List with two items
  \item{predicted }{\code{\link{data.frame}} or \code{\link{numdfr}} (dependent
	on the \code{dfr} that was used in the original call to \code{\link{GLoMo}}
	that holds the sampled data}
  \item{glomorowsused }{vector that holds 1 item per row in \code{predicted},
	holding which rowindex in \code{glomo} was used (i.e. which cell was sampled)}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}
\note{
The non-allrows version works only for 1 row at a time.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{predict}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
iris.pred.cond<-predict.conditional.allrows.GLoMo(iris.glomo, nobs=5, dfr=iris.md.nd, verbosity=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ predict }
\keyword{ conditional }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line