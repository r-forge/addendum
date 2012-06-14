\name{factorsToDummyVariables}
\alias{factorsToDummyVariables}
\alias{factorsToDummyVariables.default}
\alias{dfrConversionProps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quickly create a design matrix for simple cases
}
\description{
\code{model.frame} is quite slow for simple cases and big datasets. this function
is a lot more swift, but only works for formulas with all main effects.
}
\usage{
\method{factorsToDummyVariables}{default}(dfr, betweenColAndLevel = "", dfrConvData, verbosity=0, \dots)
dfrConversionProps(dfr, betweenColAndLevel, includeBaseLevel=FALSE, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe for which the design matrix must be made
}
  \item{betweenColAndLevel}{
If factors are present, what character should be between the original name and the value name?
}
  \item{dfrConvData}{
Prepared conversion info (return value of \code{dfrConversionProps}). Can be
used (and is preferred) to avoid repeating calculations.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Passed on to \code{dfrConversionProps} if needed (in most implementations). Otherwise ignored.
}
  \item{includeBaseLevel}{
Prepared conversion info (return value of \code{dfrConversionProps}). Can be
used (and is preferred) to avoid repeating calculations.
}
}
\details{
Similar to:
\code{
	dfrTmp<-model.frame(dfr, na.action=na.pass)
	dfrTmp<-cbind(outcome=1, dfrTmp)
	return(as.matrix("outcome~.", data=dfrTmp))[,-1])
}
}
\value{
Matrix with a row for every observation and a column for every numerical variable/ level of factor.
}
\author{
Nick Sabbe
}
\note{
Typical use is for \code{\link[glmnet]{glmnet}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{originalColumnNamesFromDummyVars}}
}
\examples{
iristmp<-iris
iristmp$tst<-factor(letters[sample.int(5, nrow(iris), replace=TRUE)], ordered=TRUE)

irisdcp<-dfrConversionProps(dfr = iristmp, betweenColAndLevel = "", includeBaseLevel = FALSE, verbosity = 4)
irismat<-factorsToDummyVariables(dfr=iristmp, dfrConvData=irisdcp, betweenColAndLevel="", verbosity=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ factor }
\keyword{ dummy }% __ONLY ONE__ keyword per line
\keyword{ coding }
