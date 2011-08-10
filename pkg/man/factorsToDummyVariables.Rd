\name{factorsToDummyVariables}
\alias{factorsToDummyVariables}
\alias{factorsToDummyVariables.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quickly create design matrix for simple cases
}
\description{
\code{model.frame} is quite slow for simple cases and big datasets. this function
is a lot more swift, but only works for formulas with all main effects.
}
\usage{
\method{factorsToDummyVariables}{default}(dfr, betweenColAndLevel = "", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe for which the design matrix must be made
}
  \item{betweenColAndLevel}{
If factors are present, what character should be between the original name and the value name?
}
  \item{\dots}{
Ignored in standard implementations
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
factorsToDummyVariables(iris, betweenColAndLevel="_")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
