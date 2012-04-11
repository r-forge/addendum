\name{findUnivariateSignificancePVal}
\alias{findUnivariateSignificancePVal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find for each variable the p-value for its univariate relevance to the outcome variable
}
\description{
Find for each variable the p-value for its univariate relevance to the outcome variable
}
\usage{
findUnivariateSignificancePVal(dfr, outcomecol, betweenColAndLevel="", returnPValBelow=0.05, splitCats=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataset that holds several variables, among which the column \code{outcomecol}
}
  \item{outcomecol}{
column name(/nr) that you want to relate the other variables to. Note: for now, this must be a binary factor!
}
  \item{betweenColAndLevel}{
see \code{\link{dfrConversionProps}}
}
  \item{returnPValBelow}{
only results where the p-value is below this boundary are returned
}
  \item{splitCats}{
if \code{TRUE}, the levels of the categorical variables are evaluated individually.
}
}
\value{
a list that holds 2 items:
\item{dfr}{The passed in dataset, reduced to the columns selected}
\item{pvals}{named vector of p-values. the names are the column names}
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	iris.cpy<-iris
	iris.cpy$out<-factor(sample(c("a", "b"), nrow(iris.cpy), replace=TRUE))
	findUnivariateSignificancePVal(iris.cpy, outcomecol="out", returnPValBelow=0.35)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rownames }
\keyword{ extend }
