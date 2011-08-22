\name{originalColumnNamesFromDummyVars}
\alias{originalColumnNamesFromDummyVars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse of factorsToDummyVariables
}
\description{
From the columnnames like the ones resulting from \code{factorsToDummyVariables}, try to find
which was the original column name
}
\usage{
originalColumnNamesFromDummyVars(dummyVarNames, dfr, betweenColAndLevel = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dummyVarNames}{
Vector of 'converted' column names. Typically these are the original column names with the factor level appended.
}
  \item{dfr}{
Dataframe holding the original columns (and their names and levels)
}
  \item{betweenColAndLevel}{
String that we expect between the original column name and the level.
}
}
\details{
This is not full-proof. For now, it doesn't actually look at the levels, but finds the original
names that each new name starts with...
}
\value{
data.frame with three columns
  \item{orgname }{Original name of the columns (from \code{dfr})}
  \item{dummyname }{Converted name of the columns}
  \item{rest }{Part of the new column name after the separator}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{factorsToDummyVariables}}
}
\examples{
irisconv<-factorsToDummyVariables(iris, betweenColAndLevel="_")
originalColumnNamesFromDummyVars(colnames(irisconv), iris, betweenColAndLevel="_")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dummy }
\keyword{ ~variable }% __ONLY ONE__ keyword per line
