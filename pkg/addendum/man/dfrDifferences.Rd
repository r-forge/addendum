\name{dfrDifferences}
\alias{dfrDifferences}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the positions where two equally dimensioned dataframes have different values
}
\description{
Find the positions where two equally dimensioned dataframes have different values. If one
\code{\link{data.frame}} has \code{NA} and the other doesn't, that's a difference. If they 
both hold \code{NA}, this is not a difference.
}
\usage{
dfrDifferences(dfrl, dfrr, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfrl, dfrr}{
\code{\link{data.frame}}s
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug 
information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
matrix with two columns. Each row represents a position where a difference was found, the 
first column holds its row index, the second its column index.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iris.cpy<-randomNA(iris, n=0.1)
iris.cpy[1,1]<-1000
dfrDifferences(iris, iris.cpy, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ compare }
\keyword{ dataset }
\keyword{ NA }
