\name{makeFactorsAndSetFrontal}
\alias{makeFactorsAndSetFrontal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare data.frame for model fitting (factors up front)
}
\description{
Clean up an existing data.frame: make factor-like columns truly factors,
and move all factor columns to the beginning of the data.frame.
}
\usage{
makeFactorsAndSetFrontal(dfr, treatAsFactorWhenDiffValRate=0.05, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe to clean up
}
  \item{treatAsFactorWhenDiffValRate}{
Numerical columns for which the number of different values divided by the number
of values is below this value, are treated as factors in the cleaned up data.frame
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
A \code{data.frame} with the same number of rows, but where the columns may be
reordered and all columns are either numerical or factors. The factors are the
'leftmost' columns
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iriscopy<-iris
iriscopy$factorlike<-rbinom(dim(iriscopy)[1], 1, 0.5)
iriscopy$someletters<-c("a", "b", "c")
makeFactorsAndSetFrontal(iriscopy, treatAsFactorWhenDiffValRate=0.05, verbosity=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~factor }
\keyword{ ~data.frame }
