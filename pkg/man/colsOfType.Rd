\name{colsOfType}
\alias{colsOfType}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find columns of a given kind in a dataframe
}
\description{
Check all columns of a dataframe whether they are of a given type
}
\usage{
colsOfType(dfr, type = c("factor", "char"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe to be investigated
}
  \item{type}{
What is checked for each column. Can be predefined \code{"char"}, \code{"factor"}. If it is a
function, this function is applied to each column. If it is none of these, the
number of \code{NA}s in each column is counted.
}
}
\details{
	In fact can also be applied to a non-dataframe. See the code to check the results.
}
\value{
A vector of length the number of columns in the dataframe. Typically, this vector
is logical, bu it depends on \code{type}.
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
colsOfType(dfr=iris, type="char")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~col }
\keyword{ ~type }
