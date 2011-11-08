\name{colsOfType}
\alias{colsOfType}
\alias{colsStartingWith}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find columns of a given kind/starting with in a dataframe
}
\description{
Check all columns of a dataframe whether they are of a given type, or if they start with a given text
}
\usage{
colsOfType(dfr, type = c("factor", "char"))
colsStartingWith(dfr, strt)
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
  \item{strt}{
text with which the columns should start
}
}
\details{
	In fact can also be applied to a non-dataframe. See the code to check the results.
}
\value{
A vector of length the number of columns in the dataframe. Typically, this vector
is logical, but it depends on \code{type}. For \code{colsStartingWith} a character 
vector holding the names of the columns that apply.
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
colsOfType(dfr=iris, type="char")
colsStartingWith(iris, "Sepal")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ column }
\keyword{ type }
