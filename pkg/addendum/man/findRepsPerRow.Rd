\name{findRepsPerRow}
\alias{findRepsPerRow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find rows that \code{newdfr} extends of \code{orgdfr}
}
\description{
For each row in \code{orgdfr}, find the rownames of \code{newdfr} that form the
start of the rowname of that row in \code{orgdfr}.
}
\usage{
findRepsPerRow(newdfr, orgdfr, failIfOrgNotFound=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newdfr}{
object that is supposed to be an 'extension' of \code{orgdfr}, i.e.: some of the
rows of \code{orgdfr} are repeated in \code{newdfr}, potentially with \code{NA}s
replaced by other values.
}
  \item{orgdfr}{
see \code{newdfr}
}
  \item{failIfOrgNotFound}{
If \code{TRUE} (the default), then if one of the rownames in \code{newdfr} cannot
be matched to any of the rownames in \code{orgdfr}, the function fails (\code{stop}).
If \code{FALSE} this event is just logged if it happens.
}
}
\details{
	In fact can also be applied to a non-dataframe. See the code to check the results.
}
\value{
Named vector. The values are the number of repetitions of a row of \code{orgdfr},
the names are the row indices in \code{orgdfr} (note: will have to be converted
to \code{integer} to be usable).
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	takesomerows<-sort(sample(nrow(iris), 50, replace=TRUE))
	iris.ex<-iris[takesomerows, ]
	findRepsPerRow(iris.ex, iris)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rownames }
\keyword{ extend }
