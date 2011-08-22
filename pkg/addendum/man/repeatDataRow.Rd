\name{repeatDataRow}
\alias{repeatDataRow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a data.frame consisting of a given row repeated several times
}
\description{
Make a new data.frame with \code{nrOfTimes} repeats of the given row.
}
\usage{
repeatDataRow(dr, nrOfTimes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dr}{
Data.frame holding one row to be repeated
}
  \item{nrOfTimes}{
repeat the row that many times
}
}
\value{
data.frame
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
repeatDataRow(iris[1], 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~data.frame }
\keyword{ ~row }
\keyword{ ~repeat }
