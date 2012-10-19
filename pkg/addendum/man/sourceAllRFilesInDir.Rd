\name{sourceAllRFilesInDir}
\alias{sourceAllRFilesInDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Source all R files in a given directory
}
\description{
Source all R files in a given directory
}
\usage{
sourceAllRFilesInDir(dir="./", fileForm=".*\\\\.r", verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
Directory holding the R files (defaults to current work directory)
}
  \item{fileForm}{
Optional way of specifying a mask for the R files. Regular expression (see: \code{\link{list.files}}) (defaults to all R files).
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\details{
Sources in the global environment.
}
\value{none}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ source }
