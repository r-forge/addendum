\name{loadSingleObjectFromFile}
\alias{loadSingleObjectFromFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
loads 1 object from an rda-file and returns it
}
\description{
loads 1 object from an rda-file and returns it, so you don't have to worry about its name.
}
\usage{
loadSingleObjectFromFile(fname, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
The filename (may include path) that holds 1 R object.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
the object in question.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{someObject<-loadSingleObjectFromFile("someSaveObjectFile.rda", verbosity=1)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }
