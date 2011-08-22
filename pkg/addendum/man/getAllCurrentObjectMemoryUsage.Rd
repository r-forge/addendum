\name{getAllCurrentObjectMemoryUsage}
\alias{getAllCurrentObjectMemoryUsage}
\alias{memorySize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the size in memory of all objects in the global envrionment
}
\description{
Find the size in memory of all objects in \code{.GlobalEnv}
}
\usage{
getAllCurrentObjectMemoryUsage()
}
%- maybe also 'usage' for other objects documented here.
\details{
\code{memorySize} is just a helper function to find the memory size of a named object.
}
\value{
a data.frame with a row for every object and two columns
  \item{obj }{Name of the object}
  \item{sz }{Size in bytes}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getAllCurrentObjectMemoryUsage()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~memory }
\keyword{ ~object }
