\name{removeUnivariateOutliers}
\alias{removeUnivariateOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove outliers from a vector
}
\description{
Either sets all outliers (in the boxplot sense: outside of the whiskers (\code{\link{outlierIndices}})) to NA or
removes them from the vector.
}
\usage{
removeUnivariateOutliers(x, setNA = TRUE, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector from which to remove outliers
}
  \item{setNA}{
if TRUE, outliers are replace with NA (useful e.g. for multivariate plots), otherwise the
items are effectively removed from the vector.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
vector similar to x.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{outlierIndices}}
}
\examples{
x<-c(-10, 1:10, 20)
removeUnivariateOutliers(x, setNA = TRUE, verbosity = 1)
removeUnivariateOutliers(x, setNA = FALSE, verbosity = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~outlier }
\keyword{ ~remove }
