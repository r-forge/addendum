\name{similarSizeGroups}
\alias{similarSizeGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Divide number of objects in a number of similar sized groups
}
\description{
Creates a vector of length \code{nobs} holding integer group number. The group sizes are all equal up to 1.
}
\usage{
similarSizeGroups(ngroups, nobs, rand = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ngroups}{
Number of groups to create
}
  \item{nobs}{
Number of objects to divide into groups
}
  \item{rand}{
if FALSE, the sizes of each group are returned. If TRUE, a vector with group numbers.
}
}
\value{
vector of length \code{ngroups} (\code{rand=FALSE}) or of length \code{nobs} (\code{rand=TRUE})
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
similarSizeGroups(3,10,FALSE)
similarSizeGroups(3,10,TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~group }
\keyword{ ~random }
