\name{corWithCat}
\alias{corWithCat}
\alias{corWithoutCat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correlation matrix after turning factors into numericals / skipping factors
}
\description{
Transforms non-numerical columns to numerical (\code{with}) or removes them
(\code{without}) and then calculates the correlation matrix.
}
\usage{
corWithCat(dfr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe on which to calculate the correlation matrix.
}
  \item{\dots}{
Parameters passed on to \code{cor}
}
}
\details{
This does not always make sense, not even for ordered factors. Most useful for
binary factors or 'equally spaced' ordered factors.
}
\value{
The same as \code{cor}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
corWithCat(iris)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~cor }
\keyword{ ~factor }% __ONLY ONE__ keyword per line
