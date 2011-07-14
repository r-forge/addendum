\name{marginalProbPerCat}
\alias{marginalProbPerCat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate marginal level probabilities for each factor
}
\description{
For each factor column in the data.frame, estimate the probability of each of its levels
as the proportion of that level in the column.
}
\usage{
marginalProbPerCat(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
data.frame to run the function on
}
}
\value{
list with a named item for each factor column. Each item holds a named vector with the probabilities of each level.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
marginalProbPerCat(iris)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~factor }
\keyword{ ~level }
\keyword{ ~probability }
