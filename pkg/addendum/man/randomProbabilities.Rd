\name{randomProbabilities}
\alias{randomProbabilities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a random set of multinomial probabilties
}
\description{
Chooses random number of classes for multinomial and generates random probabilities for them.
}
\usage{
randomProbabilities(minn=2, maxn=minn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minn}{
Minimum number of classes. To make sense, this should be at least 2. Defaults to 2.
}
  \item{maxn}{
Maximum number of classes. To make sense, this should be at least \code{minn}. Defaults to \code{minn}.
}
}
\value{
Vector of length between \code{minn} and \code{maxn} (inclusive), with probabilties summing to 1.
}
\author{
Nick Sabbe
}
\details{
Typically used to create a test data.frame or similar. Ideal for \code{\link{randomCategoricalVector}}.

Note also: \code{n} is drawn uniformly between \code{minn} and \code{maxn}.
For some purposes other choices may be better suited.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{randomCategoricalVector}}
}
\examples{
randomProbabilities(5,10)
randomProbabilities(3)
randomProbabilities()
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~probability }
\keyword{ ~random }
