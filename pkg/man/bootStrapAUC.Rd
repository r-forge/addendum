\name{bootStrapAUC}
\alias{bootStrapAUC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate AUC by bootstrapping
}
\description{
Calculate AUC by bootstrapping the predicted probabilities.
}
\usage{
bootStrapAUC(pos.scores, neg.scores, bootStrap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos.scores}{
probabilities predicted for TRUE ones
}
  \item{neg.scores}{
probabilities predicted for TRUE ones
}
  \item{bootStrap}{
number of bootstrap repeats
}
}
\value{
single AUC estimate
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
bootStrapAUC(runif(500), runif(200), 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~AUC }
\keyword{ ~bootstrap }
