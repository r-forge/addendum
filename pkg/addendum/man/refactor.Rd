\name{refactor}
\alias{refactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Turn numeric vector into factor
}
\description{
Turns a vector that e.g. is the result of as.numerical(someFactor) back into
a factor.
}
\usage{
refactor(x, labels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numerical vector
}
  \item{labels}{
lables (in order of the values in x) for the new factor
}
}
\value{
a factor vector
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
org<-factor(rep(c("a", "b", "c"), each=2))
num<-as.numeric(org)
refactor(num, levels(org))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~factor }
\keyword{ ~level }
