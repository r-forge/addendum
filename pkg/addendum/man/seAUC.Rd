\name{seAUC}
\alias{seAUC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conservative estimate for the standard error of an AUC, given only AUC
}
\description{
Calculate a conservative estimate for the standard error of an AUC, given only AUC
}
\usage{
seAUC(AUCHat, nZero, nOne)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AUCHat}{
estimate for AUC
}
  \item{nZero}{
number of true zeroes
}
  \item{nOne}{
number of true ones
}
}
\value{
an estimate for the standard error
}
\references{
Hanley en McNeil 1982
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
seAUC(0.8, 20, 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~AUC }
\keyword{ ~SE }
