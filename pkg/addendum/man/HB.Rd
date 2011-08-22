\name{HB}
\alias{HB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find significant testresults when controlling the FDR at alpha
}
\description{
Given a list of p-values, find the ones that are significant after Benjamini-Hochberg correction for multiple testing.
}
\usage{
HB(p, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
vector of p-values
}
  \item{alpha}{
significance we want to attain
}
}
\value{
A vector holding the indices of the rejected p-values (i.e.: the ones that were still smaller than \code{alpha} after correction).
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{checkSig}}
}
\examples{
pvals<-runif(100)
HB(pvals, alpha=0.05)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~FDR }
\keyword{ ~Hochberg }
\keyword{ ~Benjamini }
\keyword{ ~multiple }
\keyword{ ~testing }
