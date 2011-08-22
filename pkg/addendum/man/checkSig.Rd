\name{checkSig}
\alias{checkSig}
\alias{print.checkSig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple testing significance
}
\description{
Given a set of p-values, calculates which ones are still significant in (independent) multiple testing,
using Hochberg-Benjamini (FDR), Bonferroni (FWER) and no correction.
}
\usage{
checkSig(sa, rown, alpha, showSig = TRUE, Name = rown)
\method{print}{checkSig}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sa}{
Dataframe or matrix with one row holding p-values
}
  \item{rown}{
The row in \code{sa} holding the p-values
}
  \item{alpha}{
Significance wanted
}
  \item{showSig}{
If true, the significant values are returned (see value), if not, the insignificant ones are returned.
}
  \item{Name}{
Name that is added to the return value.
}
  \item{x}{
object to be printed
}
  \item{\dots}{
Extra parameters, currently ignored
}
}
\details{
A specialized \code{print} function is provided.
}
\value{
List (of class \code{checkSig}), holding:
  \item{HBPVals }{p-values that were (in)significant wrt FDR}
  \item{BFPVals }{p-values that were (in)significant wrt FWER}
  \item{StdPVals }{p-values that were (in)significant without correction}
  \item{Name }{Parameter}
  \item{Alpha }{Parameter}
}
\author{
Nick Sabbe
}
\note{
The particular form of this function comes from use for comparison in different situations.
The code can easily be adapted for more general usage.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{HB}}
}
\examples{
pvals<-runif(100)
checkSig(sa=matrix(pvals, nrow=1), rown=1, alpha=0.95, showSig=TRUE, Name="test")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~FDR }
\keyword{ ~FWER }
\keyword{ ~Hochberg }
\keyword{ ~Benjamini }
\keyword{ ~Bonferroni }
\keyword{ ~multiple }
\keyword{ ~testing }
