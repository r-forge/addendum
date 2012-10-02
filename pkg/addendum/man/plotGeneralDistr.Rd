\name{plotGeneralDistr}
\alias{plotGeneralDistr}
\alias{plotNormal}
\alias{plotChiSquare}
\alias{plotT}
\alias{plotF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot density/cumulative for general distributions
}
\description{
Plot density/cumulative for general distributions for a set of up to 2 parameters.
}
\usage{
plotGeneralDistr(DPQfuncs=list(d=dnorm, p=pnorm, q=qnorm), params=list(mean=0, sd=1), minQuant=0.01, maxQuant=0.99, topmult=1.1, numpts=1001, legendPos=rep(c("topleft", "topright", "bottomleft", "bottomright"), length.out=length(params)),main="", cumulative=FALSE, trueMaxY)
plotNormal(means=0, sds=1, main="normals", ...)
plotChiSquare(dfs=1, ncps=0, main="Chisquares", ..., cumulative=FALSE)
plotT(dfs=1, ncps=0, main="Ts", ...)
plotF(df1s=2, df2s=2, main="Fs", ..., cumulative=FALSE, maxQuant=0.70)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DPQfuncs}{
should be a list with 3 items, \code{d}, \code{p} and \code{q}, respectively the density, cumulative 
probability and quantile function for the distribution (e.g. \code{\link{dnorm}}, \code{\link{pnorm}} 
and \code{\link{qnorm}}). Some additional assumptions are made wrt the names of parameters for the 
functions.
}
  \item{params}{
named list. Each item should have the name of a parameter for the functions 
in \code{DPQfuncs}, and should hold a vector holding all values that need 
plotting. For now at most two parameters are allowed.
}
  \item{minQuant, maxQuant}{
Minimum and maximum quantile that will be plotted
}
  \item{topmult}{
The highest value of the densities will be multiplied with this value to obtain
the top of the plot
}
  \item{numpts}{
How many x values must be used to get a smooth plot
}
  \item{legendPos}{
Location where each legend (for the parameters) will be put. This follows the 
order of params.
}
  \item{main}{
Title for the plot
}
  \item{cumulative}{
if \code{TRUE} (not the default), the cumulative probabilities are plotted.
}
  \item{trueMaxY}{
Possibility to explicitly override the maximum value for the plot
}
  \item{means, sds, dfs, ncps, df1s, df2s}{
vectors holding parameter values for the respective distribution functions 
(the matching name in the function is the same, without the ending s)
}
  \item{\dots}{
Passed on to \code{plotGeneralDistr}.
}
}
\details{
You are invited to use \code{plotGeneralDistr} to extend its functionality to
other distributions than that provided.
}
\value{
nothing
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
layout(matrix(1:4, nrow=2))
plotNormal(means=c(0), sds=c(1,2))
plotChiSquare(dfs=c(1,2,3), ncps=0)
plotT(dfs=c(1,2,3), ncps=c(0,1), cumulative=TRUE)
plotF(df1s=c(1,2,3), df2s=c(1,2,3))
layout(1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ distribution }% __ONLY ONE__ keyword per line
