\name{rCatsAndCntInDfr}
\alias{rCatsAndCntInDfr}
\alias{rCatsInDfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(Multiply) complete dataset based on marginal properties of each column
}
\description{
(Multiply) complete dataset based on marginal properties of each column
}
\usage{
rCatsAndCntInDfr(dfr, maxFullNACatCols = 6, howManyIfTooMany = 1000, weightsName = "weights", orgriName = "orgri", reweightPerRow = FALSE, verbosity = 0, ...)
rCatsInDfr(dfr, maxFullNACatCols=6, howManyIfTooMany=1000, onlyCategorical=FALSE,	weightsName="weights", orgriName="orgri", reweightPerRow=FALSE, verbosity=0,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{\link{data.frame}} or \code{\link{numdfr}} to complete
}
  \item{maxFullNACatCols, howManyIfTooMany}{
If a row from \code{dfr} contains more than \code{maxFullNACatCols} (default: 6)
\code{NA} values, not all combinations are generated, but a +/- random subset of
size \code{howManyIfTooMany} (default: 1000)
}
  \item{onlyCategorical}{
if \code{TRUE}, only the categorical columns are returned from \code{rCatsInDfr}
}
  \item{weightsName}{
if not \code{NULL}, an extra column (with this name) is added to the return
value, holding a 'weight' so that all rows that originate from the same row of
\code{dfr} have total weight 1 (dependent on \code{reweightPerRow}). Defaults to
"weights"
}
  \item{orgriName}{
if not \code{NULL}, an extra column (with this name) is added to the return
value, holding the rownumber in \code{dfr} that this row originates from.
Defaults to "orgri"
}
  \item{reweightPerRow}{
If weights are returned, then for rows having more than \code{maxFullNACatCols}
\code{NA} values, the weights (that are originally only relative to all possible
combinations) are 'reweighted' so they sum to 1.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Ignored for now
}
}
\details{
The 'random subset' is created by drawing the missing categorical values based
on their marginal probability in \code{dfr}.

The continuous missing data is simply filled out with the mean.
}
\value{
Object of the same class as \code{dfr}. Dependent on \code{onlyCategorical}, it
may only contain the categorical columns. For the rest it mainly has the same
structure as \code{dfr}, though it may contain two extra columns based on
\code{weightsName} and \code{orgriName}.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ random }
\keyword{ NA }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line
