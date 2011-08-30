\name{rCatsAndCntInDfr}
\alias{rCatsAndCntInDfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rCatsAndCntInDfr(dfr, maxFullNACatCols = 6, howManyIfTooMany = 1000, onlyCategorical = FALSE, weightsName = "weights", orgriName = "orgri", reweightPerRow = FALSE, verbosity = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
%%     ~~Describe \code{dfr} here~~
}
  \item{maxFullNACatCols}{
%%     ~~Describe \code{maxFullNACatCols} here~~
}
  \item{howManyIfTooMany}{
%%     ~~Describe \code{howManyIfTooMany} here~~
}
  \item{onlyCategorical}{
%%     ~~Describe \code{onlyCategorical} here~~
}
  \item{weightsName}{
%%     ~~Describe \code{weightsName} here~~
}
  \item{orgriName}{
%%     ~~Describe \code{orgriName} here~~
}
  \item{reweightPerRow}{
%%     ~~Describe \code{reweightPerRow} here~~
}
  \item{verbosity}{
%%     ~~Describe \code{verbosity} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dfr, maxFullNACatCols = 6, howManyIfTooMany = 1000, 
    onlyCategorical = FALSE, weightsName = "weights", orgriName = "orgri", 
    reweightPerRow = FALSE, verbosity = 0, ...) 
{
    catCols <- findCatColNums(dfr)
    numCols <- ncol(dfr)
    contCols <- (seq(numCols))[-catCols]
    cln <- 0
    for (i in contCols) {
        cln <- cln + 1
        cattif(verbosity > 0, "rCatsAndCntInDfr: continuous column (", 
            i, "):", cln, "/", length(contCols))
        curcol <- dfr[, i, drop = TRUE]
        wherenas <- which(is.na(curcol))
        if (length(wherenas) > 0) {
            colm <- mean(curcol, na.rm = TRUE)
            dfr[wherenas, i] <- colm
        }
    }
    cattif(verbosity > 0, "rCatsAndCntInDfr: categorical columns")
    retval <- rCatsInDfr(dfr = dfr, maxFullNACatCols = maxFullNACatCols, 
        howManyIfTooMany = howManyIfTooMany, onlyCategorical = onlyCategorical, 
        weightsName = weightsName, orgriName = orgriName, reweightPerRow = reweightPerRow, 
        verbosity = verbosity - 1, ...)
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
