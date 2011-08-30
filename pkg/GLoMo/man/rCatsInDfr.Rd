\name{rCatsInDfr}
\alias{rCatsInDfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rCatsInDfr(dfr, maxFullNACatCols = 6, howManyIfTooMany = 1000, onlyCategorical = FALSE, weightsName = "weights", orgriName = "orgri", reweightPerRow = FALSE, verbosity = 0, ...)
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
    dfrl <- dfr[, catCols, drop = FALSE]
    if (onlyCategorical) {
        dfr <- dfrl
        catCols <- seq(length(catCols))
    }
    orgnames <- colnames(dfr)
    cattif(verbosity > 0, "rCatsInDfr: find marginal probabilities")
    probs <- marginalProbPerCat(dfrl)
    toAddCols <- NULL
    if ((!is.null(weightsName)) && (nchar(weightsName) > 0)) {
        toAddCols <- c(toAddCols, weightsName)
    }
    if ((!is.null(orgriName)) && (nchar(orgriName) > 0)) {
        toAddCols <- c(toAddCols, orgriName)
    }
    naLevels <- .findLevels(dfrl)
    dfr <- .quickNumMatrix(dfr)
    cattif(verbosity > 0, "rCatsInDfr: dfr is now a matrix of dimension:", 
        dim(dfr), "and class", class(dfr))
    cattif(verbosity > 0, "rCatsInDfr: while dfrl now has class", 
        class(dfrl), "and dimension:", dim(dfrl))
    naLevelNums <- lapply(naLevels, function(curlvls) {
        seq(length(curlvls))
    })
    cattif(verbosity > 0, "rCatsInDfr: start producing new rows")
    newrows <- lapply(seq(nrow(dfrl)), function(ri) {
        cattif(verbosity > 1, "rCatsInDfr: row", ri, "/", nrow(dfrl))
        currow <- dfrl[ri, , drop = TRUE]
        curnas <- which(is.na(currow))
        if (length(curnas) > maxFullNACatCols) {
            cattif(verbosity > 1, "\trCatsInDfr: too many categoricals missing")
            stopifnot(howManyIfTooMany > 1)
            if (is.null(weightsName) || (nchar(weightsName) == 
                0)) {
                cattif(verbosity > 1, "\t\trCatsInDfr: no need to calculate weights")
                catvals <- sapply(curnas, function(ci) {
                  sample.int(length(naLevels[[ci]]), howManyIfTooMany, 
                    replace = TRUE, prob = probs[[ci]])
                })
            }
            else {
                cattif(verbosity > 1, "\t\trCatsInDfr: needed to calculate weights")
                catAll <- lapply(curnas, function(ci) {
                  rv <- sample.int(length(naLevels[[ci]]), howManyIfTooMany, 
                    replace = TRUE, prob = probs[[ci]])
                  relvpr <- probs[[ci]][rv]
                  return(list(val = rv, pr = relvpr))
                })
                catvals <- sapply(catAll, "[[", "val")
                catMargprobs <- sapply(catAll, "[[", "pr")
                catCombProbs <- unlist(apply(catMargprobs, 1, 
                  prod))
                if (reweightPerRow) 
                  catCombProbs <- catCombProbs/sum(catCombProbs)
            }
        }
        else if (length(curnas) > 0) {
            cattif(verbosity > 1, "\trCatsInDfr: few categoricals missing")
            catvals <- dfr2mat(expand.grid(naLevelNums[curnas]))
            if ((!is.null(weightsName)) && (nchar(weightsName) > 
                0)) {
                cattif(verbosity > 1, "\t\trCatsInDfr: needed to calculate weights")
                catMargprobs <- expand.grid(probs[curnas])
                catCombProbs <- apply(catMargprobs, 1, prod)
            }
        }
        if (length(curnas) > 0) {
            retval <- dfr[rep(ri, length(catCombProbs)), , drop = FALSE]
            retval[, catCols[curnas]] <- catvals
        }
        else {
            cattif(verbosity > 1, "\trCatsInDfr: no categoricals missing")
            catCombProbs <- 1
            retval <- dfr[ri, , drop = FALSE]
        }
        toAdd <- rep(ri, length(catCombProbs))
        if (((!is.null(weightsName)) && (nchar(weightsName) > 
            0)) || ((!is.null(orgriName)) && (nchar(orgriName) > 
            0))) {
            cattif(verbosity > 1, "\trCatsInDfr: need to add either weight or orgri")
            if ((!is.null(weightsName)) && (nchar(weightsName) > 
                0)) {
                cattif(verbosity > 1, "\trCatsInDfr: need to add weight")
                toAdd <- catCombProbs
                if ((!is.null(orgriName)) && (nchar(orgriName) > 
                  0)) {
                  cattif(verbosity > 1, "\trCatsInDfr: need to add orgri too")
                  toAdd <- cbind(toAdd, ri)
                }
            }
            else {
                cattif(verbosity > 1, "\trCatsInDfr: need to add orgri")
                toAdd <- ri
            }
            retval <- cbind(retval, toAdd)
            colnames(retval) <- c(orgnames, toAddCols)
        }
        return(retval)
    })
    cattif(verbosity > 1, "rCatsInDfr: combine the rows")
    resmat <- do.call(rbind, newrows)
    cattif(verbosity > 1, "rCatsInDfr: turn resulting matrix into ", 
        class(dfrl), " again")
    result <- .matBack2OrgClass(dfrl, mat = resmat, catCols = catCols, 
        levelList = naLevels, colnms = c(orgnames, toAddCols), 
        verbosity = verbosity - 1)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
