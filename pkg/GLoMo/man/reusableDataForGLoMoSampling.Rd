\name{reusableDataForGLoMoSampling}
\alias{reusableDataForGLoMoSampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reusableDataForGLoMoSampling(glomo, dfr, forrows = seq(nrow(dfr)), guiddata = NULL, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glomo}{
%%     ~~Describe \code{glomo} here~~
}
  \item{dfr}{
%%     ~~Describe \code{dfr} here~~
}
  \item{forrows}{
%%     ~~Describe \code{forrows} here~~
}
  \item{guiddata}{
%%     ~~Describe \code{guiddata} here~~
}
  \item{verbosity}{
%%     ~~Describe \code{verbosity} here~~
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
function (glomo, dfr, forrows = seq(nrow(dfr)), guiddata = NULL, 
    verbosity = 0) 
{
    if (is.null(guiddata) | (!is(guiddata, "GuidData"))) {
        cattif(verbosity > 0, "reusableDataForGLoMoSampling: guiddata were not (completely) provided so recalculating.")
        guiddata <- getGuidData(glomo, dfr, guidPerObservation = guiddata)
    }
    catcols <- glomo$factorCols
    numCat <- length(catcols)
    cntcols <- seq(ncol(dfr))[-catcols]
    numCont <- length(cntcols)
    perrow <- lapply(forrows, function(currowi) {
        if ((length(forrows) > 1) & (verbosity > 1)) {
            catt("reusableDataForGLoMoSampling: working on row", 
                match(currowi, forrows), "/", length(forrows))
        }
        currow <- dfr[currowi, ]
        glomorowsforcurrow <- guiddata$possibleGlomoGuidPerObs[[currowi]]
        whichCntColNotNA <- which(!is.na(currow[, cntcols, drop = TRUE]))
        whichCntColNA <- (1:numCont)[-whichCntColNotNA]
        presentCntColsInDfr <- cntcols[whichCntColNotNA]
        missingCntColsInDfr <- cntcols[whichCntColNA]
        if (length(whichCntColNotNA) > 0) {
            omega <- glomo$omegahat[whichCntColNotNA, whichCntColNotNA, 
                drop = FALSE]
            invSig22 <- invertSymmetric(omega, careful = FALSE)
            aanwezigeXs <- matrix(currow[, presentCntColsInDfr, 
                drop = TRUE], nrow = 1)
            if (length(whichCntColNA) != numCont) {
                sig11 <- glomo$omegahat[whichCntColNA, whichCntColNA, 
                  drop = FALSE]
                sig12 <- glomo$omegahat[whichCntColNA, whichCntColNotNA, 
                  drop = FALSE]
                a <- currow[, presentCntColsInDfr, drop = TRUE]
                useSigma <- sig11 - sig12 \%*\% invSig22 \%*\% t(sig12)
                sigLeft <- sig12 \%*\% invSig22
            }
            else {
                a <- NULL
                useSigma <- glomo$omegahat
                sigLeft <- NULL
            }
        }
        if (length(glomorowsforcurrow) > 1) {
            if (length(whichCntColNotNA) == 0) {
                deltas <- glomo$pihat[glomorowsforcurrow]
            }
            else {
                deltas <- sapply(glomorowsforcurrow, function(curcatrow) {
                  pi.c <- glomo$pihat[curcatrow]
                  relvMus <- matrix(glomo$uniqueFactorCombinationsAndContinuousMeans[curcatrow, 
                    presentCntColsInDfr, drop = TRUE], ncol = 1)
                  partr <- invSig22 \%*\% relvMus
                  part1 <- aanwezigeXs \%*\% partr
                  part2 <- -1/2 * t(relvMus) \%*\% partr
                  delta.c <- part1 + part2 + log(pi.c)
                  return(delta.c)
                })
                maxd <- max(deltas)
                if (maxd > 100) {
                  subtr <- maxd - 100
                  deltas <- deltas - subtr
                }
                deltas <- exp(deltas)
            }
            deltasum <- sum(deltas)
            probs <- deltas/deltasum
        }
        else {
            probs <- NULL
        }
        retval <- list(a = a, useSigma = useSigma, sigLeft = sigLeft, 
            probs = probs, whichCntColNotNA = whichCntColNotNA, 
            whichCntColNA = whichCntColNA, presentCntColsInDfr = presentCntColsInDfr, 
            missingCntColsInDfr = missingCntColsInDfr)
        class(retval) <- "ReusableDataForGLoMoSamplingForOneRow"
        return(retval)
    })
    retval <- list(guiddata = guiddata, forrows = forrows, perrow = perrow)
    class(retval) <- "ReusableDataForGLoMoSampling"
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
