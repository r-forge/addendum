\name{predict.GLoMo}
\alias{predict.GLoMo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.GLoMo(object, nobs = 1, newdata = NULL, forrows = seq(nrow(newdata)), reusabledata = NULL, returnRepeats = FALSE, returnSelectedGlomoRows = FALSE, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{nobs}{
%%     ~~Describe \code{nobs} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{forrows}{
%%     ~~Describe \code{forrows} here~~
}
  \item{reusabledata}{
%%     ~~Describe \code{reusabledata} here~~
}
  \item{returnRepeats}{
%%     ~~Describe \code{returnRepeats} here~~
}
  \item{returnSelectedGlomoRows}{
%%     ~~Describe \code{returnSelectedGlomoRows} here~~
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
function (object, nobs = 1, newdata = NULL, forrows = seq(nrow(newdata)), 
    reusabledata = NULL, returnRepeats = FALSE, returnSelectedGlomoRows = FALSE, 
    verbosity = 0) 
{
    glomo <- object
    if (is.null(newdata) & (!is.null(reusabledata))) {
        warning("You should not provide reusabledata if newdata is NULL. It will be ignored.")
        reusabledata <- NULL
    }
    if (is.null(newdata)) {
        cattif(verbosity > 1, "predict.GLoMo: _fully_ predicting dataset.")
        probs <- glomo$pihat
        howofteniseachglomorowsampled <- as.vector(rmultinom(1, 
            nobs, prob = probs))
        glomorowsforcurrow <- which(howofteniseachglomorowsampled > 
            0)
        howofteniseachglomorowsampled <- howofteniseachglomorowsampled[glomorowsforcurrow]
        glomorowschosen <- rep(glomorowsforcurrow, howofteniseachglomorowsampled)
        retval <- glomo$uniqueFactorCombinationsAndContinuousMeans[glomorowschosen, 
            ]
        firstposofeachglomorowinresult <- cumsum(c(1, howofteniseachglomorowsampled))[-(length(howofteniseachglomorowsampled) + 
            1)]
        lastposofeachglomorowinresult <- cumsum(howofteniseachglomorowsampled)
        for (i in seq_along(glomorowsforcurrow)) {
            cattif(verbosity > 2, "\tpredict.GLoMo: working on glomorow", 
                i, "/", length(glomorowsforcurrow))
            curglomorowi <- glomorowsforcurrow[i]
            howmanysamplesforcurglomorow <- howofteniseachglomorowsampled[i]
            useMu <- unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[curglomorowi, 
                cntcols, drop = TRUE])
            gen <- qrmvnorm(howmanysamplesforcurglomorow, mean = useMu, 
                sigma = glomo$omegahat)
            allposinresforcurglomorow <- seq(from = firstposofeachglomorowinresult[i], 
                to = lastposofeachglomorowinresult[i])
            retval[allposinresforcurglomorow, missingCntColsInDfr] <- gen
        }
        if (returnSelectedGlomoRows) {
            return(list(predicted = retval, glomorowsused = glomorowschosen))
        }
        else {
            return(retval)
        }
    }
    if (any(class(newdata) != class(glomo$uniqueFactorCombinationsAndContinuousMeans))) {
        warning("Unmatching classes found between glomo and dfr. Will try to coerce dfr.")
        if (inherits(glomo$uniqueFactorCombinationsAndContinuousMeans, 
            "data.frame")) {
            cattif(verbosity > 1, "predict.GLoMo: coercing newdata to data.frame.")
            newdata <- as.data.frame(newdata)
        }
        else if (inherits(glomo$uniqueFactorCombinationsAndContinuousMeans, 
            "numdfr")) {
            cattif(verbosity > 1, "predict.GLoMo: coercing newdata to numdfr.")
            newdata <- numdfr(newdata)
        }
    }
    if (length(nobs) != length(forrows)) {
        if (length(nobs) != 1) 
            stop("Unsupported nobs passed along.")
        cattif(verbosity > 0, "predict.GLoMo: readjusting nobs.")
        nobs <- rep(nobs, length(forrows))
    }
    if (is.null(reusabledata) | (!is(reusabledata, "ReusableDataForGLoMoSampling"))) {
        cattif(verbosity > 0, "predict.GLoMo: reusabledata were not (completely) provided so recalculating.")
        reusabledata <- reusableDataForGLoMoSampling(glomo = glomo, 
            dfr = newdata, forrows = forrows, guiddata = reusabledata, 
            verbosity = verbosity - 1)
    }
    levelslist <- .findLevels(newdata)
    predPerRow <- lapply(forrows, function(currowi) {
        howManiethRow <- match(currowi, forrows)
        if ((length(forrows) > 1) & (verbosity > 1)) {
            catt("predict.GLoMo: working on row", howManiethRow, 
                "/", length(forrows))
        }
        currow <- newdata[currowi, ]
        curreusabledata <- reusabledata$perrow[[match(currowi, 
            reusabledata$forrows)]]
        if (sum(is.na(currow)) == 0) 
            return(newdata[currowi, ])
        glomorowsforcurrow <- reusabledata$guiddata$possibleGlomoGuidPerObs[[currowi]]
        howmanysamplesforcurrow <- nobs[howManiethRow]
        if (length(glomorowsforcurrow) == 0) {
            cattif(verbosity > 1, "predict.GLoMo: row has no matching glomorows.")
            warning(paste("predict.GLoMo: Row passed along for which there are no valid predictions. There is no matching combination of categories in the GLoMo object. The rownumber was", 
                currowi, ". Will simply pick random values."))
            retval <- randomFillAndRepeatDataRow(currow = currow, 
                howmanysamplesforcurrow = howmanysamplesforcurrow, 
                levelslist = levelslist, newdata = newdata)
            if (returnSelectedGlomoRows) {
                return(predicted = retval, glomorowsused = character(0))
            }
            else {
                return(retval)
            }
        }
        else if (length(glomorowsforcurrow) == 1) {
            cattif(verbosity > 1, "predict.GLoMo: row has 1 matching glomorow.")
            howofteniseachglomorowsampled <- howmanysamplesforcurrow
        }
        else {
            cattif(verbosity > 1, "predict.GLoMo: row has multiple matching glomorow.")
            probs <- curreusabledata$probs
            cattif(verbosity > 5, "predict.GLoMo: their probabilities are: ", 
                probs)
            cattif(verbosity > 5, "predict.GLoMo: and we need: ", 
                howmanysamplesforcurrow, "samples.")
            howofteniseachglomorowsampled <- as.vector(rmultinom(1, 
                howmanysamplesforcurrow, prob = probs))
        }
        glomorowsforcurrow <- glomorowsforcurrow[howofteniseachglomorowsampled > 
            0]
        howofteniseachglomorowsampled <- howofteniseachglomorowsampled[howofteniseachglomorowsampled > 
            0]
        glomorowschosen <- rep(glomorowsforcurrow, howofteniseachglomorowsampled)
        retval <- glomo$uniqueFactorCombinationsAndContinuousMeans[glomorowschosen, 
            ]
        cntcols <- ncol(newdata)[-glomo$factorCols]
        if (sum(is.na(currow[, cntcols])) == 0) {
            cattif(verbosity > 1, "\tpredict.GLoMo: no missing continuous data!")
            retval[, cntcols] <- currow[rep(1, howmanysamplesforcurrow), 
                ]
        }
        else {
            retval[, curreusabledata$presentCntColsInDfr] <- currow[rep(1, 
                howmanysamplesforcurrow), curreusabledata$presentCntColsInDfr]
            firstposofeachglomorowinresult <- cumsum(c(1, howofteniseachglomorowsampled))[seq_along(howofteniseachglomorowsampled)]
            lastposofeachglomorowinresult <- cumsum(howofteniseachglomorowsampled)
            for (i in seq_along(glomorowsforcurrow)) {
                cattif(verbosity > 2, "\tpredict.GLoMo: working on glomorow", 
                  i, "/", length(glomorowsforcurrow))
                curglomorowi <- glomorowsforcurrow[i]
                howmanysamplesforcurglomorow <- howofteniseachglomorowsampled[i]
                useMu <- unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[curglomorowi, 
                  cntcols, drop = TRUE])
                if (length(curreusabledata$whichCntColNA) != 
                  length(cntcols)) {
                  mu1 <- useMu[curreusabledata$whichCntColNA]
                  mu2 <- useMu[curreusabledata$whichCntColNotNA]
                  useMu <- unlist(mu1 + as.vector(curreusabledata$sigLeft \%*\% 
                    matrix(unlist(curreusabledata$a - mu2), ncol = 1)))
                }
                if (verbosity > 2) 
                  catt("predict.GLoMo: Past parameter calculation. Will now generate conditional normal.")
                gen <- qrmvnorm(howmanysamplesforcurglomorow, 
                  mean = useMu, sigma = curreusabledata$useSigma)
                allposinresforcurglomorow <- seq(from = firstposofeachglomorowinresult[i], 
                  to = lastposofeachglomorowinresult[i])
                retval[allposinresforcurglomorow, missingCntColsInDfr] <- gen
            }
        }
        if (returnSelectedGlomoRows) {
            return(list(predicted = retval, glomorowsused = glomorowschosen))
        }
        else {
            return(retval)
        }
    })
    if (returnSelectedGlomoRows) {
        result <- do.call(rbind, lapply(predPerRow, "[[", "predicted"))
        glomorowsused <- do.call(c, lapply(predPerRow, "[[", 
            "glomorowsused"))
    }
    else {
        result <- do.call(rbind, predPerRow)
    }
    if (!is.null(rownames(predPerRow))) {
        rownames(predPerRow) <- postfixToMakeUnique(rownames(predPerRow))
    }
    if (returnRepeats) {
        numRepPerRow <- sapply(seq_along(forrows), function(curi) {
            currow <- newdata[forrows[curi], ]
            if (sum(is.na(currow)) == 0) 
                return(1)
            else return(nobs[curi])
        })
        names(numRepPerRow) <- as.character(forrows)
        retval <- list(predicted = result, numRepPerRow = numRepPerRow)
        if (returnSelectedGlomoRows) 
            retval$glomorowsused <- glomorowsused
        return(retval)
    }
    if (returnSelectedGlomoRows) {
        return(list(predicted = result, glomorowsused = glomorowsused))
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
