\name{predict.conditional.GLoMo}
\alias{predict.conditional.GLoMo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.conditional.GLoMo(glomo, nobs = 1, dfr, forrow, validateFunction = validateFunction.default, guiddata = NULL, otherData = NULL, initialSuccessRateGuess = 0.5, verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glomo}{
%%     ~~Describe \code{glomo} here~~
}
  \item{nobs}{
%%     ~~Describe \code{nobs} here~~
}
  \item{dfr}{
%%     ~~Describe \code{dfr} here~~
}
  \item{forrow}{
%%     ~~Describe \code{forrow} here~~
}
  \item{validateFunction}{
%%     ~~Describe \code{validateFunction} here~~
}
  \item{guiddata}{
%%     ~~Describe \code{guiddata} here~~
}
  \item{otherData}{
%%     ~~Describe \code{otherData} here~~
}
  \item{initialSuccessRateGuess}{
%%     ~~Describe \code{initialSuccessRateGuess} here~~
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
function (glomo, nobs = 1, dfr, forrow, validateFunction = validateFunction.default, 
    guiddata = NULL, otherData = NULL, initialSuccessRateGuess = 0.5, 
    verbosity = 0) 
{
    cattif(verbosity > 0, "predict.conditional.GLoMo for row", 
        forrow)
    if (!.hasNA(dfr[forrow, ])) {
        if (is.null(guiddata)) {
            guiddata <- getGuidData(glomo, dfr[forrow, ], guidPerObservation = NULL)
            guidToFind <- guiddata$guidPerObservation[1]
        }
        else {
            guidToFind <- guiddata$guidPerObservation[forrow]
        }
        return(list(dfr[forrow, ], glomorowsused = match(guidToFind, 
            glomo$uid)))
    }
    successes <- 0
    attempts <- 0
    successRateSoFar <- initialSuccessRateGuess
    tryAtATime <- as.integer((nobs - successes)/successRateSoFar)
    if (tryAtATime < (nobs - successes)) 
        tryAtATime <- (nobs - successes)
    if (tryAtATime < 1) 
        tryAtATime <- 1
    reusabledata <- reusableDataForGLoMoSampling(glomo = glomo, 
        dfr = dfr, forrows = forrow, guiddata = guiddata, verbosity = verbosity - 
            1)
    acceptedRows <- NULL
    acceptedGLoMoRowsRows <- NULL
    howManyLoops <- 0
    while (successes < nobs) {
        howManyLoops <- howManyLoops + 1
        cattif(verbosity > 0, "predict.conditional.GLoMo start of loop", 
            howManyLoops)
        cattif(verbosity > 5, "predict.conditional.GLoMo will try", 
            tryAtATime, "unconditional predictions")
        cattif(verbosity > 5, "predict.conditional.GLoMo successes:", 
            successes, "/", attempts, "->", successRateSoFar)
        newAttempts <- predict(glomo, nobs = tryAtATime, newdata = dfr, 
            forrows = forrow, reusabledata = reusabledata, returnRepeats = FALSE, 
            returnSelectedGlomoRows = TRUE, verbosity = verbosity - 
                1)
        newAttemptValidity <- validateFunction(newAttempts, otherData, 
            forrow, verbosity = verbosity - 1)
        newlyAccepted <- length(newAttemptValidity)
        if (newlyAccepted > 0) {
            if (newlyAccepted > nobs - successes) {
                newAttemptValidity <- newAttemptValidity[seq(nobs - 
                  successes)]
                newlyAccepted <- nobs - successes
            }
            successes <- successes + newlyAccepted
            if (is.null(acceptedRows)) {
                acceptedRows <- newAttempts$predicted[newAttemptValidity, 
                  ]
                acceptedGLoMoRowsRows <- newAttempts$glomorowsused[newAttemptValidity]
            }
            else {
                acceptedRows <- rbind(acceptedRows, newAttempts$predicted[newAttemptValidity, 
                  ])
                acceptedGLoMoRowsRows <- c(acceptedGLoMoRowsRows, 
                  newAttempts$glomorowsused[newAttemptValidity])
            }
        }
        attempts <- attempts + tryAtATime
        if (successes == 0) 
            successRateSoFar <- successRateSoFar/2
        else successRateSoFar <- successes/attempts
        tryAtATime <- as.integer((nobs - successes)/successRateSoFar)
        if (tryAtATime < (nobs - successes)) 
            tryAtATime <- (nobs - successes)
        if (tryAtATime < 1) 
            tryAtATime <- 1
    }
    return(list(predicted = acceptedRows, glomorowsused = acceptedGLoMoRowsRows))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
