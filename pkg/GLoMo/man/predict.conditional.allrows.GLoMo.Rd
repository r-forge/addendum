\name{predict.conditional.allrows.GLoMo}
\alias{predict.conditional.allrows.GLoMo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.conditional.allrows.GLoMo(glomo, nobs = 1, dfr, forrows = seq(nrow(dfr)), validateFunction = validateFunction.default, guiddata = NULL, otherData = NULL, initialSuccessRateGuess = 0.5, verbosity = 0)
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
  \item{forrows}{
%%     ~~Describe \code{forrows} here~~
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
function (glomo, nobs = 1, dfr, forrows = seq(nrow(dfr)), validateFunction = validateFunction.default, 
    guiddata = NULL, otherData = NULL, initialSuccessRateGuess = 0.5, 
    verbosity = 0) 
{
    if (length(nobs) != length(forrows)) {
        if (length(nobs) != 1) 
            stop("Unsupported nobs passed along.")
        cattif(verbosity > 0, "predict.conditional.allrows.GLoMo: readjusting nobs.")
        nobs <- rep(nobs, length(forrows))
    }
    if (is.null(guiddata) | (!is(guiddata, "GuidData"))) {
        cattif(verbosity > 0, "predict.conditional.allrows.GLoMo: guiddata were not (completely) provided so recalculating.")
        guiddata <- getGuidData(glomo, dfr, guidPerObservation = guiddata)
    }
    predPerRow <- lapply(seq_along(forrows), function(currowi) {
        predict.conditional.GLoMo(glomo = glomo, nobs = nobs[currowi], 
            dfr = dfr, forrow = forrows[currowi], validateFunction = validateFunction, 
            guiddata = guiddata, otherData = otherData, initialSuccessRateGuess = initialSuccessRateGuess, 
            verbosity = verbosity - 1)
    })
    result <- do.call(rbind, lapply(predPerRow, "[[", "predicted"))
    repsPerRow <- sapply(predPerRow, function(resCurRow) {
        nrow(resCurRow$predicted)
    })
    glomorowsused <- do.call(c, lapply(predPerRow, "[[", "glomorowsused"))
    return(list(predicted = result, glomorowsused = glomorowsused, 
        repsperrow = repsPerRow))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
