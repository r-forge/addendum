\name{GLoMo}
\alias{GLoMo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GLoMo(dfr, weights = rep(1, dim(dfr)[1]), uniqueIdentifiersPerRow = NULL, separator = ",", pooledCov = TRUE, verbosity = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
%%     ~~Describe \code{dfr} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{uniqueIdentifiersPerRow}{
%%     ~~Describe \code{uniqueIdentifiersPerRow} here~~
}
  \item{separator}{
%%     ~~Describe \code{separator} here~~
}
  \item{pooledCov}{
%%     ~~Describe \code{pooledCov} here~~
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
function (dfr, weights = rep(1, dim(dfr)[1]), uniqueIdentifiersPerRow = NULL, 
    separator = ",", pooledCov = TRUE, verbosity = 0, ...) 
{
    stopifnot(!.hasNA(dfr))
    numCols <- ncol(dfr)
    factorCols <- findCatColNums(dfr)
    numCat <- length(factorCols)
    stopifnot(numCols != numCat)
    stopifnot(numCat > 0)
    dfrDim <- dim(dfr)
    cattif(verbosity > 0, "GLoMo: ", class(dfr), "(", dfrDim, 
        "), factorCols=", factorCols)
    nobs <- dfrDim[1]
    numCont <- dfrDim[2] - numCat
    if (is.null(uniqueIdentifiersPerRow)) {
        cattif(verbosity > 0, "GLoMo: uniqueIdentifiersPerRow were not provided so recalculating.")
        require(addendum)
        uniqueIdentifiersPerRow <- categoricalUniqueIdentifiers(dfr, 
            separator = separator, na.becomes = NA)
    }
    cattif(verbosity > 0, "GLoMo: uniqueIdentifiersPerRow str: ", 
        str(uniqueIdentifiersPerRow))
    weights <- weights/sum(weights)
    cattif(verbosity > 0, "GLoMo: Unique uniqueIdentifiersPerRow.")
    uids <- uniqueCharID(uniqueIdentifiersPerRow, needSort = TRUE, 
        includeOccurrence = FALSE, impNr = 1)
    cattif(verbosity > 0, length(uids), "found.")
    firstOccurrenceOfEachUMidInDfr <- match(uids, uniqueIdentifiersPerRow)
    uniqueFactorCombinationsAndContinuousMeans <- dfr[firstOccurrenceOfEachUMidInDfr, 
        ]
    cattif(verbosity > 0, "GLoMo: Find the pihat + means")
    matw <- sparseMatrix(i = match(uniqueIdentifiersPerRow, uids), 
        j = seq_along(uniqueIdentifiersPerRow), x = weights)
    contDataAsMat <- .contDataAsMat(dfr)
    pihat <- rowSums(matw)
    themeans <- as.matrix(matw \%*\% contDataAsMat)
    contCols <- seq(ncol(dfr))[-factorCols]
    contnames <- colnames(dfr)[contCols]
    uniqueFactorCombinationsAndContinuousMeans[, contCols] <- themeans
    if (!pooledCov) 
        stop("GLoMo for non-pooled covariance matrix has not been implemented yet.")
    if (verbosity > 0) 
        catt("Getting centralized covariances")
    omegahat <- cov.wt(contDataAsMat, wt = weights, method = "ML")$cov
    dimnames(omegahat) <- list(rowvar = contnames, colvar = contnames)
    retval <- list(uid = uids, pihat = pihat, omegahat = omegahat, 
        orgdatadim = dim(dfr), uniqueFactorCombinationsAndContinuousMeans = uniqueFactorCombinationsAndContinuousMeans, 
        factorCols = factorCols, guidSeparator = separator)
    class(retval) <- "GLoMo"
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
