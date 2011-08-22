\name{factorsToDummyVariables.numdfr}
\alias{factorsToDummyVariables.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
factorsToDummyVariables.numdfr(dfr, betweenColAndLevel = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
%%     ~~Describe \code{dfr} here~~
}
  \item{betweenColAndLevel}{
%%     ~~Describe \code{betweenColAndLevel} here~~
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
function (dfr, betweenColAndLevel = "", ...) 
{
    numdfr.debugtxt("factorsToDummyVariables.numdfr")
    nc <- dim(dfr$mat)[2]
    nr <- dim(dfr$mat)[1]
    coln <- colnames(dfr$mat)
    retval <- do.call(cbind, lapply(seq(nc), function(ci) {
        lvls <- dfr$lvls[[ci]]
        if (length(lvls) > 0) {
            lvls <- lvls[-1]
            stretchedcols <- dfr$mat[, rep(ci, length(lvls))]
            comparelvls <- matrix(rep(seq_along(lvls) + 1, each = nr), 
                nrow = nr)
            stretchedcols <- stretchedcols == comparelvls
            mode(stretchedcols) <- "integer"
            if (!is.matrix(stretchedcols)) {
                warning("In factorsToDummyVariables.numdfr: stretchedcols was not a matrix?")
                stretchedcols <- matrix(stretchedcols, nrow = 1)
            }
            colnames(stretchedcols) <- paste(coln[ci], lvls, 
                sep = betweenColAndLevel)
            return(stretchedcols)
        }
        else {
            curcol <- dfr$mat[, ci, drop = FALSE]
            return(curcol)
        }
    }))
    rownames(retval) <- rownames(dfr)
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
