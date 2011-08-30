\name{randomFillAndRepeatDataRow}
\alias{randomFillAndRepeatDataRow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
randomFillAndRepeatDataRow(currow, obsneeded, levelslist, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{currow}{
%%     ~~Describe \code{currow} here~~
}
  \item{obsneeded}{
%%     ~~Describe \code{obsneeded} here~~
}
  \item{levelslist}{
%%     ~~Describe \code{levelslist} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
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
function (currow, obsneeded, levelslist, newdata) 
{
    fcols <- which(sapply(levelslist, length) > 0)
    curnacols <- which(is.na(currow))
    retval <- currow[rep(1, obsneeded), ]
    for (curnacol in curnacols) {
        if (curnacol \%in\% fcols) {
            retval[, curnacol] <- sample.int(length(levelslist[[currowi]]), 
                size = obsneeded, replace = TRUE)
        }
        else {
            coldata <- newdata[, curnacol, drop = TRUE]
            uwmean <- mean(coldata, na.rm = TRUE)
            uwsd <- sd(coldata, na.rm = TRUE)
            retval[, curnacol] <- rnorm(obsneeded, mean = uwmean, 
                sd = uwsd)
        }
    }
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
