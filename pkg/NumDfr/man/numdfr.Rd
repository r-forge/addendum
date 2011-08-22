\name{numdfr}
\alias{numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
numdfr(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
%%     ~~Describe \code{dfr} here~~
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
function (dfr) 
{
    lvls <- lapply(dfr, function(cc) {
        if (is.factor(cc)) 
            return(levels(cc))
        else return(character(0))
    })
    mat <- matrix(unlist(dfr), ncol = ncol(dfr))
    colnames(mat) <- colnames(dfr)
    rownames(mat) <- rownames(dfr)
    retval <- list(mat = mat, lvls = lvls)
    class(retval) <- "numdfr"
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
