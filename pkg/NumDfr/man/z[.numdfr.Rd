\name{[.numdfr}
\alias{[.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[.numdfr(x, i, j, returnAsMatrix = drop, drop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{j}{
%%     ~~Describe \code{j} here~~
}
  \item{returnAsMatrix}{
%%     ~~Describe \code{returnAsMatrix} here~~
}
  \item{drop}{
%%     ~~Describe \code{drop} here~~
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
function (x, i, j, returnAsMatrix = drop, drop = FALSE) 
{
    numdfr.debugtxt("[.numdfr")
    numdfr.debugtxt(ifelse(missing(i), "no i", paste("i:", i)))
    numdfr.debugtxt(ifelse(missing(j), "no j", paste("j:", j)))
    numdfr.debugtxt("returnAsMatrix:", returnAsMatrix)
    numdfr.debugtxt("drop:", drop)
    if (returnAsMatrix) {
        return(x$mat[i, j, drop = drop])
    }
    newlvls <- if (missing(j)) 
        x$lvls
    else x$lvls[j]
    newmat <- x$mat[i, j, drop = FALSE]
    retval <- list(mat = newmat, lvls = newlvls)
    class(retval) <- "numdfr"
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
