\name{updateGuidData}
\alias{updateGuidData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateGuidData(oldglomo, newglomo, oldrowsused = seq(nrow(oldglomo$uid)), oldguiddata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oldglomo}{
%%     ~~Describe \code{oldglomo} here~~
}
  \item{newglomo}{
%%     ~~Describe \code{newglomo} here~~
}
  \item{oldrowsused}{
%%     ~~Describe \code{oldrowsused} here~~
}
  \item{oldguiddata}{
%%     ~~Describe \code{oldguiddata} here~~
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
function (oldglomo, newglomo, oldrowsused = seq(nrow(oldglomo$uid)), 
    oldguiddata) 
{
    oldrowsused <- unique(oldrowsused)
    oldtonew <- sapply(oldglomo$uid[oldrowsused], function(curolduid) {
        match(curolduid, newglomo$uid)
    })
    oldguiddata$possibleGlomoGuidPerObs <- lapply(oldguiddata$possibleGlomoGuidPerObs, 
        function(oldrows) {
            oldtonew[match(oldrows, oldrowsused)]
        })
    return(oldguiddata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
