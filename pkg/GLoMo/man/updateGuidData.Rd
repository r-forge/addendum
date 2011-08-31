\name{updateGuidData}
\alias{updateGuidData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update \code{\link{GuidData}} object from one GLoMo to the next
}
\description{
During repeated fits of GLoMo, typically some 'glomorows' get lost in the process.
This function updates a \code{\link{GuidData}} object from the 'old' GLoMo to the reduced
newer one, so it doesn't have to be fully recalculated (since this implies a
\code{\link{grep}} which could be expensive).
}
\usage{
updateGuidData(oldglomo, newglomo, oldrowsused = seq(nrow(oldglomo$uid)), oldguiddata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oldglomo}{
old \code{\link{GLoMo}} object
}
  \item{newglomo}{
new \code{\link{GLoMo}} object
}
  \item{oldrowsused}{
Which of the rows in \code{oldglomo} were used for sampling from a certain dataset.
}
  \item{oldguiddata}{
\code{\link{GuidData}} object that needs updating to the new
}
}
\details{
It is the \code{possibleGlomoGuidPerObs} of the \code{\link{GuidData}} that is updated
}
\value{
\code{\link{GuidData}} object
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}
\note{
Will have to see whether this really provides a speedup.
Maybe doing the \code{\link{grep}} again with the reduced uids will be faster than this
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{getGuidData}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
iris.guids<-getGuidData(iris.glomo, dfr=iris.md.nd)
emulatedusedrow<-sample(length(iris.glomo$uid), size=2)
iris.guids2<-updateGuidData(oldglomo=iris.glomo, newglomo=iris.glomo,
	oldrowsused = emulatedusedrow, iris.guids)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ unique }
\keyword{ identifier }
\keyword{ uid }
\keyword{ update }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line
