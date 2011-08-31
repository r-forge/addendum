\name{randomFillAndRepeatDataRow}
\alias{randomFillAndRepeatDataRow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill unmatched datarow in GLoMo
}
\description{
Is only called from within \code{\link{predict.GLoMo}} to handle the unlikely 
case of a row that has no matching row(s) in the GLoMo uids.

Note: this really only is unlikely in the setting that I plan to use it in,
i.e.: impute in the original data from a GLoMo fit to it.
}
\usage{
randomFillAndRepeatDataRow(currow, obsneeded, levelslist, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{currow}{
(unmatched) row of data for which to sample
}
  \item{obsneeded}{
how many rows should be 'sampled'
}
  \item{levelslist}{
list holding (possible) levels for all columns in \code{currow}
}
  \item{newdata}{
complete dataset from which currow was one.
}
}
\value{
a \code{\link{data.frame}} or \code{\link{numdfr}} (dependent
	on the \code{dfr} that was used in the original call to \code{\link{GLoMo}}
	that holds the sampled data.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}
\note{
Really only intended as an internal function. Kept public for who knows what.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{predict.GLoMo}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
mdrow<-min(which(apply(iris.md, 1, function(currow){any(is.na(currow))})))
randomFillAndRepeatDataRow(currow=iris.md.nd[mdrow,], obsneeded=10,
	levelslist=allLevels(iris.md.nd), iris.md.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ predict }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line
