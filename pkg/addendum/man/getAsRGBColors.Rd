\name{getAsRGBColors}
\alias{getAsRGBColors}
\alias{neatColorSet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert colors in any format to their rgb equivalent
}
\description{
Convert colors in any format to their rgb equivalent / Get a set of 25 colors that are
relatively very different.
}
\usage{
getAsRGBColors(clrs=palette(), alpha=TRUE)
neatColorSet(excludergb, sampleN, alpha=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clrs}{
vector of colors in any format (names from \code{\link{colors}}, ordinal
in \code{\link{palette}}, \code{\link{rgb}} values themselves)
}
  \item{alpha}{
Whether or not to include the alpha level (transparancy)
}
  \item{excludergb}{
vector of \code{\link{rgb}} values that need to be excluded if they were present in the list
}
  \item{sampleN}{
if present: if greater than zero, this number of colors is drawn randomly from the set after
exclusion of \code{excludergb}. If to zero, the colors in the set are simply 
shuffled. If smaller than zero, the maximum of the actual number of colors and the absolute 
value of this parameter is sampled.
}
}
\details{
	Most useful for functions that only accept certain kinds of color parameters. Also useful
	for comparing colors.
	
	The set of colors from \code{neatColorSet} is useful for e.g. \code{\link{matplot}} to make
	the different lines reasonably discernable.
}
\value{
A vector of the same length as \code{clrs}. Each item is the return value of a call to \code{\link{rgb}}
}
\author{
Nick sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getAsRGBColors()
getAsRGBColors(c("olivedrab", "plum", "gold"), alpha=FALSE)
getAsRGBColors(2:5, alpha=FALSE)
neatColorSet(excludergb=getAsRGBColors(c("black")))
neatColorSet(sampleN=5)
neatColorSet(sampleN=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
\keyword{ rgb }
