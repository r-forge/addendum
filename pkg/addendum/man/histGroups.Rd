\name{histGroups}
\alias{histGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a histogram in which the groups are stacked next to each other per bin
}
\description{
The standard \verb{\link{hist}} does not allow to make histograms per group. This one does.
}
\usage{
histGroups(vals, grps, bins=10, lbls)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vals}{
vector of values for which you want the histograms.
}
  \item{grps}{
vector of the same length as \verb{vals} that holds the group indicator for each value.
Typically this is a factor. The names of the levels are used for the legend.
}
  \item{bins}{
vector of cutpoints for the bins (including the bottom and top value, so this holds one 
more item than the number of bins). If only a single number is passed along, this is taken to 
be the number of bins within the range defined by \verb{vals}
}
  \item{lbls}{
Optional character vector (or coercable to it) with one item per bin (so normally one item
smaller than \verb{bins}) that holds the description of each bin for the horizontal axis. If 
not passed along, the rounded (up to 0.01) lower bound of each bin is used.
}
}
\value{
return value of \verb{\link{barplot}}.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{hist}}
}
\examples{
\dontrun{histGroups(iris$Sepal.Width, iris$Species)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ scale }
