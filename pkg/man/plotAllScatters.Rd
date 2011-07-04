\name{plotAllScatters}
\alias{plotAllScatters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot scatterplot (or categorical equivalent) of all columns with a given column
}
\description{
Runs over (all) columns in a data.frame and visually compares them with a given column.
If both columns are numerical, a scatterplot with density is created, if one is categorical,
boxplots per category are drawn, and if both are categorical, a sunflowerplot is drawn.

For the reference column, a barplot or histogram with density is plotted.
}
\usage{
plotAllScatters(comparingColName, dfr, ylab, markCols = c(), onlyMarkCols = FALSE, indicateObs = c(), excludeOrg = FALSE, avoidUnivariateOutliers=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{comparingColName}{
Column(name) you want to compare the other columns to
}
  \item{dfr}{
Data.frame from which columns are taken
}
  \item{ylab}{
y-label for plots
}
  \item{markCols}{
Set of columns you want to accentuate (in green)
}
  \item{onlyMarkCols}{
If TRUE, plot only the comparisons for the columns in markCols
}
  \item{indicateObs}{
rownumber in \code{dfr} of observations you want to indicate (in orange)
}
  \item{excludeOrg}{
if TRUE, don't include the barplot/histogram for \code{comparingColName}
}
  \item{avoidUnivariateOutliers}{
if TRUE, before drawing, the univariate outliers (see \code{\link{outlierIndices}})
are set to \code{NA} so they don't distort the graphs.
}
}
\details{
The screen is divided in parts as squarely as possible (depends on the number of
columns - see \code{\link{squareLikeLayoutForNGraphs}}). This is done with \code{layout},
but afterwards the layout is again reduced to default.

This is very useful as a data exploration step before fitting regression or similar.
}
\value{
The result of the final \code{layout} call.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{smartpairs}}
}
\examples{
plotAllScatters("Sepal.Width", iris, "sepal width", markCols="Petal.Width", indicateObs = 5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~scatterplot }
\keyword{ ~data }
\keyword{ ~exploration }
