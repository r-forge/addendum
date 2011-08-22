\name{smartpairs}
\alias{smartpairs}
\alias{plotBox}
\alias{plotTwoCats}
\alias{plotTwoCats2}
\alias{smartAxis}
\alias{smartplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize pairwise relations of all columns in data.frame
}
\description{
\code{smartpairs}: Similar to \code{\link{pairs}}, this function plots pairwise relations between all
columns of a data.frame. Like \code{\link{plotAllScatters}}, when both variables are continuous,
this is a scatterplot with loess curve, when one is categorical, boxplots
per category are drawn, otherwise a function for two categorical variables is applied.
}
\usage{
smartpairs(truex, labels, panel = points, ..., lower.panel = panel, upper.panel = panel, diag.panel = NULL, text.panel = textPanel, label.pos = 0.5 + has.diag/3, cex.labels = NULL, font.labels = 1, row1attop = TRUE, gap = 1, localPlot = defaultLocalPlot, localAxis = defaultLocalAxis, twoCatPlot = plotTwoCats2, verbosity = 0, avoidUnivariateOutliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{truex}{
data.frame for which pairwise relations need to be visualized
}
  \item{labels}{
the names of the variables
}
  \item{panel}{
\code{function(x,y,...)} which is used to plot the contents of each panel of the display
}
  \item{\dots}{
arguments to be passed to or from methods.

Also, graphical parameters can be given as can arguments to \code{plot} such as \code{main}. \code{par("oma")}
 will be set appropriately unless specified
}
  \item{lower.panel, upper.panel}{
separate panel functions to be used below and above the diagonal respectively.
}
  \item{diag.panel}{
optional \code{function(x, ...)} to be applied on the diagonals.
}
  \item{text.panel}{
optional \code{function(x, y, labels, cex, font, ...)} to be applied on the diagonals.
}
  \item{label.pos}{
\code{y} position of labels in the text panel.
}
  \item{cex.labels, font.labels}{
graphics parameters for the text panel.
}
  \item{row1attop}{
logical. Should the layout be matrix-like with row 1 at the top, or graph-like with row 1 at the bottom?
}
  \item{gap}{
Distance between subplots, in margin lines.
}
  \item{localPlot}{
function that decides how to plot. Default indirectly refers to \code{smartplot}
}
  \item{localAxis}{
function that decides how to show axes. Default indirectly refers to \code{smartAxis}
}
  \item{twoCatPlot}{
function that is able to plot for two categorical variables. Typically either \code{plotTwoCats} or \code{plotTwoCats}
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{avoidUnivariateOutliers}{
if TRUE, before drawing, the univariate outliers (see \code{\link{outlierIndices}})
are set to \code{NA} so they don't distort the graphs.
}
}
\details{
Either this is plotTwoCats (sunflowerplot) or plotTwoCats2 (colorcoded plot)
or a custom implementation.

The diagonal shows histograms with density estimates.

You can read the details of pairs to see more explanation of the other parameters.

The other functions, plotBox, plotTwoCats, plotTwoCats2,
smartAxis and smartplot are mainly helpers or examples of how to
customize.

Like plotAllScatters, this can best be used as a form of data exploration.
}
\value{
Nothing is returned
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotAllScatters}}
}
\examples{
smartpairs(iris)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~pairs }
\keyword{ ~cetagorical }
