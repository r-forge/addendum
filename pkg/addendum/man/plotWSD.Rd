\name{plotWSD}
\alias{plotWSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot points and error bars 1 SD wide
}
\description{
Plot points and error bars 1 SD wide.
}
\usage{
plotWSD(x, y, ysd, ylim, erbarwidth=0.01, col="red", erbarcol=makeTransparent(col, alpha=50), ..., add=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, ylim,col}{
similar to \code{\link{plot}} (but the default for \code{col} is "red")
}
  \item{\dots}{
passed on to \code{\link{plot}}
}
  \item{erbarwidth}{
width of the endings of the error bars.
}
  \item{erbarcol}{
Color that will be given to the error bars.
}
  \item{add}{
if \code{TRUE}, the new plot is added to the current existing plot.
}
}
\value{
Nothing
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

\seealso{
\code{\link{plot}}, \code{\link{par}}
}
\examples{
y<-rnorm(100)
ysd<-abs(rnorm(100, sd=0.1))
plotWSD(x=1:100, y=y, ysd=ysd, verbosity=1, legendx="topright")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ SD }
\keyword{ error }
\keyword{ bar }
