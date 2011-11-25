\name{matplotsd}
\alias{matplotsd}
\alias{sdcolinfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
similar to matplot, but adding error bars where SDs are provided
}
\description{
based on the column names of the main parameter, decides which columns represent
the SDs of other columns, then does something similar to matplot and adds error
bars to the variables where SDs were provided. \code{sdcolinfo} provides the 
column information.
}
\usage{
matplotsd(x, y, sdpostfix="sd", type = "p", lty = 1:5, lwd = 1, lend = par("lend"), pch = NULL, col = 1:6, cex = NULL, bg = NA, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, ..., add = FALSE, verbose = getOption("verbose"), erbarcol=makeTransparent(col, alpha=50), skipBaseCols=NULL, onlyBaseCols=NULL, verbosity=0, legendx, legendy, legendcex=1)
sdcolinfo(mat, sdpostfix="sd", rangewithsd=TRUE, skipBaseCols=NULL, onlyBaseCols=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
similar to in \code{\link{matplot}}, but may now contain some columns that hold the SD for other 
columns. For that, the name has to be the same as the 'value' column, postfixed with \code{sdpostfix}
}
  \item{x, type, lty, lwd, lend, pch, col, cex, bg, xlab, ylab, xlim, ylim, \dots, add, verbose}{
see \code{\link{matplot}}
}
  \item{sdpostfix}{
postfix that makes a column a potential SD column.
}
  \item{erbarcol}{
Colors that will be given to the error bars. Should hold 1 value for each value of \code{col}
}
  \item{skipBaseCols}{
Names of columns that do not need to appear in the plot (and if they have a matching SD column,
neither will that)
}
  \item{onlyBaseCols}{
Names of the columns (out of all of them) that you want included in the graph, unless it is \code{NULL},
which means all columns.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{legendx}{
\code{x} parameter to \code{\link{legend}}
}
  \item{legendy}{
\code{y} parameter to \code{\link{legend}}
}
  \item{legendcex}{
\code{cex} parameter to \code{\link{legend}}
}
  \item{mat}{
\code{cex} parameter to \code{\link{legend}}
}
}
\value{
For sdcolinfo, an object of class "", i.e.: list with the following items:
\item{rangeinfo}{\code{\link{data.frame}} with one row per variable that is selected, 
  \code{name}, \code{sdname}, \code{min}, \code{max}, \code{stringsAsFactors}}
\item{colswithsd}{names of columns that have a matching SD column}
\item{colswithoutsd}{names of columns that don't have a matching SD column}
\item{colssd}{names of SD columns for each of the columns named in \code{colswithsd}}
\item{ignoredcols}{columns that are not selected}
\item{usedcols}{columns that are used (non-SD)}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

\seealso{
\code{\link{matplot}}, \code{\link{legend}}
}
\examples{
x1<-rnorm(100)
x1sd<-abs(rnorm(100, sd=0.1))
x2<-rnorm(100)
x2sd<-abs(rnorm(100, sd=0.1))
x3<-rnorm(100)
mat<-cbind(x1=x1, x1sd=x1sd, x2=x2, x2sd=x2sd, x3=x3)
matplotsd(x=1:100, y=mat, verbosity=1, legendx="topright")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ matplot }
\keyword{ SD }
