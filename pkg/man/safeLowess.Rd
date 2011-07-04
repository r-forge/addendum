\name{safeLowess}
\alias{safeLowess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
lowess wrapper that handles missing values etc. (by not including them)
}
\description{
First drops all potentially problematic values from the data, then fits a \code{link{lowess}}.
}
\usage{
safeLowess(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x-variable
}
  \item{y}{
y-variable
}
  \item{\dots}{
Extra parameters passed on to \code{link{lowess}}
}
}
\value{
see \code{link{lowess}}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{link{lowess}}
}
\examples{
x<-seq(100)
x[10]<-NA
x[50]<-1/0
y<-runif(100)
y[10]<-sqrt(-1)
y[70]<-NA
plot(safeLowess(x,y), type="l", col = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~lowess }
\keyword{ ~NA }
