\name{qqWithPred}
\alias{qqWithPred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QQ-plot with line
}
\description{
Does a QQ-plot, and adds the \code{qqline} (in red).
}
\usage{
qqWithPred(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data for which the QQ-plot is made.
}
  \item{\dots}{
Extra parameters passed on to \code{qqnorm}.
}
}
\value{
result of call to \code{qqline}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x<-rnorm(100)
qqWithPred(x, col="green")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~QQ-plot }
\keyword{ ~QQ-line }
