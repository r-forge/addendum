\name{linearPredict}
\alias{linearPredict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict linear form from a subset of coefficients
}
\description{
When you have a named vector of coefficients from some (generalized) linear model,
typically after dummy coding, this method calculates the linear form for each 
observation, given that set of coefficients.
}
\usage{
linearPredict(dfr, coefs, itcname="(Intercept)", dfrconv, betweenColAndLevel="", verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
dataset (\code{\link{data.frame}}) that you want to calculate the linear form for
}
  \item{coefs}{
named vector of coefficients. May also hold an intercept, in which case it's name
should be the one passed on in \verb{itcname}
}
  \item{itcname}{
name within \code{coefs} of the intercept.
}
  \item{dfrconv}{
see \verb{\link{dfrConversionProbs}}. If not passed along, it is recreated through that function.
}
  \item{betweenColAndLevel}{
see \verb{\link{dfrConversionProbs}}.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
named (the \verb{rownames} of the dataset are used) vector of linear forms
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
cfs<-c(-1,2,0.5)
names(cfs)<-c("(Intercept)", "Speciesversicolor", "Sepal.Width")
linearPredict(dfr=iris, coefs=cfs)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ scale }
