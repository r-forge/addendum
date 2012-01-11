\name{scaleBack}
\alias{scaleBack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale back coefficients base on the scale attributes in a dataset
}
\description{
When you have attained coefficients in a regression from a scaled dataset (i.e.:
some variables have been changed to have mean zero and unit variance), you can use
this function to rescale the coefficients back so they can be applied to the original
variables instead of their rescaled ones. If you do not pass coefficients, the dataset
is simply scaled back itself.
}
\usage{
scaleBack(coefs, dfr, itcname="(intercept)", verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coefs}{
named vector of coefficients. If not passed along, the dataset itself is scaled back.
}
  \item{dfr}{
dataset (\code{\link{data.frame}}) for which some columns hold the \code{\link{scale}}
attributes ("scaled:center" and "scaled:scale")
}
  \item{itcname}{
name within \code{coefs} of the intercept (note: scaling some variables may influence 
the intercept!!)
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
named vector of coefficients (may include intercept) after scaling them back. If no
\verb{coefs} was passed in, a dataset is returned of the same form as the original
but now with the columns scaled back totheir original format (normally, this should
be exactly the same as the originating dataset)
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{scaleNonFactors}}
}
\examples{
iris.sc<-scaleNonFactors(iris)
iris.sc$y<-runif(nrow(iris))
fit<-lm(y~., data=iris.sc)
orgCoefs<-fit$coefficients
scCoefs<-scaleBack(orgCoefs, iris.sc, itcname="(Intercept)")
iris.bk<-scaleBack(dfr=iris.sc, verbosity=1)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ scale }
