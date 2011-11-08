\name{coefs2Formula}
\alias{coefs2Formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert named coefficients to proper display of formula
}
\description{
Convert named coefficients to proper display of formula with rounding
}
\usage{
coefs2Formula(coefs, sigdig=2, itcname="(intercept)", minabs=0.0001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coefs}{
named vector of coefficients
}
  \item{sigdig}{
round (and display) to this many significant digits
}
  \item{itcname}{
name within \code{coefs} of the intercept
}
  \item{minabs}{
only display coefficients where the absolute value is larger than or equal to this value
}
}
\value{
String representation of the formula
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
testing<-runif(5) / c(1,1,100,10,100)
names(testing)<-c("height", "(intercept)", "itc", "sex.male", "age_in_years")
coefs2Formula(testing, sigdig=2, itcname="(intercept)")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ formula }
