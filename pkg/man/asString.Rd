\name{asString}
\alias{asString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert formula to proper display string
}
\description{
converts a formula into a string that can be used e.g. as the title for a plot
}
\usage{
asString(form)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{
Formula to be converted. If this is already a string, it will be returned unaltered
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
aFormStr<-"y~x1+x2"
cat(asString(aFormStr), "\n")
cat(asString(formula(aFormStr)), "\n")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~formula }
