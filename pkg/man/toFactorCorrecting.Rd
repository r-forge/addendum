\name{toFactorCorrecting}
\alias{toFactorCorrecting}
\alias{toNumericCorrecting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert vector to factor/number with replacement of NA value
}
\description{
Every value equal to \code{crval} is replaced with NA, the rest is turned into factor or numeric.
}
\usage{
toFactorCorrecting(vr,crval="-9")
toNumericCorrecting(vr,crval="-9")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vr}{
vector holding the original values
}
  \item{crval}{
value that is to be considered NA. If \code{NULL} of \code{NA}, none are replaced with NA. Defaults to "-9".
}
  \item{replaceComma}{
if \code{TRUE} (the default), commas are replaced with dots before conversion to numeric.
}
}
\value{
vector of the same length as the original
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iriscopy<-iris
iriscopy$sl<-round(iriscopy$Sepal.Length)
iriscopy$pwi<-gsub(".", ",", as.character(round(iriscopy$Sepal.Width*10)/2), fixed=TRUE)
toFactorCorrecting(iriscopy$sl, 8)
toNumericCorrecting(iriscopy$pwi, replaceComma=TRUE)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~factor }
