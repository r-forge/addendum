\name{categoricalUniqueIdentifiers}
\alias{categoricalUniqueIdentifiers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
String per observation identifying factor columns
}
\description{
Generate a character string per observation (row in \code{dfr}), by combining
the numerical values of the factor levels for all factor columns
}
\usage{
categoricalUniqueIdentifiers(dfr, separator=",", na.becomes="\\\\d+")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{data.frame} to find identifiers for.
}
  \item{separator}{
character(s) between the values of different factor columns. Defaults to ",".
}
  \item{na.becomes}{
If \code{NA}, nothing special is done to \code{NA}s. Otherwise replaced by this string.
Defaults to \code{"\\d+"}, which is good for \code{\link{grep}}.
}
}
\value{
A character vector as long as \code{nrow(dfr)}.
}
\author{
Nick Sabbe
}
\details{
If you know no \code{NA}s are present in the data, it is better to pass \code{NA}
as na.becomes, because this will work quicker.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{makeFactorsAndSetFrontal}}
}
\examples{
aDfr<-do.call(data.frame,replicate(10, factor(sample(c("a", "b", "c"), 20, replace=TRUE)), simplify=FALSE))
aDfr$oneMoreCol<-runif(20)
categoricalUniqueIdentifiers(aDfr, separator="<->", na.becomes=NA)
mRow<-sample.int(nrow(aDfr), 5, replace=TRUE)
mCol<-sample.int(ncol(aDfr), 5, replace=TRUE)
for(i in seq_along(mRow)) aDfr[mRow[i],mCol[i]]<-NA
categoricalUniqueIdentifiers(aDfr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~factor }
\keyword{ ~unique }
