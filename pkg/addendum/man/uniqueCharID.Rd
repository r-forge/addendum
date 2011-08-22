\name{uniqueCharID}
\alias{uniqueCharID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find unique values in a vector of characters
}
\description{
Provides several (3) implementations of the same thing. They may be the most 
performant in different situations.
}
\usage{
uniqueCharID(x, needSort=FALSE, includeOccurrence=TRUE, impNr=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of characters.
}
  \item{needSort}{
If \code{TRUE}, the result is sorted. Defaults to \code{FALSE}.
}
  \item{includeOccurrence}{
If \code{TRUE}, also returns the index of the first occurrence of the unique
values within the original vector. Defaults to \code{TRUE}.
}
  \item{impNr}{
Implementation number (1 up to 3). Defaults to 1.
}
}
\value{
If \code{includeOccurrence==FALSE}: a character vector. Otherwise a \code{list}
holding that same vector as first item (uniquex) and the indexes of the first
occurrences of every unique string as the second item (firstOccurrence).
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{unique}}
}
\examples{
#ran this testing code to see the difference
poolOfStrings<-randomStrings(n=500,maxLength=400, alphabet=character(1:3), separator=",")
curSize<-1000
reps<-100
tst<-replicate(reps, {
	curSetOfStrings<-sample(poolOfStrings, curSize, replace=TRUE)
	c(imp1=tshort(system.time(uniqueCharID(curSetOfStrings, impNr=1)))[3],
	imp2=tshort(system.time(uniqueCharID(curSetOfStrings, impNr=2)))[3],
	imp3=tshort(system.time(uniqueCharID(curSetOfStrings, impNr=3)))[3])
})
rowMeans(tst)

#This resulted in:
#imp1.elapsed imp2.elapsed imp3.elapsed
#      0.0110       0.0537       0.0542
#->so: implementation 1 is the fastest for this dataset.
#note: this may differ for other settings

#If sorting is not needed (note: Imp2 and Imp1 are then the same):
#imp1.elapsed imp2.elapsed imp3.elapsed
#      0.0008       0.0001       0.0531
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~random }
\keyword{ ~character }
