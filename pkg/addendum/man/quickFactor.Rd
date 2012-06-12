\name{quickFactor}
\alias{quickFactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quickly turn an int vector into a factor
}
\description{
Turns an integer vector into a factor with given levels - without safety checks!!.
}
\usage{
quickFactor(x, labels, ordered=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of int
}
  \item{labels}{
character vector. Elements of x with value 1 will be turned into a level with the first label etc.
}
  \item{ordered}{
logical indicating whether the factor is ordered.
}
}
\details{
Should only be used if you know for certain that x holds integers between 1 and length(labels)
}
\value{
a factor of the same length as x
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
somevals<-sample.int(5, 100, replace=TRUE)
somevalsasfactor<-quickFactor(somevals, letters[seq(max(somevals))])
somevalsasfactor
somevalsasordered<-quickFactor(somevals, letters[seq(max(somevals))], ordered=TRUE)
somevalsasordered
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ factor }
