\name{catif}
\alias{catif}
\alias{catt}
\alias{cattif}
\alias{printif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conditional cat with time indication
}
\description{
For debugging purposes: allows to cat only when the condition in the first parameter is true
}
\usage{
catif(cond = TRUE, ...)
catt(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
cattif(cond=TRUE, ...)
printif(cond=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cond}{
Only perform the cat if this condition is true
}
  \item{\dots}{
Other parameters passed to \code{cat(t)}
}
  \item{file}{
See \code{cat}
}
  \item{sep}{
See \code{cat}
}
  \item{fill}{
See \code{cat}
}
  \item{labels}{
See \code{cat}
}
  \item{append}{
See \code{cat}
}}
\details{
\code{catt(if)} perform a (conditional) \code{cat}, but additionally add the current system time and a newline.

Similarly, \code{printif} is a conditional version of \code{print}.
}
\value{
None (invisible NULL).
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cat}}
}
\examples{
testct<-function(x, debug=FALSE){
	cattif(debug, "Start of testct function")
	rv<-sum(x)
	cattif(debug, "End of testct function")
	return(rv)
}
testct(1:5, TRUE)
testct(1:5, FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~cat }
