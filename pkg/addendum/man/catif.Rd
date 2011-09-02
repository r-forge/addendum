\name{catif}
\alias{catif}
\alias{catt}
\alias{cattif}
\alias{catw}
\alias{catwif}
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
catw(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE, prefix=1)
catwif(cond=TRUE, ..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE, prefix=1)
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
}
  \item{prefix}{
Text to be prefixed before the cat. If it is an integer, the function name of the
function at that position in the call stack is used, followed by a ":"
}}
\details{
\code{catt(if)} perform a (conditional) \code{cat}, but additionally add the current system time and a newline.

Similarly, \code{printif} is a conditional version of \code{print}.

The 'w' version precede each (conditional) cat with the text in prefix, or, if
it is an integer, the name of the calling function at that point in the stack
and a semicolon is used. The default (1) works if no anonymous methods are used. 

If you do use the apply family of functions or similar, with anonymous functions,
you may need to add at least 2 (to prefix) within that anonymous method, to get
the name of the 'actual' calling function. May need some experimenting...
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
