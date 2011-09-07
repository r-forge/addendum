\name{catif}
\alias{catif}
\alias{catt}
\alias{cattif}
\alias{catw}
\alias{catwif}
\alias{printif}
\alias{curfntester}
\alias{curfnfinder}
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
catw(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE, prefix=0)
catwif(cond=TRUE, ..., prefix=0)
printif(cond=TRUE, ...)
curfntester()
curfnfinder(skipframes=0, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1), retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
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
}
  \item{skipframes}{
From the frame within \code{curfnfinder}, do not look at the first \code{skipframes}
frames to find named function calls
}
  \item{cils, defaultCI}{
See \code{\link{clean_cs}}.
}
  \item{retIfNone}{
What does \code{curfnfinder} return if no (unskipped) named functions were found
}
  \item{retStack}{
if \code{TRUE}, all named functions on the stack are returned, separated by "|". Default is \code{FALSE}
}
  \item{extraPrefPerLevel}{
if \code{retStack=FALSE}, for every named function in the stack except the 'lowest' one, this text is added to the front of the prefix.
}
}
\details{
\code{catt(if)} perform a (conditional) \code{cat}, but additionally add the current system time and a newline.

Similarly, \code{printif} is a conditional version of \code{print}.

The 'w' version precede each (conditional) cat with the text in prefix, or, if
it is an integer, the name of the calling function at that point in the stack
and a semicolon is used. The default typically works.

\code{curfntester} is a function that is added to allow for this experimentation:
it shows for each level in `sys.parents` the relevant call, match.call and some
more info.

Finally, if you want to see what it did, set \code{\link{setDebugmodeAddendum}} to
\code{TRUE}.
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
	catwif(debug, "End of testct function")
	return(rv)
}
testct(1:5, TRUE)
testct(1:5, FALSE)
testct2<-function(showTester=TRUE){
	catt("from the top")
	if(showTester) curfntester()
	catt("curfntester result: '", curfnfinder(), "'")
	lapply(1:3, function(i){
		catt("Item", i, "/3, in anonymous call")
		if(showTester) curfntester()
		catt("Let's see if curfnfinder works:")
		catt("curfntester result: '", curfnfinder(), "'")
	})
	invisible()
}
testct2()
testct2(FALSE)
do.call(testct2, list(showTester=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cat }
