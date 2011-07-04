\name{showCallStackObjects}
\alias{showCallStackObjects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print current call stack
}
\description{
Prints the call stack from where this method is called
}
\usage{
showCallStackObjects()
}
%- maybe also 'usage' for other objects documented here.
\value{
Nothing is returned
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
a<-function(par1)
{
	cat("Hello from a:", par1, "\n")
	showCallStackObjects()
}
b<-function(par1, par2)
{
	cat("Hello from b:", par1, ", ", par2, "\n")
	a(par1)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~call stack }
