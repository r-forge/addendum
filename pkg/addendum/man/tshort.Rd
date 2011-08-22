\name{tshort}
\alias{tshort}
\alias{ttxt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
convert the result of system.time to something that can be used
}
\description{
Convert the result of \code{system.time} to something that can be used: a named
vector of user, elapsed an system time.

\code{ttxt} does the same but turns it into a string.
}
\usage{
tshort(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
return value of \code{system.time}
}
}
\value{
named
vector of user, elapsed an system time
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{system.time}}
}
\examples{
st<-system.time({
	for(i in 1:1000){cat(i, "\n")}
})
print(tshort(st))
print(ttxt(st))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~system }
\keyword{ ~time }
\keyword{ ~format }
