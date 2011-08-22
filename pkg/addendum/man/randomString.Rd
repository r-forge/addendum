\name{randomString}
\alias{randomString}
\alias{randomStrings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate random character string
}
\description{
Generate a random string of characters from a given alphabet and with given
minimum and maximum length
}
\usage{
randomString(maxLength=100, minLength=1, alphabet=c(letters, LETTERS), separator="")
randomStrings(n, maxLength=100, minLength=1, alphabet=c(letters, LETTERS), separator="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maxLength}{
maximum length (in characters) of the generated string. Defaults to 100.
}
  \item{minLength}{
minimum length (in characters) of the generated string. Defaults to 1.
}
  \item{alphabet}{
Chose from this set of 'letters'. Defaults to all letters including capitals.
}
  \item{separator}{
character(s) that occur between all randomly sampled letters from the alphabet. Defaults to "".
}
  \item{n}{
Number of strings to generate.
}
}
\value{
A single character string for \code{randomString} or a vector of \code{n} strings for \code{randomStrings}.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{letters}}
}
\examples{
dnaString<-randomString(maxLength=200, minLength=50, alphabet=c("A", "C", "G", "T"))
fixedRnaString<-randomString(maxLength=20, minLength=20, alphabet=c("A", "C", "G", "U"), separator=";")
randomStrings(10, maxLength=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~random }
\keyword{ ~character }
