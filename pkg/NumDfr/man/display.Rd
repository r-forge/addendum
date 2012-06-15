\name{display.numdfr}
\alias{display.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display a \code{\link{numdfr}}
}
\description{
display the contents of a \code{\link{numdfr}}.
}
\usage{
\method{display}{numdfr}(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
The \code{\link{numdfr}} to display
}}
\value{
invisible \code{\link{data.frame}} (!) with potential changes during 'edit' incorporated.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{display}}
}
\examples{
iris.nd<-numdfr(iris)
\dontrun{display(iris.nd)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data.frame }
\keyword{ edit }
\keyword{ view }
