\name{display}
\alias{display}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display a data.frame
}
\description{
The same as \code{\link{edit}}, but returning invisible.
}
\usage{
display(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
The data.frame to display
}}
\value{
invisible data.frame with potential changes during 'edit' incorporated.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edit}}
}
\examples{
\dontrun{display(iris)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~data.frame }
\keyword{ ~edit }
