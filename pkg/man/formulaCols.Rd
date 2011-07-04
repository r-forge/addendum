\name{formulaCols}
\alias{formulaCols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Given a (simple) formula, find the names of the predictors
}
\description{
Find the predictors used in a formula (note: even when then matching coefficient would be zero)
}
\usage{
formulaCols(form)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{
Formula of interest
}
}
\value{
A list containing
  \item{outcol }{Column name of outcome column}
  \item{predcols }{Vector holding the names of predictor columns (covariates)}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
frm<-formula("y~x+z+a")
formulaCols(frm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~formula }
\keyword{ ~split }
