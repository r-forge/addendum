\name{makeTransparent}
\alias{makeTransparent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find transparent equivalent of a given set of colors
}
\description{
Find transparent equivalent of a given set of colors. Ideal for confidence regions around curves.
}
\usage{
makeTransparent(someColor, alpha=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{someColor}{
One or more colors in any obvious format (numeric, name or rgb) as you can use in the typical basic plot commands. 
If you specify more than one, they should be in a vector (not a list)
}
  \item{alpha}{
alpha level (on an integer scale of 0 to 255) for the resulting colors
}
}
\value{
Vector of the same length as \code{someColor}, holding the rgb values for the transparent version of the colors
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
makeTransparent(2)
makeTransparent("green")
makeTransparent(rgb(0.5,0.5,0.5))
makeTransparent(1:4, alpha=50)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
\keyword{ transparent }
