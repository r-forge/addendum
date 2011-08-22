\name{getColorRange}
\alias{getColorRange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform a set of values to a set of colors
}
\description{
For every value of \code{x}, finds the matching color. The range of colors is built up by gradually
in-/decreasing two of the rgb values and keeping one constant.
}
\usage{
getColorRange(x, mini = min(x), maxi = max(x), minFirstColor = 0.5, maxSecondColor = 0.5, constThirdColor = 0, returnAsString = FALSE, orderColors = c("r", "g", "b"), verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Values for which the color must be found
}
  \item{mini}{
value for \code{x} that represents the  'smallest' color
}
  \item{maxi}{
value for \code{x} that represents the  'biggest' color
}
  \item{minFirstColor}{
minimum value for the first (increasing) r/g/b (between 0 and 1)
}
  \item{maxSecondColor}{
maximum value for the second (decreasing) r/g/b (between 0 and 1)
}
  \item{constThirdColor}{
constant value for the constant r/g/b (between 0 and 1)
}
  \item{returnAsString}{
if TRUE, a string representation is returned, otherwise an rgb value
}
  \item{orderColors}{
vector holding \code{"r"}, \code{"g"} and \code{"b"} in the (obvious) order: \code{FirstColor}, \code{SecondColor}, \code{ThirdColor}.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
A vector of the same length as \code{x}, holding either string representations or rgb values for the colors
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getColorRange(c(1,2,6), returnAsString=TRUE, orderColors=c("g", "b", "r"),
	verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~color }
\keyword{ ~range }
