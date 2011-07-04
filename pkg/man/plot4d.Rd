\name{plot4d}
\alias{plot4d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes a plot in 3d and allows for 4th dimension through color
}
\description{
scatterplot in three dimensions (\code{x}, \code{y} and \code{z}) where points are
indicated in a color based on \code{u}.

After creating the plot, it can easily be rotated for data exploration.
}
\usage{
plot4d(x, y, z, u, main = "", xlab = "", ylab = "", zlab = "", ulab = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector holding values in the first dimension
}
  \item{y}{
vector holding values in the second dimension
}
  \item{z}{
vector holding values in the third dimension
}
  \item{u}{
vector holding values in the fourth dimension (color)
}
  \item{main}{
main title for the graph
}
  \item{xlab}{
label for x-axis
}
  \item{ylab}{
label for y-axis
}
  \item{zlab}{
label for z-axis
}
  \item{ulab}{
label for u-axis
}
}
\details{
The values of \code{u} are translated internally to color based on the terrain map.
}
\value{
Nothing is returned
}
\author{
Nick Sabbe
}
\note{
Makes use of \code{rgl} for the plot, so depends on that package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x<-rnorm(100)
y<-runif(100)
z<-rexp(100)
u<-seq(100)
plot4d(x,y,z,u, main="demo", xlab="normal", ylab="uniform", zlab="exponential", ulab="ordinal")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~plot }
\keyword{ ~4d }
