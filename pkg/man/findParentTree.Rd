\name{findParentTree}
\alias{findParentTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
From a list and a partial order relation, find direct parents
}
\description{
Given a list and an order relation (\code{isAncestorOf}), find the parent-child
relations that have no steps in between.
}
\usage{
findParentTree(x, isAncestorOf = get(">"), returnPosPairs = "indices", verbosity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of items to compare (note: could be a vector too for simplistic cases)
}
  \item{isAncestorOf}{
Closure (function) that compare two items in the list \code{x}
}
  \item{returnPosPairs}{
Defines the form of the result. See details.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\details{
For
}
\value{
Depending on \code{returnPosPairs} being
  \item{"indices" }{matrix with two columns, child and parent, holding
       indices within x}
  \item{"items" }{list where each item is itself a list with members
        child and parent, holding the items themselves}
  \item{otherwise }{list where each item is itself a list with members
         curx (actual child item) and parents (indices of parents within x)}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#create 20 random subsets of 1:20
smpls<-lapply(1:20, function(i){
	howMany<-sample(20,1)
	return(sample(20, howMany))
})
superset<-function(lset, rset)
{
	return(length(match(rset, lset))==length(rset))#every item is matched
}
findParentTree(smpls, superset)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~tree }
