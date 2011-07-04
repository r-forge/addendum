\name{getListItemsAsColumn}
\alias{getListItemsAsColumn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a list with per-item subitems of a list
}
\description{
When you have a list where each item has a sub-item with the
given name, this function returns alist with all those subitems}
\usage{
getListItemsAsColumn(lst, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lst}{
original list
}
  \item{name}{
named element of each list item to be retrieved
}
}
\value{
A list of the same length of \code{lst}, holding the subitems.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
alist<-list(a=list(e="a", k=1:3), b=list(l=letters, e="i"), c=list(e="m"))
getListItemsAsColumn(alist, "e")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~list }
\keyword{ ~sublist }
