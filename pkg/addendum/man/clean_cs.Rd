\name{clean_cs}
\alias{clean_cs}
\alias{clean_onecall}
\alias{applyCleanItemMatch}
\alias{typicalCleanItemList}
\alias{applyTransform}
\alias{applyTransform.mapCleanItem}
\alias{matches}
\alias{matches.mapCleanItem}
\alias{mapCleanItem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helper functions for \code{\link{curfnfinder}}
}
\description{
Functions that clean up the items returns by \code{\link{sys.calls}}, typically
for use in \code{\link{curfnfinder}}.
}
\usage{
clean_cs(x, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1))
clean_onecall(xt, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1))
applyCleanItemMatch(towhat, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1))
typicalCleanItemList()
\method{applyTransform}{mapCleanItem}(transf, applyTo)
\method{matches}{mapCleanItem}(pattern, applyTo)
mapCleanItem(pattern, useItem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
result of a call to \code{\link{sys.calls}}, or similarly shaped.
}
  \item{cils}{
list of "mapCleanItem" items
}
  \item{defaultCI}{
"mapCleanItem" object, to use if none of those in \code{cils} apply
}
  \item{xt}{
one item of the return value of \code{\link{sys.calls}()}
}
  \item{towhat}{
apply the cleanup to this item. Typically one item of the return value of
\code{\link{sys.calls}}.
}
  \item{transf}{
Transformation to apply. For \code{applyTransform.mapCleanItem}, this is expected to
be a "mapCleanItem" object (and then, actually its \code{useItem} member is applied),
but the general S3 method is foreseen (without a default).
}
  \item{applyTo}{
To what do you want to match the pattern/apply the transform. For the "mapCleanItem",
this typically is a stringized version of one item of the return value of
\code{\link{sys.calls}()}, and in that case, the 'applying' is really done to the
first subitem.
}
  \item{pattern}{
Pattern to look for. For \code{applyTransform.mapCleanItem}, this is expected to
be a "mapCleanItem" object (and then, actually its \code{pattern} member is applied),
but the general S3 method is foreseen (without a default). For the \code{mapCleanItem}
constructor, this is used as the \code{pattern} member.
}
  \item{useItem}{
Used as the \code{useItem} member of the resulting "mapCleanItem" object.
}
}
\details{
A "mapCleanItem" object has 2 fields, \code{pattern} and \code{useItem}. The first
is a character string holding a pattern that can be used in \code{\link{grep}},
to see if a string matches this pattern. The second is either a number (in that
case the 'transformation' really is taking this subitem of \code{applyTo}), or a
character string (in that case, this string is returned).

To see some intermediate results, set \code{\link{setDebugmodeAddendum}} to
\code{TRUE}.
}
\value{
All functions return character vectors of varying lengths.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{curfnfinder}}
}
\examples{
test1<-function(){
	cs<-sys.calls()
	print(clean_cs(cs))
}
test2<-function(){
	test1()
}
test1()
test2()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ call }
\keyword{ stack }
\keyword{ curfnfinder }
