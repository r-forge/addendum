\name{combineSimilarDfrList}
\alias{combineSimilarDfrList}
\alias{combineSimilarDfrList.prev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine dataframes of the same structure (like rbind)
}
\description{
When you have a list of dataframes of the same format, and you want to combine
them (as rows) into one...
Not pretty, but a lot better than rbind!
The original implementation of this is still given as \code{combineSimilarDfrList.prev}.

There are some conditions on the dataframes for this function to work - see \code{dfrlst}
}
\usage{
combineSimilarDfrList(dfrlst)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfrlst}{
list containing dataframes that have the same structure (i.e. the same columns),
and only numerical or factor variables. \code{NA}s may or may not work (untested),
and even some other modes may be available (but untested).
}
}
\details{
Gives the same result as \code{do.call(dfrlst, rbind)}, only a lot faster.
}
\value{
A dataframe of the same structure of each list item, but now containing all rows
of all the list items.
}
\references{
http://stackoverflow.com/questions/5980240/performance-of-rbind-data-frame
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
ir1<-iris[1:2,]
ir2<-iris[3:4,]
combineSimilarDfrList(list(ir1, ir2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
