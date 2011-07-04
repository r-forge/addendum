\name{randomFillDS}
\alias{randomFillDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simplistic single imputation (univariate multinomial/mean)
}
\description{
Impute a data.frame once. For factors/character, the missing data is drawn from a univariate
multinomial distribution (estimated from the data), and fro the continuous columns,
missing data is the univariate mean (!)
}
\usage{
randomFillDS(ds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
Dataframe to impute
}
}
\value{
A \code{data.frame} where all NA values are replaced with somewhat suitable imputed values
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iriscopy<-iris
missc<-sample(dim(iriscopy)[2], 20, replace=TRUE)
missr<-sample(dim(iriscopy)[1], 20, replace=TRUE)
for(i in 1:20){iriscopy[missr[i], missc[i]]<-NA}
randomFillDS(iriscopy)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~imputation }
\keyword{ ~data.frame }
