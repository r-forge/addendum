#' Collect parameters to EMLasso.1l.lognet into one list
#' 
#' Collect parameters to \code{\link{EMLasso.1l.lognet}} into one list
#' 
#' @param dfr see \code{\link{EMLasso.1l.lognet}}
#' @param resp see \code{\link{EMLasso.1l.lognet}}
#' @param lambda see \code{\link{EMLasso.1l.lognet}}
#' @param nrOfSamplesPerMDRow see \code{\link{EMLasso.1l.lognet}}
#' @param rowsToUseForFit see \code{\link{EMLasso.1l.lognet}}
#' @param minIt see \code{\link{EMLasso.1l.lognet}}
#' @param maxIt see \code{\link{EMLasso.1l.lognet}}
#' @param weightsName see \code{\link{EMLasso.1l.lognet}}
#' @param orgriName see \code{\link{EMLasso.1l.lognet}}
#' @param precalcGuidData see \code{\link{EMLasso.1l.lognet}}
#' @param dfrconvprobs see \code{\link{EMLasso.1l.lognet}}
#' @param verbosity see \code{\link{EMLasso.1l.lognet}}
#' @param reuseDebugLevel see \code{\link{EMLasso.1l.lognet}}
#' @param betweenColAndLevel if \code{dfrconvprobs} was not provided, this separator is used (see \code{\link{dfrConversionProbs}})
#' @return object of class "EMLasso.1l.lognet.param" having all these
#' parameters (except \code{betweenColAndLevel}) as named members
#' @note created for use in \code{\link{run.parallel}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{run.parallel}}, \code{\link{EMLasso.1l.lognet}}
#' @keywords constructor parameter
#' @examples data(emlcvfit, package="EMLasso")
#' emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' @export
EMLasso.1l.lognet.param<-function(dfr, resp, lambda, nrOfSamplesPerMDRow,
	rowsToUseForFit=seq(nrow(dfr)), maxIt=20,
	minIt=10, weightsName="weights", orgriName="orgri", precalcGuidData=FALSE,
	dfrconvprobs,	verbosity=0, reuseDebugLevel=-1, betweenColAndLevel="")
{
	if(missing(dfrconvprobs))
	{
		catwif(verbosity>0, "dfrconvprobs was not passed along. Calculating it now.")
		dfrconvprobs<-dfrConversionProbs(dfr, betweenColAndLevel=betweenColAndLevel)
	}
	rv<-list(dfr=dfr, resp=resp, lambda=lambda,
		nrOfSamplesPerMDRow=nrOfSamplesPerMDRow, rowsToUseForFit=rowsToUseForFit,
		maxIt=maxIt, minIt=minIt, weightsName=weightsName,
		orgriName=orgriName, precalcGuidData=precalcGuidData,
		dfrconvprobs=dfrconvprobs, verbosity=verbosity, reuseDebugLevel=reuseDebugLevel)
	class(rv)<-"EMLasso.1l.lognet.param"
	return(rv)
}

#' @rdname EMLasso.1l.lognet.param
#' 
#' @usage \method{[}{EMLasso.1l.lognet.param}(x, i)
#' @param x \code{EMLasso.1l.lognet.param} object
#' @param i integer indexer
#' @aliases [.EMLasso.1l.lognet.param
#' @method [ EMLasso.1l.lognet.param
#' @seealso \code{\link{EMLasso.1l.lognet}}
#' @examples aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
#' y<-as.factor(rbinom(150, 1, 0.5))
#' emlcvparm<-EMLasso.1l.lognet.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' emlcvparm[1]
#' @export
`[.EMLasso.1l.lognet.param`<-function(x, i)
{
	rv<-x
	rv$lambda<-rv$lambda[i]
	return(rv)
}
## @name extract.EMLasso.1l.lognet.param
## @aliases  extract.EMLasso.1l.lognet.param [.EMLasso.1l.lognet.param


#' @rdname EMLasso.1l.lognet.param
#' 
## @param x \code{EMLasso.1l.lognet.param} object
#' @method length EMLasso.1l.lognet.param
#' @examples aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
#' y<-as.factor(rbinom(150, 1, 0.5))
#' emlcvparm<-EMLasso.1l.lognet.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' length(emlcvparm)
#' @export
length.EMLasso.1l.lognet.param<-function(x)
{
	return(length(x$lambda))
}

