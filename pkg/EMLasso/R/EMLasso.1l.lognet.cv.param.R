#' Collect parameters to EMLasso.1l.lognet.cv into one list
#' 
#' Collect parameters to \code{\link{EMLasso.1l.lognet.cv}} into one list
#' 
#' @param dfr see \code{\link{EMLasso.1l.lognet.cv}}
#' @param resp see \code{\link{EMLasso.1l.lognet.cv}}
#' @param lambda see \code{\link{EMLasso.1l.lognet.cv}}
#' @param nrOfSamplesPerMDRow see \code{\link{EMLasso.1l.lognet.cv}}
#' @param howManyFold see \code{\link{EMLasso.1l.lognet.cv}}
#' @param groupNrPerRow see \code{\link{EMLasso.1l.lognet.cv}}
#' @param maxIt see \code{\link{EMLasso.1l.lognet.cv}}
#' @param minIt see \code{\link{EMLasso.1l.lognet.cv}}
#' @param weightsName see \code{\link{EMLasso.1l.lognet.cv}}
#' @param orgriName see \code{\link{EMLasso.1l.lognet.cv}}
#' @param precalcGuidData see \code{\link{EMLasso.1l.lognet.cv}}
#' @param nrOfSamplesForCriteria see \code{\link{EMLasso.1l.lognet.cv}}
#' @param calculateCriteria see \code{\link{EMLasso.1l.lognet.cv}}
#' @param verbosity see \code{\link{EMLasso.1l.lognet.cv}}
#' @param reuseDebugLevel see \code{\link{EMLasso.1l.lognet.cv}}
#' @param type.measure see \code{\link{EMLasso.1l.lognet.cv}}
#' @param grouped see \code{\link{EMLasso.1l.lognet.cv}}
#' @param maxFracZeroesAllowed see \code{\link{EMLasso.1l.lognet.cv}}
#' @param dfrconvprobs see \code{\link{EMLasso.1l.lognet.cv}}
#' @return object of class "EMLasso.1l.lognet.cv.param" having all these
#' parameters as named members
#' @note created for use in \code{\link{run.parallel}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{run.parallel}}, \code{\link{EMLasso.1l.lognet.cv}}
#' @keywords constructor parameter
#' @examples data(emlcvfit, package="EMLasso")
#' emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' @export
EMLasso.1l.lognet.cv.param<-function(dfr, resp, lambda, nrOfSamplesPerMDRow,
	howManyFold=10, groupNrPerRow=NULL, maxIt=20,
	minIt=10, weightsName="weights", orgriName="orgri", precalcGuidData=TRUE,
	nrOfSamplesForCriteria=nrOfSamplesPerMDRow,
	calculateCriteria=calculateCriteria.EMLasso.1l.lognet, verbosity=0,
	reuseDebugLevel=-1, type.measure="auc", grouped=TRUE,
	maxFracZeroesAllowed=0,
	dfrconvprobs=dfrConversionProbs(dfr, betweenColAndLevel=""))
{
	rv<-list(dfr=dfr, resp=resp, lambda=lambda,
		nrOfSamplesPerMDRow=nrOfSamplesPerMDRow, howManyFold=howManyFold,
		groupNrPerRow=groupNrPerRow, maxIt=maxIt, minIt=minIt, weightsName=weightsName,
		orgriName=orgriName, precalcGuidData=precalcGuidData,
		nrOfSamplesForCriteria=nrOfSamplesForCriteria,
		calculateCriteria=calculateCriteria, verbosity=verbosity,
		reuseDebugLevel=reuseDebugLevel, type.measure=type.measure, grouped=grouped,
		maxFracZeroesAllowed=maxFracZeroesAllowed,
		dfrconvprobs=dfrconvprobs)
	class(rv)<-"EMLasso.1l.lognet.cv.param"
	return(rv)
}

#' @rdname EMLasso.1l.lognet.cv.param
#' 
#' @usage \method{[}{EMLasso.1l.lognet.cv.param}(x, i)
#' @param x \code{EMLasso.1l.lognet.cv.param} object
#' @param i integer indexer
#' @aliases [.EMLasso.1l.lognet.cv.param
#' @method [ EMLasso.1l.lognet.cv.param
#' @seealso \code{\link{cv.1l.emlasso}}, \code{\link{EMLasso.1l.lognet}}
#' @examples aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
#' y<-as.factor(rbinom(150, 1, 0.5))
#' emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' emlcvparm[1]
#' @export
`[.EMLasso.1l.lognet.cv.param`<-function(x, i)
{
	rv<-x
	rv$lambda<-rv$lambda[i]
	return(rv)
}
## @name extract.EMLasso.1l.lognet.cv.param
## @aliases  extract.EMLasso.1l.lognet.cv.param [.EMLasso.1l.lognet.cv.param


#' @rdname EMLasso.1l.lognet.cv.param
#' 
## @param x \code{EMLasso.1l.lognet.cv.param} object
#' @method length EMLasso.1l.lognet.cv.param
#' @examples aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
#' y<-as.factor(rbinom(150, 1, 0.5))
#' emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' length(emlcvparm)
#' @export
length.EMLasso.1l.lognet.cv.param<-function(x)
{
	return(length(x$lambda))
}

