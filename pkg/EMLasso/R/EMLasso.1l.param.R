#' Collect parameters to EMLasso.1l into one list
#' 
#' Collect parameters to \code{\link{EMLasso.1l}} into one list
#' 
#' @param ds see \code{\link{EMLasso.1l}}
#' @param out see \code{\link{EMLasso.1l}}
#' @param lambda see \code{\link{EMLasso.1l}}
#' @param nrOfSamplesPerMDRow see \code{\link{EMLasso.1l}}
#' @param rowsToUseForFit see \code{\link{EMLasso.1l}}
#' @param firstTimeCompleter see \code{\link{EMLasso.1l}}
#' @param imputeDs2FitDsProperties see \code{\link{EMLasso.1l}}
#' @param fitPredictor see \code{\link{EMLasso.1l}}
#' @param family see \code{\link{EMLasso.1l}}
#' @param convergenceChecker see \code{\link{EMLasso.1l}}
#' @param postProcess see \code{\link{EMLasso.1l}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param extraLog see \code{\link{EMLasso.1l}}
#' @return object of class "EMLasso.1l.param" having all these
#' parameters as named members
#' @note created for use in \code{\link{run.parallel}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{run.parallel}}, \code{\link{EMLasso.1l}}
#' @keywords constructor parameter
#' @examples aDfr<-generateTypicalIndependentDfr(numCat=10, numCnt=10, numObs=100, catProbs=rep(1/3,3),
#' rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=1)
#' 
#' outlins<- -mean(aDfr$cnt1)+aDfr$cnt1+2*(aDfr$cat1=="b")
#' outprobs<-expit(outlins)
#' y<-factor(sapply(outprobs, function(prob){sample(c("no", "yes"), 1, prob=c(1-prob,prob))}))
#' 
#' aDfr.MD<-randomNA(aDfr, 0.01, verbosity=1)
#' emlcvparm<-EMLasso.1l.param(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' @export
EMLasso.1l.param<-function(ds, out, lambda, nrOfSamplesPerMDRow=10,
	rowsToUseForFit=seq(nrow(ds)), firstTimeCompleter=marginalCompleter(),
	imputeDs2FitDsProperties=normalImputationConversion(),	
	fitPredictor=GLoMo,
	family="binomial", convergenceChecker=convergenceCheckCreator(),
	postProcess=postProcessEMLasso1l, verbosity=0, extraLog=function(...){})
{
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity)
	rv<-list(ds=ds, out=out, lambda=lambda,
		nrOfSamplesPerMDRow=nrOfSamplesPerMDRow, rowsToUseForFit=rowsToUseForFit,
		firstTimeCompleter=firstTimeCompleter, imputeDs2FitDsProperties=imputeDs2FitDsProperties, 
		fitPredictor=fitPredictor, family=family, convergenceChecker=convergenceChecker,
		postProcess=postProcess, verbosity=verbosity, extraLog=extraLog)
	class(rv)<-"EMLasso.1l.param"
	return(rv)
}

#' @rdname EMLasso.1l.param
#' 
#' @usage \method{[}{EMLasso.1l.param}(x, i)
#' @param x \code{EMLasso.1l.param} object
#' @param i integer indexer
#' @aliases [.EMLasso.1l.param
#' @method [ EMLasso.1l.param
#' @seealso \code{\link{EMLasso.1l}}
#' @examples aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
#' y<-as.factor(rbinom(150, 1, 0.5))
#' emlcvparm<-EMLasso.1l.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' emlcvparm[1]
#' @export
`[.EMLasso.1l.param`<-function(x, i)
{
	rv<-x
	rv$lambda<-rv$lambda[i]
	return(rv)
}
## @name extract.EMLasso.1l.param
## @aliases  extract.EMLasso.1l.param [.EMLasso.1l.param


#' @rdname EMLasso.1l.param
#' 
## @param x \code{EMLasso.1l.param} object
#' @method length EMLasso.1l.param
#' @examples aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
#' y<-as.factor(rbinom(150, 1, 0.5))
#' emlcvparm<-EMLasso.1l.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' length(emlcvparm)
#' @export
length.EMLasso.1l.param<-function(x)
{
	return(length(x$lambda))
}

