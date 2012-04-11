#' Function to help calculate predicted probability of a lognet fit
#' 
#' Method that provides information that can be reused to predict
#' probabilities from the same lognet several times
#' 
#' @param lognet binomial \code{\link{glmnet}} fit to use for prediction
#' @param ds \code{\link{numdfr}} or \code{\link{data.frame}} that holds the structure
#' from which dataset predictions will need to happen.
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} object that will provide the conversion from imputed
#' 	dataset to one that is ready for fitting the predictor model
#' @param usecol which of the column of beta must be used. Must be provided if \code{lognet}
#' is not the result of a 1-lambda \code{\link{glmnet}} call.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return object of class "lognetProbabilityReusable": list with items:
#' \item{conversionProps }{\code{data.frame} with columns \code{fromCols} (original 
#' 	 column number), \code{newNames} (new column name), \code{mustEqual} (which value
#' 	 must it equal for this level of the factor), \code{useBeta} (coefficient)} 
#' \item{originalLognet }{\code{lognet} that was passed in} 
#' \item{usedcol }{usecol that was passed in (or 1 if only it was missing)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{dfrConversionProps}}
#' @keywords lognet glmnet predict dummy
#' @examples data(iris)
#' 	iris.nd2<-numdfr(iris)
#' 	y2<-rbinom(nrow(iris), 1, 0.5)
#' 	iris.nic2<-normalImputationConversion(
#'		scalingParams=typicalScaleAndCenter(), 
#'		transformParams=typicalTransformations())
#' 	iris.cp2<-imputeDs2FitDsProps(iris.nic2,iris.nd2,verbosity=1)
#' 
#' 	iris.cvtd2<-imputeDs2FitDs(iris.cp2,ds=iris.nd2,verbosity=3)
#'
#' 	lnet<-glmnet(iris.cvtd2, y2, family="binomial")
#' 	lognetProbabilityReusable(lnet, iris.nd2, imputeDs2FitDsProperties=iris.cp2, usecol=5, verbosity=1)
#' @export
lognetProbabilityReusable<-function(lognet, ds, imputeDs2FitDsProperties=normalImputationConversion(), usecol, verbosity=0)
{
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	#mat<-imputeDs2FitDsProps(imputeDs2FitDsProperties,ds[1,,drop=FALSE],verbosity=verbosity-1)
	if(missing(usecol))
	{
		if(ncol(lognet$beta) != 1) stop("lognetProbabilityReusable needs usecol if not 1 lambda lognet")
		usecol<-1
	}
	
	catwif(verbosity > 0, "usecol:", usecol)
	betas<-lognet$beta[,usecol] #apparantly, this is a named vector
	keepVars<-abs(betas)>0
	if(sum(keepVars) < length(betas))
	{
		catwif(verbosity >0, "reducing from ", length(betas), "to", sum(keepVars), "variables/dummies\n",
			"because these are the only ones used in the model for the lambda of interest")
		betas<-betas[keepVars]
		catwif(verbosity >5, "Variables kept are:", rownames(betas))
	}
	
	retval<-list(imputeDs2FitDsProperties = imputeDs2FitDsProperties,
							 betas=betas,
							 a0=lognet$a0[usecol])
	class(retval)<-"lognetProbabilityReusable"
	return(retval)
}