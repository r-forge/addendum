#' Fit EMLasso to a complete dataset, creating a GLoMo for each lambda
#' 
#' Fit EMLasso to a complete dataset, creating a GLoMo for each lambda
#' 
#' @aliases EMLasso EMLasso.glmnet-class EMLasso.glmnet EMLasso.lognet-class EMLasso.lognet EMLasso-class
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param family see \code{\link{glmnet}}
#' @param lambdas see \code{\link{dfrConversionProps}}
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} object that will provide the conversion from imputed
#' 	dataset to one that is ready for fitting the predictor model
#' @param \dots passed on to \code{\link{EMLasso.1l}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param logdir path to folder where logfiles (and results) of each repeat are stored
#' @param saveTempResults if \code{TRUE}, save the results of each (parallelized) 
#' @param postProcessAll function, like \code{\link{postProcessEMLasso}} (its default) and of that signature, to do some extra
#' 	work before returning the result of \code{EMLasso}
#' @return an object of class "EMLasso", "glmnet" and some other classes based on the class of
#' 	the outcome model. The items added to
#' the \code{\link{glmnet}} members are:
#' \item{result }{list of \code{\link{EMLasso1l}} objects per lambda} 
#' \item{params }{\code{\link{EMLasso.1l.param}} parameters passed in} 
#' \item{logdir }{directory where logging/saving occurred} 
#' \item{combinedGLoMo }{\code{\link{GLoMo}} object: the combination (through \code{\link{combineGLoMos}}) of 
#' 	the \code{\link{GLoMo}}s for each lambda (note: this item is added by \code{\link{postProcessEMLasso}}, so
#' 	it may not be present if another \code{postProcessAll} is used)}
#' \item{imputeDs2FitDsProperties}{as passed in, but first fed to \code{\link{imputeDs2FitDsProps}}}
#' \item{family}{as passed in}
#' @note If lambdas is not passed along or is \code{NULL}, a set of lambdas is used
#' 	by utilizing \code{\link{findReasonableLambdaHelper}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l}}
#' @keywords GLoMo EMLasso
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' require(addendum)
#' require(NumDfr)
#' require(GLoMo)
#' require(snowfall)
#' require(EMLasso)
#' sfInit(parallel = FALSE, cpus = 1)
#' sfLibrary(addendum)
#' sfLibrary(NumDfr)
#' sfLibrary(GLoMo)
#' sfLibrary(EMLasso)
#' iris.cpy<-randomNA(iris, n=0.1)
#' iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y, family="binomial",
#' 	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
#' sfStop()
#' @export
EMLasso<-function(ds, out, family=c("gaussian","binomial","poisson","multinomial","cox"), 
	lambdas=smartLambdaRetriever(), imputeDs2FitDsProperties=normalImputationConversion(), 
	..., verbosity=0, logdir="./", saveTempResults=TRUE, postProcessAll=postProcessEMLasso)
{
	this.call = match.call()
	if(is.data.frame(ds))
	{
		catwif(verbosity > 0, "ds was passed in as a data.frame. For performance reasons, it will now be converted to numdfr first!")
		ds<-as.numdfr(ds)
	}
	if(missing(family))
	{
		catw("Dangerous: family was not specified. Trying to find a clever default...")
		if(is.null(dim(out)))
		{
			if(is.logical(out))
			{
				catw("Logical outcome so using family binomial")
				family<-"binomial"
			}
			else if(is.factor(out))
			{
				nlev<-length(levels(out))
				if(nlev==2)
				{
					catw("2-level factor outcome so using family binomial")
					family<-"binomial"
				}
				else if(nlev>2)
				{
					catw("More than 2-level factor outcome so using family multinomial")
					family<-"multinomial"
				}
			}
			else if(length(unique(out))==2)
			{
				catw("Only two different values in outcome so using family binomial")
				family<-"binomial"
			}
			else
			{
				catw("Just using the default family, gaussian")
				family<-match.arg(family)
			}
		}
		else
		{
			catw("Just using the default family, gaussian")
			family<-match.arg(family)
		}
	}
	if(family=="binomial")
	{
		if(! is.factor(out))
		{
			catw("Family is binomial, so converting the outcome to factor.")
			out<-factor(out)
			if(length(levels(out)) != 2)
			{
				stop("The outcome should have exactly 2 levels")
			}
		}
	}
	else if(family=="multinomial")
	{
		if(! is.factor(out))
		{
			catw("Family is binomial, so converting the outcome to factor.")
			out<-factor(out)
		}
	}
	if(any(is.na(out))) stop("No missing values allowed in the outcome for EMLasso.")
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity)
	catwif(verbosity > 0, "Complete the data once ")
	weightsName<-"EMLasso_weights"
	orgriName<-"EMLasso_orgri"
	completedData<-rCatsAndCntInDfr(dfr=ds, weightsName=weightsName,
																	orgriName=orgriName, reweightPerRow=TRUE, verbosity=verbosity-1)
	completedWeights<-completedData[[weightsName]]
	completedOrgri<-completedData[[orgriName]]
	rmColsPos<-match(c(weightsName, orgriName), colnames(completedData))
	completedData<-completedData[,-rmColsPos]
	completedUseOut<-subsetFirstDim(out,completedOrgri)
	
	lambdas<-retrieveLambdas(lambdas, ds=completedData, out=completedUseOut,...)
	
	catwif(verbosity > 0, "Collect parameters for actual run down lambdas.")
	allrelevantparams<-c(list(ds=ds, out=out, lambda=lambdas, 
		imputeDs2FitDsProperties=imputeDs2FitDsProperties, verbosity=verbosity,
		family=family), list(...))
	params<-do.call(EMLasso.1l.param, allrelevantparams) #in fact I should be able to get this from e1l_param
	if(saveTempResults) savedir<-logdir else savedir<-NULL
	catwif(verbosity > 0, "Will now start parallel run..")
	result<-run.parallel(ds=ds, out=out, lambda=lambdas, imputeDs2FitDsProperties=imputeDs2FitDsProperties, 
		verbosity=verbosity, ..., family=family, paramcreationname="EMLasso.1l.param",
		functionname="EMLasso.1l", paramname="e1l_param", logdir=logdir,
		savedir=savedir, postprocessname=NULL, 
		loadLibsIfSfNotRunning=c("Matrix", "glmnet", "addendum", "NumDfr", "GLoMo", "EMLasso"))
	catwif(verbosity > 0, "Done with parallel run, so collecting results and returning.")
	#retval<-list(result=result, lambda=lambdas, params=params, logdir=logdir)
	if(any(sapply(result, class) == "try-error"))
	{
		catw("At least one run failed, so will not attempt to complete listOfLassoFits")
		listOfLassoFits<-NULL
		useBeta<-NULL
	}
	else
	{
		listOfLassoFits<-lapply(result, "[[", "lasso.fit")
		useBeta<-try(do.call(cBind, lapply(listOfLassoFits, "[[", "beta")))
	}
	
	retval<-list(
		call=this.call,
		a0=try(sapply(listOfLassoFits, "[[", "a0")),
		beta=useBeta,
		lambda=lambdas,
		dev.ratio=try(sapply(listOfLassoFits, "[[", "dev.ratio")),
		nulldev=try(sapply(listOfLassoFits, "[[", "nulldev")),
		df=try(sapply(listOfLassoFits, "[[", "df")),
		dim=dim(useBeta),
		nobs=listOfLassoFits[[1]]$nobs,
		npasses=try(sum(sapply(listOfLassoFits, "[[", "npasses"))),
		offset=listOfLassoFits[[1]]$offset,
		jerr=try(sapply(listOfLassoFits, "[[", "jerr")), #this ends the glmnet items
		result=result,
		params=params, 
		logdir=logdir,
		family=family,
		imputeDs2FitDsProperties=imputeDs2FitDsProperties
		)
	resultclass<-class(result[[1]]$lasso.fit)
	class(retval)<-c(paste("EMLasso", resultclass, sep="."), "EMLasso", resultclass)
	catwif(verbosity > 0, "Postprocessing final results and returning.")
	retval<-postProcessAll(predictorModel=result[[1]]$predictorModel, retval=retval, verbosity=verbosity-1)
	return(retval)
}