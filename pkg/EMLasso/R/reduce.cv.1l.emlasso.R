#' Reduce memory footprint of cv.1l.emlasso object
#' 
#' Reduce memory footprint of \code{\link{cv.1l.emlasso}} object
#' 
#' @param object \code{\link{cv.1l.emlasso}} object to reduce
#' @param orgdfr original dataset that was used to fit it to
#' @param \dots provided for conformance to S3 \code{\link{reduce}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return An object of class \code{\link{cv.1l.emlasso.reduced}}
#' @method reduce cv.1l.emlasso
#' @note mostly uses class \code{\link{numdfr.rep}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords reduce memory
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlcvfit<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlcvfit<-eml17
#' emlcvfit.red<-reduce(emlcvfit, verbosity=10)
#' @export
reduce.cv.1l.emlasso<-function(object, orgdfr, ..., verbosity=0)
{
	if(is.character(object))
	{
		catwif(verbosity>0, "loading cv.1l.emlasso object from file '", object, "'")
# 		tmpenv<-new.env()
# 		ldd<-load(object, envir=tmpenv)
# 		catwif(verbosity>0, "loaded object:'", ldd, "'")
# 		object<-get(ldd, envir=tmpenv, inherits=FALSE)#[[ldd]]
# 		rm(tmpenv)
		object<-loadSingleObjectFromFile(object, verbosity=verbosity)
	}
	if(missing(orgdfr))
	{
		catwif(verbosity>0, "orgdfr was not passed along")
		orgdfr<-object$actualfits[[1]]$fitinfo$dfr
	}
	dfrConvData<-dfrConversionProbs(orgdfr, "")

	#what do we keep from the crossvalidations?
	#The coefficient history
	catwif(verbosity>0, "Coefficient history")
	allCoefs<-coef(object, foldname="reduce_cv_1l_emlasso_fold",
		itername="reduce_cv_1l_emlasso_iter", onlyEqualZeroPercent=2,
		verbosity=verbosity-1, form="sparse")
	fold<-allCoefs[,"reduce_cv_1l_emlasso_fold"]
	iter<-allCoefs[,"reduce_cv_1l_emlasso_iter"]
	acn<-colnames(allCoefs)
	keepCols<-seq_along(acn)[-match(c("reduce_cv_1l_emlasso_fold", "reduce_cv_1l_emlasso_iter"), acn)]
	allCoefs<-allCoefs[,keepCols]
#	allCoefs$reduce_cv_1l_emlasso_fold<-NULL #hope this works
#	allCoefs$reduce_cv_1l_emlasso_iter<-NULL #hope this works
	#The group division used
	catwif(verbosity>0, "Group division")
	numRows<-nrow(orgdfr)
	numCols<-ncol(orgdfr)
	orgRowsPerFold<-lapply(object$actualfits, function(curfit){seq(numRows)[-curfit$fitinfo$rowsToUseForFit]})
	#Validation Sample
	if(verbosity > 5)
	{
		for(i in seq_along(object$actualfits))
		{
			curpred<-object$actualfits[[i]]$valsample$predicted
			catw("dimensions of", i, "th element of object$actualfits's predicted set:", dim(curpred))
			catw("\tIts class is: ", class(curpred))
		}
	}
	catwif(verbosity>0, "Validation data")
	predicted<-lapply(object$actualfits, function(curfit){reduce(curfit$valsample$predicted, orgdfr)})
	if(verbosity > 5)
	{
		for(i in seq_along(predicted))
		{
			curpred<-predicted[[i]]
			catw("dimensions of", i, "th element of predicted:", dim(curpred))
			catw("\tIts class is: ", class(curpred))
		}
	}
	predicted<-combineSimilarDfrList(predicted)
	if(verbosity > 5)
	{
		catw("dimensions of resulting predicted:", dim(predicted))
	}
	numRepPerRow<-do.call(c, lapply(object$actualfits, function(curfit){curfit$valsample$numRepPerRow}))
	resp<-do.call(c, lapply(object$actualfits, function(curfit){curfit$valsample$resp}))
	predProb<-do.call(c, lapply(object$actualfits, function(curfit){curfit$criteria$predProb}))
	fromfold<-sapply(object$actualfits, function(curfit){sum(curfit$valsample$numRepPerRow)})
	fromfold<-rep(seq_along(fromfold), fromfold)
	valsample<-list(predicted=predicted, numRepPerRow=numRepPerRow, resp=resp,
		predProb=predProb, fromfold=fromfold)
	#fit lasso to complete validation dataset
	catwif(verbosity>0, "Refit lasso")
	wts<-rep(1/numRepPerRow, numRepPerRow)
	lasso.fit<-fit.lognet(predicted, resp, object$lambda, wts,
		verbosity=verbosity-1, dfrConvData=dfrConvData)
	#reduced glomo
	catwif(verbosity>0, "Combine GLoMo")
	#glomo<-do.call(combineGLoMos, c(lapply(object$actualfits, function(curfit){curfit$fitinfo$glomo}), verbosity=verbosity-1))
	glomo<-combineGLoMos(listOfGLoMos=lapply(object$actualfits, function(curfit){curfit$fitinfo$glomo}), verbosity=verbosity-1)
	#crossvalidation results and the rest into retval
	catwif(verbosity>0, "Store results")
	retval<-list(lambda=object$lambda, cvm=object$cvm, cvsd=object$cvsd,
		cvup=object$cvup, cvlo=object$cvlo, name=object$name,
		type.measure=object$type.measure, grouped=object$grouped,
		coefHistory=list(coefs=allCoefs, fold=fold, iter=iter),
		orgRowsPerFold=orgRowsPerFold,
		valsample=valsample, lasso.fit=lasso.fit, glomo=glomo)
	class(retval)<-"cv.1l.emlasso.reduced"
	return(retval)
}