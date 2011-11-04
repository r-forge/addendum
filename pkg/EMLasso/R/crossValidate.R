#' Crossvalidate a model
#' 
#' Crossvalidate a model
#' 
#' @param model model fit
#' @param \dots for flexibility in 'derived' implementation
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @note aids to generalize crossvalidation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords crossvalidate model
#' @export
crossValidate<-function(model, ..., verbosity=0) UseMethod("crossValidate")

#' @rdname crossValidate
#' 
#' @aliases crossValidate.EMLasso.1l.lognet cv.EMLasso.1l.lognet-class cv.EMLasso.1l.lognet
#' @method crossValidate EMLasso.1l.lognet
#' @usage \method{crossValidate}{EMLasso.1l.lognet}(model, ds=model$dfr, out=model$resp, wts, dsconvprobs, needPredict=0, betweenColAndLevel="",..., verbosity=0)
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param wts vector of weights (defaults to equal weights for all rows)
#' @param dsconvprobs see \code{\link{dfrConversionProbs}}
#' @param needPredict If \code{> 0}, the number of rows that is predicted from the \code{GLoMo}
#' 	in \code{model} for rows with missing data in \code{ds}
#' @param betweenColAndLevel see \code{\link{dfrConversionProbs}}
#' @return object of type "cv.EMLasso.1l.lognet":
#' \item{cv.logreg }{list of \code{\link{cv.1l.emlasso.reduced}} objects per lambda} 
#' \item{ds }{as passed in or reduced if predicted}
#' \item{out }{as passed in or extended if predicted}
#' \item{wts }{as passed in or extended if predicted}
#' \item{fromLambda }{as passed in, the lambda that came from the original model}
#' @seealso \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}}
#' @keywords GLoMo EMLasso
#' @export
crossValidate.EMLasso.1l.lognet<-function(model, ds=model$dfr, out=model$resp, 
	wts, dsconvprobs, needPredict=0, betweenColAndLevel="",..., verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProbs(ds, betweenColAndLevel=betweenColAndLevel)
	}
	if(dsconvprobs$betweenColAndLevel != betweenColAndLevel)
	{
		catwif(verbosity>0, "Passed along betweenColAndLevel does not match the one in dsconvprobs. This last one (", dsconvprobs$betweenColAndLevel, ") will be used.")
		betweenColAndLevel<-dsconvprobs$betweenColAndLevel
	}
	useVarIndexes<-as.vector(unlist(predict(model$lasso.fit, type="nonzero")))
	useVarNames<-rownames(model$lasso.fit$beta)[useVarIndexes]
	catwif(verbosity > 0, "For lambda ", model$lambda, ", use variables: ", useVarNames)
	
	if(needPredict>0)
	{
		newdta<-predict(model$glomo, nobs=needPredict, newdata=ds, returnRepeats=TRUE, 
			verbosity=verbosity-1)
		orgds<-ds
		ds<-newdta$predicted
		if(verbosity >5)
		{
			catw("dim of predicted dataset: ", dim(ds))
			catw("repeats (length=", length(newdta$numRepPerRow), "):")
			print(newdta$numRepPerRow)
			catw("length of out=", length(out))
		}
		out<-rep(out, newdta$numRepPerRow)
		wtfct<-rep(1/newdta$numRepPerRow, newdta$numRepPerRow)
		if(missing(wts))
		{
			catwif(verbosity >5, "no wts was passed in")
			wts<-wtfct
		}
		else
		{
			catwif(verbosity >5, "length of wts passed in=", length(wts))
			wts<-rep(wts, newdta$numRepPerRow)*wtfct
		}
	}
	else
	{
		wts<-rep(1, nrow(ds))
	}
	
	logregres<-try(cv.logreg(dfr=ds, resp=out, wts=wts, verbosity=verbosity-1, useCols=useVarNames, 
		dfrConvData=dsconvprobs, ...))
	if(needPredict>0)
	{
		#make the predicted ds object less memory intensive
		ds<-reduce(ds, orgdfr=orgds, repsperrow=newdta$numRepPerRow)
	}
	retval<-list(cv.logreg=logregres, ds=ds, out=out, wts=wts, fromLambda=model$lambda)
	class(retval)<-"cv.EMLasso.1l.lognet"
	return(retval)
}
		 
#' @rdname crossValidate
#' 
#' @aliases crossValidate.EMLasso.lognet cv.EMLasso.lognet-class cv.EMLasso.lognet
#' @method crossValidate EMLasso.lognet
#' @usage \method{crossValidate}{EMLasso.lognet}(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0, betweenColAndLevel="",..., type.measure="auc", keepResultPerLambda=FALSE, verbosity=0)
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param keepResultPerLambda if \code{TRUE} (not the default), the individual results
#' 	from the \code{crossValidate.EMLasso.1l.lognet} are also returned in an extra item
#' 	\code{resultPerLambda}
#' @return object of type "cv.EMLasso.lognet". This is mainly the same as a \code{\link{cv.glmnet}}.
#' The added/altered items are:
#' \item{glmnet.fit }{is now the model passed in, so of class "EMLasso.lognet", besides "glmnet"} 
#' \item{resultPerLambda }{list of "cv.EMLasso.1l.lognet" objects per lambda. Not present if \code{keepResultPerLambda=FALSE}}
#' @seealso \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso
#' @export
crossValidate.EMLasso.lognet<-function(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, 
	wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0, betweenColAndLevel="",..., type.measure="auc", 
	keepResultPerLambda=FALSE, verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProbs(ds, betweenColAndLevel=betweenColAndLevel)
	}
	if(dsconvprobs$betweenColAndLevel != betweenColAndLevel)
	{
		catwif(verbosity>0, "Passed along betweenColAndLevel does not match the one in dsconvprobs. This last one (", dsconvprobs$betweenColAndLevel, ") will be used.")
		betweenColAndLevel<-dsconvprobs$betweenColAndLevel
	}
	partres<-lapply(model$result, crossValidate, ds=ds, out=out, wts=wts, dsconvprobs=dsconvprobs,
		needPredict=needPredict, ..., type.measure=type.measure, verbosity=verbosity-1)
	cvlogreglist<-lapply(partres, "[[", "cv.logreg")
	
	lambda<-model$lambda
	cvm<-as.vector(unlist(try(sapply(cvlogreglist, "[[", "cvm"))))
	cvsd<-as.vector(unlist(try(sapply(cvlogreglist, "[[", "cvsd"))))
	nz<-try(sapply(predict(model, type = "nonzero"), length))
	out<-list(
		lambda=lambda,
		cvm=cvm,
		cvsd=cvsd,
		cvup=try(cvm+cvsd),
		cvlo=try(cvm-cvsd),
		nzero=nz,
		glmnet.fit=model
	)
  lamin<-try(if (type.measure == "auc") 
      getmin(lambda, -cvm, cvsd)
  else getmin(lambda, cvm, cvsd))
  obj = c(out, as.list(lamin))
	if(keepResultPerLambda)
	{
		obj<-c(obj, list(resultPerLambda=partres))
	}
	class(obj)<-c("cv.EMLasso.lognet", "cv.glmnet")
	return(obj)
}