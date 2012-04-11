#' Validate a model (to its original data)
#' 
#' Validate a model (to its original data)
#' 
#' @param model model fit
#' @param \dots for flexibility in 'derived' implementation
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @note aids to generalize validation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords validate model
#' @export
simplyValidate<-function(model, ..., verbosity=0) UseMethod("simplyValidate")

#' @rdname simplyValidate
#' 
#' @aliases simplyValidate.EMLasso1l sv.EMLasso1l-class sv.EMLasso1l
#' @method simplyValidate.EMLasso1l
#' @usage \method{simplyValidate}{EMLasso1l}(model, ds=model$dfr, out=model$resp, wts, dsconvprobs, needPredict=0, betweenColAndLevel="", type.measure="auc", ..., verbosity=0)
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param wts vector of weights (defaults to equal weights for all rows)
#' @param dsconvprobs see \code{\link{dfrConversionProps}}
#' @param needPredict If \code{> 0}, the number of rows that is predicted from the \code{GLoMo}
#' 	in \code{model} for rows with missing data in \code{ds}
#' @param betweenColAndLevel see \code{\link{dfrConversionProps}}
#' @param type.measure see \code{\link{cv.glmnet}} - for now, only "auc"" is supported
#' @return object of type "sv.EMLasso1l":
#' \item{sv.logreg }{\code{\link{cv.glmnet}}-like objects} 
#' \item{ds }{as passed in or reduced if predicted}
#' \item{out }{as passed in or extended if predicted}
#' \item{wts }{as passed in or extended if predicted}
#' \item{fromLambda }{as passed in, the lambda that came from the original model}
#' @seealso \code{\link{EMLasso.1l}}
#' @keywords GLoMo EMLasso
#' @S3method simplyValidate EMLasso1l
simplyValidate.EMLasso1l<-function(model, ds=model$dfr, out=model$resp, 
	wts, dsconvprobs, needPredict=0, betweenColAndLevel="", type.measure="auc", ..., verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProps(ds, betweenColAndLevel=betweenColAndLevel)
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
	if(type.measure != "auc")
	{
		stop("Other measures than auc are not yet supported in simplyValidate.EMLasso1l")
	}
	mat<-factorsToDummyVariables(ds, dfrConvData=dsconvprobs)
	#debug.tmp<<-list(mat=mat, wts=wts, out=out, ds=ds)
	#nicked these lines from cv.lognet, simplified for 1 "fold" and 1 lambda
	predmat<-matrix(predict(model$lasso.fit, mat, type = "response"), ncol=1)
	#debug.tmp$predmat<<-predmat
	y<-out #to be able to copy code from cv.lognet
  nc<-dim(y)
  if (is.null(nc)) {
      y = as.factor(y)
      ntab = table(y)
      nc = as.integer(length(ntab))
      y = diag(nc)[as.numeric(y), ]
  }
	#debug.tmp$y<<-y
	cvraw<-matrix(auc.mat(y, predmat[, 1, drop=FALSE], wts))
	#debug.tmp$cvraw<<-cvraw
	#good<-matrix(1, 1, 1)
	N<-1 #?? I think this is the right translation of the if (type.measure == "auc") part in cv.lognet
	weights<-sum(wts) #yikes!

  cvm<-apply(cvraw, 2, weighted.mean, w = weights, na.rm = TRUE)
  cvsd<-sqrt(apply(scale(cvraw, cvm, FALSE)^2, 2, weighted.mean, 
      w = weights, na.rm = TRUE)/(N - 1)) #oops, this will surely fail...
  typenames<-c(mse = "Mean-Squared Error", mae = "Mean Absolute Error", 
      deviance = "Binomial Deviance", auc = "AUC", class = "Misclassification Error")
  name<-typenames[type.measure]

	result<-list(lambda=model$lambda, cvm=cvm, cvsd=cvsd, cvup=cvm+cvsd, cvlo=cvm-cvsd,
		nzero=length(useVarIndexes), name=name, glmnet.fit=model, lambda.min=model$lambda, 
		lambda.1se=model$lambda)
	class(result)<-c("cv.glmnet", "cv.lognet")
 	if(needPredict>0)
 	{
 		#make the predicted ds object less memory intensive
 		ds<-reduce(ds, orgdfr=orgds, repsperrow=newdta$numRepPerRow)
 	}
	retval<-list(sv.logreg=result, ds=ds, out=out, wts=wts, fromLambda=model$lambda)
	class(retval)<-"sv.EMLasso1l"
	return(retval)
}

		 
#' @rdname simplyValidate
#' 
#' @aliases simplyValidate.EMLasso sv.EMLasso-class sv.EMLasso
#' @method simplyValidate.EMLasso
#' @usage \method{simplyValidate}{EMLasso}(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0, betweenColAndLevel="",..., type.measure="auc", keepResultPerLambda=FALSE, verbosity=0)
#' @param keepResultPerLambda if \code{TRUE} (not the default), the individual results
#' 	from the \code{simplyValidate.EMLasso1l} are also returned in an extra item
#' 	\code{resultPerLambda}
#' @return object of type "sv.EMLasso". This is mainly the same as a \code{\link{cv.glmnet}}.
#' The added/altered items are:
#' \item{glmnet.fit }{is now the model passed in, so of class "EMLasso", besides "glmnet"} 
#' \item{resultPerLambda }{list of "sv.EMLasso1l" objects per lambda. Not present if \code{keepResultPerLambda=FALSE}}
#' @seealso \code{\link{EMLasso.1l}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso
#' @S3method simplyValidate EMLasso
simplyValidate.EMLasso<-function(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, 
	wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0, betweenColAndLevel="",..., type.measure="auc", 
	keepResultPerLambda=FALSE, verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProps(ds, betweenColAndLevel=betweenColAndLevel)
	}
	if(dsconvprobs$betweenColAndLevel != betweenColAndLevel)
	{
		catwif(verbosity>0, "Passed along betweenColAndLevel does not match the one in dsconvprobs. This last one (", dsconvprobs$betweenColAndLevel, ") will be used.")
		betweenColAndLevel<-dsconvprobs$betweenColAndLevel
	}
	partres<-lapply(model$result, simplyValidate, ds=ds, out=out, wts=wts, dsconvprobs=dsconvprobs,
		needPredict=needPredict, ..., type.measure=type.measure, verbosity=verbosity-1)
	cvlogreglist<-lapply(partres, "[[", "sv.logreg")
	
	lambda<-model$lambda
	cvm<-as.vector(unlist(try(sapply(cvlogreglist, "[[", "cvm"))))
	#cvsd<-as.vector(unlist(try(sapply(cvlogreglist, "[[", "cvsd")))) these are all NaN!
	#so we use the variability over the cvm
	cvsd<-rep(sd(cvm), length(cvm))
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
	class(obj)<-c("sv.EMLasso", "cv.glmnet")
	return(obj)
}