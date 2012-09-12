#' Get EMLasso from the save files in a folder
#' 
#' Get EMLasso from the save files in a folder (like what is saved during \code{\link{EMLasso}})
#' 
#' @aliases EMLassoFromDir
#' @usage EMLassoFromDir(dir, fileForm="EMLasso\\\\.1l_parallel_([0-9]+)\\\\.saved", toNrForm="\\\\1", postProcessAll = postProcessEMLasso, family="unidentifiable", params="unidentifiable", verbosity=0)
#' @param dir path where the files are
#' @param fileForm Regular expression showing the form of the filenames (should also hold a group to find the index of that file)
#' @param toNrForm replacement regular expression: combined with \code{fileForm} should turn the name into its index (see \code{\link{sub}})
#' @param postProcessAll function, like \code{\link{postProcessEMLasso}} (its default) and of that signature, to do some extra
#' 	work before returning the result of \code{\link{EMLasso}}
#' @param family see \code{\link{EMLasso}}, but it is overridden
#' @param params see \code{\link{EMLasso}} object that will provide the conversion from imputed
#' 	dataset to one that is ready for fitting the predictor model
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return very similar to the return value of \code{\link{EMLasso}}, though some (mostly unimportant) variables may be 
#' 	impossible to reconstruct, especially the \code{call} and \code{params}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso}}
#' @keywords GLoMo EMLasso
#' @export
EMLassoFromDir<-function(dir, fileForm="EMLasso\\.1l_parallel_([0-9]+)\\.saved", toNrForm="\\1", 
												 postProcessAll = postProcessEMLasso, 
												 family="unidentifiable",
												 params="unidentifiable",
												 verbosity=0)
{
	this.call = match.call()
	
	fls<-list.files(dir, fileForm)
	nrs<-as.integer(sub(fileForm, toNrForm, fls))
	fls<-fls[order(nrs)]
	nrs<-nrs[order(nrs)]
	result<-lapply(fls, function(flnm){
		fullflnm<-paste(dir, flnm, sep="")
		curOb<-loadSingleObjectFromFile(fullflnm, verbosity=verbosity-1)
		catwif(verbosity>1, "Loaded object from file '", flnm, "' of class: ", class(curOb))
		return(curOb)
	})
	catwif(verbosity > 0, "Collecting results and returning.")
	if (any(sapply(result, class) == "try-error")) {
		catw("At least one run failed, so will not attempt to complete listOfLassoFits")
		listOfLassoFits <- NULL
		useBeta <- NULL
	}
	else {
		listOfLassoFits <- try(lapply(result, "[[", "lasso.fit"))
		useBeta <- try(do.call(cBind, lapply(listOfLassoFits, "[[", "beta")))
		lambdas <- try(sapply(listOfLassoFits, "[[", "lambda"))
	}
	catwif(verbosity > 0, "got listOfLassoFits.")
	resultclass <- class(result[[1]]$lasso.fit)
	
	foundClass<-match(resultclass, c("elnet", "lognet", "fishnet", "multnet", "coxnet", "mrelnet"))
	foundClass<-foundClass[!is.na(foundClass)]
	if(length(foundClass) > 0)
	{
		family<-c("gaussian","binomial","poisson","multinomial","cox","mgaussian")[foundClass]
	}
	
	retval <- list(
		call = this.call, 
		a0 = try(sapply(listOfLassoFits, "[[", "a0")), 
		beta = useBeta, 
		lambda = lambdas, 
		dev.ratio = try(sapply(listOfLassoFits, "[[", "dev.ratio")), 
		nulldev = try(sapply(listOfLassoFits, "[[", "nulldev")),
		df = try(sapply(listOfLassoFits, "[[", "df")), 
		dim = dim(useBeta), 
		nobs = listOfLassoFits[[1]]$nobs, 
		npasses = try(sum(sapply(listOfLassoFits, "[[", "npasses"))), 
		offset = listOfLassoFits[[1]]$offset, 
		jerr = try(sapply(listOfLassoFits, "[[", "jerr")), 
		result = result, 
		params = params, 
		logdir = dir, 
		family = family, 
		imputeDs2FitDsProperties = result[[1]]$imputeDs2FitDsProperties
	)
	
	class(retval) <- c(paste("EMLasso", resultclass, sep = "."), 
										 "EMLasso", resultclass)
	catwif(verbosity > 0, "Postprocessing final results and returning.")
	retval <- postProcessAll(predictorModel = result[[1]]$predictorModel, 
													 retval = retval, verbosity = verbosity - 1)
	return(retval)
}