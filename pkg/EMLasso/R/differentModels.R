#' Find the different models selected during crossvalidation for EMLasso.lognet objects
#' 
#' Find the different models selected during crossvalidation for \code{\link{EMLasso.lognet}} objects
#' 
#' @param model \code{\link{EMLasso.lognet}} object
#' @param ds dataset on which to crossvalidate (default is the one used in \code{model})
#' @param out outcome variable on which to crossvalidate (default is the one used in \code{model})
#' @param nfolds number of folds for crossvalidation
#' @param dsconvprobs see \code{\link{dfrConversionProbs}} and \code{\link{fit.lognet}}
#' @param reusableDatas list of reusableData objects, one per \code{\link{GLoMo}} object, i.e. one per lambda. 
#' 	See also \code{\link{predict.GLoMo}}
#' @param zeroThres how high does a value have to be (in absolute value) to be nonzero.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return List with two items:
#' \item{uniqueres}{list holding information on all unique models selected over all crossvalidations and lambdas}
#' \item{perlamuniquecount}{number of unique models selected per lambda}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords EMLasso model unique
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
#' iris.emlognet<-fullDataEMLassoAndGLoMo(ds=numdfr(iris.cpy), out=y,  
#' 	showPlot=FALSE, type.measure="auc", lambdas=c(0.03,0.002,0.0003),
#' 	nrOfSamplesPerMDRow=10, verbosity=2,
#' 	minIt=5, maxIt=150, precalcGuidData=FALSE)
#' sfStop()
#' differentModels(iris.emlognet, verbosity=1)
#' @export
differentModels<-function(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, nfolds=10, 
	dsconvprobs, reusableDatas, zeroThres=0.0001, verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProbs(ds, betweenColAndLevel="")
	}
	lambda<-model$lambda
	if(missing(reusableDatas))
	{
		reusableDatas<-lapply(lapply(model$result, "[[", "glomo"), reusableDataForGLoMoSampling, 
													dfr = ds, verbosity = verbosity - 1)
	}
	mdlsPerLambda<-lapply(seq_along(lambda), function(j)
	{
		catwif(verbosity > 1, "  Lambda", j, "/", length(lambda))
		curglomo<-model$result[[j]]$glomo
		curds<-predict(curglomo, newdata=ds, reusabledata = reusableDatas[[j]], verbosity=verbosity-1)
		grps<-similarSizeGroups(ngroups=nfolds, nobs=nrow(curds), rand=TRUE)
		mdlPerFold<-lapply(seq(nfolds), function(curfld)
		{
			fitds<-curds[grps!=curfld,]
			fitout<-out[grps!=curfld]
			curcv<-fit.lognet(dfr=fitds, resp=fitout, lambda=lambda[j], verbosity=verbosity-2, 
												dfrConvData=dsconvprobs, standardize=FALSE)
			nzcofs<-(abs(curcv$beta[,1,drop=TRUE]) > zeroThres)
			return(sort(rownames(curcv$beta)[nzcofs]))
		})
		return(mdlPerFold)
	})
	allmdls<-do.call(c, mdlsPerLambda)
	catwif(verbosity>0, "Structure of allmdls:")
	strif(verbosity > 0, allmdls)
	mdltxts<-sapply(allmdls, paste, collapse=" + ")
	catwif(verbosity>0, "Structure of mdltxts:")
	strif(verbosity > 0, mdltxts)
	uniquemdltxts<-unique(mdltxts)
	firstposofuniquemdltxts<-match(uniquemdltxts, mdltxts)
	countofuniquemdltxts<-sapply(uniquemdltxts, function(txt){sum(mdltxts==txt)})
	
	uniqueres<-lapply(seq_along(uniquemdltxts), function(i){list(txt=uniquemdltxts[i], 
																															 firstpos=firstposofuniquemdltxts[i], cnt=countofuniquemdltxts[i], vars=allmdls[[firstposofuniquemdltxts[i]]])})
	perlamuniquecount<-sapply(mdlsPerLambda, function(curmdls){
		length(unique(sapply(curmdls, paste, collapse=" + ")))
	})
	list(uniqueres=uniqueres, perlamuniquecount=perlamuniquecount)
}
