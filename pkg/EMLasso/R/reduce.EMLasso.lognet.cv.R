#' Reduce memory footprint of a list of \code{\link{cv.1l.emlasso}} objects and recalculates some criteria
#' 
#' Reduce memory footprint of a list of \code{\link{cv.1l.emlasso}} objects and recalculates some criteria
#' 
#' @aliases reduce.EMLasso.lognet.cv cv.emlasso-class cv.emlasso
#' @param object list of \code{\link{cv.1l.emlasso}} objects or object of class "reducedResultList" 
#' @param orgdfr original dataset used. If missing, the first one found is used
#' @param orgresp original outcome vector used. If missing, the first one found is used
#' @param samplesForFinalFit how many samples must be used in the final GLoMo to make a dataset
#' for the final refit
#' @param \dots passed on to \code{\link{getSortedReducedResultList}} if needed
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param splitPatternLastPart see \code{\link{getSortedReducedResultList}}
#' @param keepReducedResultList if \code{TRUE}, the resulting "reducedResultList"
#' object is kept as an item of the return value
#' @return object of class "cv.emlasso" (and \code{\link{cv.glmnet}}, to be able to abuse some of \code{\link{glmnet}}'s code).
#'  (where I write dataset, this could either be a \code{\link{data.frame}} or \code{\link{numdfr}} object)
#' Very similarly shaped to \code{\link{cv.glmnet}} objects (in fact, all objects of this class are also \code{\link{cv.glmnet}})
#' \enumerate{
#' 	\item \code{lambda} : vector of lambda values
#' 	\item \code{cvm}: mean criterion per lambda
#' 	\item \code{cvsd}: sd of criterion per lambda
#' 	\item \code{cvup}: upper limit of criterion per lambda
#' 	\item \code{cvlo}: lower limit of criterion per lambda
#' 	\item \code{cvlo}: number of nonzeroes per lambda
#' 	\item \code{name}: name of criterion
#' 	\item \code{glmnet.fit}: the actual final lasso fit
#' 	\item \code{lambda.min}: optimum lambda
#' 	\item \code{lambda.1se}: biggest lambda with criterion within 1 se of best criterion
#' 	\item \code{orgdfr}: dataset originally used
#' 	\item \code{orgresp}: outcome vector originally used
#' 	\item \code{glomo}: \code{\link{GLoMo}} object at convergence
#' 	\item \code{valsample}: dataset used for validating (=by applying prediction from \code{glomo} to \code{orgdfr})
#' 	\item \code{orgcoef}: coefficients before refitting glmnet.fit
#' 	\item \code{reducedResultList}: (may not be present): list that was used to create the results (see \code{\link{getSortedReducedResultList}})
#' }
#' @method reduce EMLasso.lognet.cv
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{reduce}}
#' @keywords reduce
#' @export
reduce.EMLasso.lognet.cv<-function(object, orgdfr, orgresp,
	samplesForFinalFit=10, ..., verbosity=0, splitPatternLastPart="/",
	keepReducedResultList=FALSE)
{
	#note: may not be a memory-efficient solution... figure out later
	if(inherits(object, "reducedResultList"))
	{
		catwif(verbosity > 0, "Reusing passed along reducedResultList")
		reducedResultList<-object
	}
	else
	{
		catwif(verbosity > 0, "getSortedReducedResultList")
		if(missing(orgdfr)) #missing doesn't work in nested functions
		{
			reducedResultList<-getSortedReducedResultList(object, ..., verbosity=verbosity-1, splitPatternLastPart=splitPatternLastPart)
		}
		else
		{
			reducedResultList<-getSortedReducedResultList(object, orgdfr, ..., verbosity=verbosity-1, splitPatternLastPart=splitPatternLastPart)
		}
	}
	obj<-list()
	#OK, what do we want to keep ?
	#The goal of this function is _only_ to generate a cv.glmnet-like object!!
	#As such, we ignore the 'history' of the creation for the most part
	#See ?cv.glmnet to see what it contains:
#an object of class "cv.glmnet" is returned, which is a list with the ingredients of the cross-validation fit.
#lambda	 the values of lambda used in the fits.
#cvm	 The mean cross-validated error - a vector of length length(lambda).
#cvsd	 estimate of standard error of cvm.
#cvup	 upper curve = cvm+cvsd.
#cvlo	 lower curve = cvm-cvsd.
#nzero	 number of non-zero coefficients at each lambda.
#name	 a text string indicating type of measure (for plotting purposes).
#glmnet.fit	 a fitted glmnet object for the full data.
#lambda.min	 value of lambda that gives minimum cvm.
#lambda.1se	 largest value of lambda such that error is within 1 standard error of the minimum.
	#However: note that it is not so simply possible to have 1 member lasso.fit, as
	#this typically changes over the course of the lambdas !!
	#For now, we will	handle it like this: find all GLoMo objects between
	#lambda.min and lambda.1se and combine these to 1 validation set, to which
	#the lasso is refit.
	tmpres<-try({
		type.measure<-reducedResultList[[1]]$type.measure

		catwif(verbosity > 0, "simple subdata grouping")
		lambda<-sapply(reducedResultList, "[[", "lambda")
		cvm<-sapply(reducedResultList, "[[", "cvm")
		cvsd<-sapply(reducedResultList, "[[", "cvsd")
		cvup<-sapply(reducedResultList, "[[", "cvup")
		cvlo<-sapply(reducedResultList, "[[", "cvlo")
		catwif(verbosity > 0, "nzero")
		nzero<-sapply(reducedResultList, function(curobj){
				length(predict(curobj$lasso.fit, type = "nonzero")[[1]])#predict nonzero returns a list with an item for every lambda
			})
		name<-sapply(reducedResultList, "[[", "name")
		catwif(verbosity > 0, "lamin")
		lamin <- if (type.measure == "auc")
			getmin(lambda, -cvm, cvsd)
		else getmin(lambda, cvm, cvsd)

		catwif(verbosity > 0, "lambdas for GLoMo combining")
		relvlampos<-as.list(sort(match(lamin, lambda)))
		names(relvlampos)<-c("from", "to")
		relvlampos<-do.call(seq, relvlampos)
		catwif(verbosity > 0, "used lambdas for GLoMo combining:", relvlampos)

		catwif(verbosity > 0, "combineGLoMos")
		listGLoMos<-lapply(reducedResultList[relvlampos], "[[", "glomo")
		glomo<-combineGLoMos(listOfGLoMos=listGLoMos, verbosity=verbosity-1)
		if(missing(orgdfr))
		{
			catwif(verbosity > 0, "Restoring orgdfr from reduced results")
			orgdfr<-originalDataset(reducedResultList[[1]]$valsample$predicted)
			catwif(verbosity > 0, "Restored orgdfr from reduced results; dim:", dim(orgdfr))
		}
		if(missing(orgresp))
		{
			catwif(verbosity > 0, "Restoring orgresp from reduced results")
			#normally this should work to rebuild the original response...
			firstvalsample<-reducedResultList[[1]]$valsample
			firstposofeach<-cumsum(c(1, firstvalsample$numRepPerRow))
			orgrows<-as.integer(names(firstvalsample$numRepPerRow))
			orgresp<-firstvalsample$resp[firstposofeach]
			orgresp<-orgresp[order(orgrows)]
			catwif(verbosity > 0, "Restored orgresp from reduced results; length:", length(orgresp))
		}

		catwif(verbosity > 0, "using combined GLoMo to complete orgdfr")
		valsample<-predict(glomo, nobs=samplesForFinalFit, newdata=orgdfr, returnRepeats=TRUE, verbosity=verbosity-1)
		valsample$resp<-orgresp[rep(as.integer(names(valsample$numRepPerRow)), valsample$numRepPerRow)]
		valsample$wts<-rep(1/valsample$numRepPerRow, valsample$numRepPerRow)

		catwif(verbosity > 0, "refitting lasso to completed dataset")
		lasso.fit<-fit.lognet(valsample$predicted, valsample$resp, lambda, valsample$wts,
			verbosity=verbosity-1)

		catwif(verbosity > 0, "collecting 'original' coefficients per lambda")
		orgcoef<-do.call(cBind, lapply(reducedResultList, function(curobj){coef(curobj$lasso.fit)}))
		orgitc<-as.vector(orgcoef[1,])
		orgcoef<-orgcoef[-1,]
		colnames(orgcoef)<-paste("s", seq(ncol(orgcoef)), sep="")

		#first act like cv.glmnet:
		out<-list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvup, cvlo = cvlo,
			nzero = nzero, name = name, glmnet.fit = lasso.fit)
		obj <- c(out, as.list(lamin))
		obj$orgdfr<-orgdfr
		obj$orgresp<-orgresp
		obj$glomo<-glomo
		obj$valsample<-valsample
		obj$orgcoef<-orgcoef
		obj$orgitc<-orgitc
	})
	if(inherits(tmpres, "try-error"))
	{
		catw("An error occurred:")
		print(tmpres)
		catw("Will return what I got so far...")
	}
	if(keepReducedResultList)
	{
		obj$reducedResultList<-reducedResultList
	}
	class(obj) <- c("cv.emlasso", "cv.glmnet")
	return(obj)
}