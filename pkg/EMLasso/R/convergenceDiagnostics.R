#' Diagnose convergence of EMLasso.lognet objects
#' 
#' Diagnose convergence of \code{\link{EMLasso.lognet}} objects
#' 
#' @param model model fit (should be the result of \code{\link{EMLasso}})
#' @param minIt check convergence from this iteration on
#' @param checkConvergence method, normally \code{\link{checkConvergence.glmnet}}, that checks convergence.
#' @param \dots passed on to \code{checkConvergence}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return list with one item per lambda. Each item is a list with items:
#' \item{numit }{number of iterations until convergence for this lambda} 
#' \item{simpleconv }{for all iterations from minIt to the last one used, whether or not convergence occurred}
#' \item{changedNonZeroness }{for all iterations from minIt to the last one used, the variable names for which zeroness
#' 	is different from the last iteration}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords crossvalidate model
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
#' iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,  
#' 	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
#' sfStop()
#' convergenceDiagnostics(iris.emlognet, minIt=2, verbosity=1)
#' @export
convergenceDiagnostics<-function(model, minIt=10, checkConvergence=checkConvergence.glmnet, ..., verbosity=0)
{
	retval<-lapply(model$result, function(curres)
	{
		catwif(verbosity > 0, "Start of diagnostics for 1 lambda")
		cof<-curres$coefs
		numit<-nrow(cof)
		simpleconv<-sapply((minIt+1):numit, function(curit)
		{
			checkConvergence(coefs=cof[1:curit,,drop=FALSE], minIt=minIt, 
															maxIt=numit+1, verbosity=verbosity-1, ...)$converged
		})
		if(sum(simpleconv) > 1)
		{
			lastrow<-(cof[numit,,drop=TRUE]==0)
			changedNonZeroness<-sapply((minIt+1):numit, function(curit)
			{
				currow<-(cof[curit,,drop=TRUE]==0)
				diffs<-currow!=lastrow
				if(sum(diffs) > 0)
				{
					return(colnames(curres$coefs)[diffs])
				}
				else
				{
					return(NULL)
				}
			})
		}
		else
		{
			changedNonZeroness<-NULL
		}
		list(numit=numit, simpleconv=simpleconv, changedNonZeroness=changedNonZeroness)
	})
	return(retval)
}
