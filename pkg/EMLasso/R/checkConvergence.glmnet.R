#' Check whether convergence has occurred in the zeroness of coefficients
#' 
#' Check whether convergence has occurred in the zeroness of coefficients
#' 
#' @param coefs (sparse) matrix of coefficients (cols=variables, rows=iterations)
#' @param minIt minimum number of iterations before convergence is possible
#' @param maxIt maximum number of iterations before convergence is automatically assumed
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return a single logical (\code{TRUE} is convergence has happened or maxIt passed)
#' @note the first row (initial estimates) and column (intercept) are skipped in the checks
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords convergence zeroness
#' @examples curset<-rbinom(11, 1, 0.5)
#' ret<-matrix(curset, nrow=1)
#' for(i in 1:10)
#' {
#' 	chg<-rbinom(11, 1, 0.05)
#' 	curset[chg==1] <- 1-curset[chg==1]
#' 	ret<-rbind(ret, curset)
#' }
#' print(ret)
#' checkConvergence.glmnet(ret, 2, 20, verbosity = 2)
#' @export checkConvergence.glmnet
checkConvergence.glmnet<-function(coefs, minIt, maxIt, verbosity = 0)
{
	#used to be full.glmnet.EM.fit.convergence
	converged<-FALSE
	coefs<-coefs[-1, -1]
	#first row holds initial model estimate: ignore this one
	#first column holds the intercepts: not penalized
	iterCount<-dim(coefs)[1]
	if(iterCount>= maxIt)
	{
		catwif(verbosity > 0, "Reached maximum nr of iterations (", maxIt, ")")
		converged<-TRUE
	}
	else if(iterCount>= minIt)
	{
		#suggested criterion for  convergence:
		#use the fraction of zeros to predict zeros in each column (1/2 threshold)
		#if the last row is the best match over all columns, we have convergence
		fracZeroPerCol<-apply(coefs, 2, function(curcol){mean(curcol==0)})
		bestZeroPerCol<-(fracZeroPerCol >= 0.5)
		if(verbosity > 0)
		{
			if(mean(bestZeroPerCol) > 0) #most of the cols most often zero, so print the rest
			{
				catw("Column coefficients most often NOT zero:\n", names(bestZeroPerCol)[!bestZeroPerCol])
			}
			else
			{
				catw("Column coefficients most often zero:\n", names(bestZeroPerCol)[bestZeroPerCol])
			}
		}
		matchesPerRow<-apply(coefs, 1, function(currow){
				sum(ifelse(bestZeroPerCol, currow==0, currow!=0))
			})
		catwif(verbosity > 0, "Matches per row:")
		printif(verbosity > 0, matchesPerRow)
		if(max(matchesPerRow)==matchesPerRow[length(matchesPerRow)])
		{
			converged<-TRUE
		}
	}
	else
	{
		catwif(verbosity > 0, "Haven't reached minimum nr of iterations (", minIt, ") yet.")
	}
	retval<-list(converged=converged, minIt=minIt, maxIt=maxIt, iterCount=iterCount, usedFunction="checkConvergence.glmnet")
	return(retval)
}
#' @rdname checkConvergence.glmnet
#' 
#' @aliases convergenceCheckCreator
#' @method convergenceCheckCreator
#' @usage convergenceCheckCreator(minIt, maxIt, basicCheckFunction=checkConvergence.glmnet)
#' @param basicCheckFunction function (like \code{checkConvergence.glmnet}) that will do the actual work
#' @return the result of \code{basicCheckFunction}
#' @seealso \code{\link{EMLasso}}
#' @export convergenceCheckCreator
convergenceCheckCreator<-function(minIt=20, maxIt=30, basicCheckFunction=checkConvergence.glmnet)
{
	force(minIt)
	force(maxIt)
	force(basicCheckFunction)
	retval<-function(coefs, verbosity=0)
	{
		basicCheckFunction(coefs, minIt, maxIt, verbosity = verbosity)
	}
	return(retval)
}
