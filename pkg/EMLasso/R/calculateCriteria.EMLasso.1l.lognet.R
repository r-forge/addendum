#' Function to calculate AUC / missclassification based on a logistic EMLasso
#' 
#' Function to calculate AUC / missclassification based on a logistic EMLasso
#' 
#' @param fitinfo \code{\link{EMLasso.1l.lognet}} object
#' @param valsample dataset to use for calculating the criteria
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return see the \code{criteria} part of \code{\link{cv.1l.emlasso}}
#' @note Passed as default \code{calculateCriteria} parameter to a lot of the functions
#' in this package
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{cv.1l.emlasso}}, \code{\link{EMLasso.1l.lognet.cv}},\code{\link{refreshCriteria}},
#' \code{\link{refreshCriteria.cvpart.emlasso}},\code{\link{refreshCriteria.cv.1l.emlasso}},
#' \code{\link{EMLasso.1l.lognet.cv.param}},
#' @keywords AUC missclassification
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlfit<-eml17$actualfits[[1]]$fitinfo
#' valSample<-predict(object=emlfit$glomo, nobs=5,
#' 	newdata=numdfr(aDfr.MD), returnRepeats=TRUE, verbosity=10)
#' valSample$resp<-rep(y, valSample$numRepPerRow)
#' crits<-calculateCriteria.EMLasso.1l.lognet(emlfit, valSample, verbosity=10)
#' @export
calculateCriteria.EMLasso.1l.lognet<-function(fitinfo, valsample, verbosity)
{
	catwif(verbosity > 0, "Create design matrix")
	dfr<-factorsToDummyVariables(valsample$predicted)
  N<-nrow(dfr) #number of observations
  if(N==0)
  {
		catw("Unexpected situation: no observations!")
  }
	catwif(verbosity > 0, "Predict")
  ##assume that predictedFactor is binary, and that the second level is the predicted one
  trueOutcome<-as.numeric(as.factor(valsample$resp))
  if(is.null(fitinfo$lasso.fit))
  {
		predProb<-rep(mean(trueOutcome-1, na.rm=TRUE), length(trueOutcome))
  }
  else
  {
  	predProb<-as.vector(predict(fitinfo$lasso.fit, dfr, type="response"))
  }
	catwif(verbosity > 0, "Past predictions")
  if(is.null(fitinfo$logreg.fit))
  {
		predProb_logreg<-rep(mean(trueOutcome-1, na.rm=TRUE), length(trueOutcome))
  }
  else
  {
		useCols<-rownames(fitinfo$logreg.fit$beta)
		useCols<-match(useCols, colnames(dfr))
		if(length(useCols) > 0)
		{
			dfr<-dfr[,useCols]
	  	predProb_logreg<-as.vector(predict(fitinfo$logreg.fit, dfr, type="response"))
		}
		else
		{
			predProb_logreg<-rep(mean(trueOutcome-1, na.rm=TRUE), length(trueOutcome))
		}
  	#predProb_logreg<-as.vector(predict(fitinfo$logreg.fit, as.data.frame(dfr), type="response"))
  }
	catwif(verbosity > 0, "Past predictions")


  thresholds<-seq(from=0.05, to=0.95, by=0.05) #maybe improve on this later ??

	catwif(verbosity > 0, "Result per threshold")
  allPredicted<-outer(predProb, thresholds, ">") #rows for each observation, cols per threshold
  resPerThres<-apply(allPredicted, 2, function(curPredCol){
			fpos<-(curPredCol & (trueOutcome==1))
			fneg<-((!curPredCol) & (trueOutcome==2))
			misclass<-(fpos | fneg)
			mcrate<-mean(misclass)
			mcrate.se<-sqrt(var(misclass)/N) #huh?
			fposrate<-mean(fpos) #=1-sensitivity
			fnegrate<-mean(fneg) #=1-specificity
			c(mcrate=mcrate, fposrate=fposrate, fnegrate=fnegrate, mcrate.se=mcrate.se)
		})

		TO<-(trueOutcome==2)
		TZ<-(trueOutcome==1)
	catwif(verbosity > 0, "AUCs")
		AUC1<-calcAUC.Binary(predProb, trueOnes=TO, trueZeroes=TZ, includeSE=TRUE,
			verbosity=verbosity-10)
		AUC2<-calcAUC.Binary(predProb, trueOnes=TO, trueZeroes=TZ, bootStrap=1000,
			includeSE=TRUE, verbosity=verbosity-10)
  return(list(predictionSummary=resPerThres, AUC=AUC1$AUC, varAUC=AUC1$varAUC,
		AUCbs=AUC2$AUC, varAUCbs=AUC2$varAUC, N=N, predProb=predProb,
		predProb_logreg=predProb_logreg))
}