#' Collect validation data for lognet
#' 
#' Collect reusable validation data for lognet
#' 
#' @param glmnetmodel \code{\link{glmnet}} binomial model fit
#' @param dfr dataset for which validation needs to occur
#' @param betweenColAndLevel separator for dummy column naming
#' @param outcomes true outcome vector
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return list of class "LognetValidationData":
#' \item{valModelWrapper }{result of \code{\link{lognetProbWrapper}}} \item{outcomes }{as passed in}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{lognetProbWrapper}}
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' irisdummy<-factorsToDummyVariables(iris, verbosity=10)
#' lnet<-glmnet(irisdummy, y, family="binomial")
#' LognetValidationData<-function(glmnetmodel=lnet, dfr=iris, betweenColAndLevel="", outcomes=y, verbosity=1)
#' @export
LognetValidationData<-function(glmnetmodel, dfr, betweenColAndLevel, outcomes, verbosity=0)
{
	retval<-list(valModelWrapper=lognetProbWrapper(glmnetmodel, dfr,
		betweenColAndLevel, verbosity=verbosity-1), outcomes=outcomes)
	class(retval)<-"LognetValidationData"
	return(retval)
}
