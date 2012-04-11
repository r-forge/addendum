#' Create reusable part of validation (for rejection sampling)
#' 
#' Create reusable part of validation (for rejection sampling)
#' 
#' @param outcomeModel outcome model (e.g. \code{\link{glmnet}})
#' @param ds dataset for which validation needs to occur
#' @param out outcomes for which validation needs to occur
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return a single logical (\code{TRUE} is convergence has happened or maxIt passed)
#' @note the first row (initial estimates) and column (intercept) are skipped in the checks
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords outcome validation rejection sampling
#' @export outcomeModelValidationReusables
outcomeModelValidationReusables<-function(outcomeModel, ds, out, imputeDs2FitDsProperties, verbosity=0) UseMethod("outcomeModelValidationReusables")
#' @rdname outcomeModelValidationReusables
#' 
#' @aliases outcomeModelValidationReusables.default
#' @method outcomeModelValidationReusables.default
#' @usage \method{outcomeModelValidationReusables}{default}(outcomeModel, ds, out, imputeDs2FitDsProperties, verbosity=0)
#' @return the result of \code{basicCheckFunction}
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method outcomeModelValidationReusables default
outcomeModelValidationReusables.default<-function(outcomeModel, ds, out, imputeDs2FitDsProperties, verbosity=0)
{
	retval<-list()
	class(retval)<-"SamplingReusables"
	return(retval)
}
#' @rdname outcomeModelValidationReusables
#' 
#' @aliases outcomeModelValidationReusables.lognet
#' @method outcomeModelValidationReusables.lognet
#' @usage \method{outcomeModelValidationReusables}{lognet}(outcomeModel, ds, out, imputeDs2FitDsProperties, verbosity=0)
#' @return the result of \code{basicCheckFunction}
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method outcomeModelValidationReusables lognet
outcomeModelValidationReusables.lognet<-function(outcomeModel, ds, out, imputeDs2FitDsProperties, verbosity=0)
{
	retval<-list(valModelWrapper=lognetProbabilityReusable(lognet=outcomeModel, ds=ds,
		imputeDs2FitDsProperties=imputeDs2FitDsProperties, verbosity=verbosity-1), outcomes=out)
	class(retval)<-"SamplingReusablesLognet"
	return(retval)
}
	