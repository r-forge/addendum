#' Set tracing of all calls on/off
#' 
#' If set to TRUE, some calls to a \code{\link{addendum}} specific implementation is traced.
#' 
#' @param doDebug \code{TRUE} to turn it on, \code{FALSE} for off
#' @return invisibly the old value
#' @note not effectively used in the package at this time
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords tracing
#' @examples setDebugmodeEMLasso()
#' setDebugmodeEMLasso(FALSE)
#' @export
setDebugmodeEMLasso<-function(doDebug=TRUE){
	oldDebug<-.actual_debugmode$get()
	.actual_debugmode$set(doDebug)
	invisible(oldDebug)
}

#The rest below here are internal functions for tracing/debugging
.debugtxt<-function(...)
{
	if(.isEMLassoDebugging())
	{
		if(length(list(...))==0)
		{
			cat("**D:", curfnfinder(skipframes=2, extraPrefPerLevel=""), "\n")
		}
		else
		{
			cat("**D:", ..., "\n")
		}
	}
}

.emlasso_debugmode <- function() {
  .debugging <- FALSE

  list(
    get = function() .debugging,
    set = function(value) .debugging <<- value
  )
}
.actual_debugmode <- .emlasso_debugmode()
.isEMLassoDebugging<-function(){.actual_debugmode$get()}

.emlasso_debugEML <- function() {
  .curdata <- list()

  list(
    get = function() .curdata,
    set = function(value) .curdata <<- value,
    getProp = function(propname) .curdata[[propname]],
    setProp = function(propname, value) .curdata[[propname]] <<- value
  )
}
.actual_debugEML <- .emlasso_debugEML()
.setEMLProp<-function(propname, value){.actual_debugEML$setProp(propname, value)}
.getEMLProp<-function(propname){.actual_debugEML$getProp(propname)}
.setEML<-function(value){.actual_debugEML$set(value)}
.getEML<-function(){.actual_debugEML$get()}
.clearEML<-function() {.actual_debugEML$set(list())} 
.loadEML<-function(fn="debug.EML.for.predict.profiling.saved")
{
	ldenv<-new.env()
	ldd<-load(fn, envir=ldenv)
	.actual_debugEML$set(get(ldd, envir=ldenv))
	rm(list=ldd)
	invisible()
}
.saveEML<-function(fn="debug.EML.for.predict.profiling.saved")
{
	.debug.EML<-.actual_debugEML$get()
	save(list=".debug.EML", fn)
	rm(.debug.EML)
	invisible()
}
