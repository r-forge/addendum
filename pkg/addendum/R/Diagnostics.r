
#list memory sizes of all non-function objects present in memory
	#threshold<-100
showMemoryUsage<-function(threshold, depth=1, forEnvironment=globalenv())
{
	if(is.null(forEnvironment)) forEnvironment<-parent.frame()
	invisible(sapply(objects(forEnvironment), function(curobname){
			curob<-get(curobname, envir=forEnvironment)
			if(!is.function(curob))
			{
				msize<-round(object.size(curob)/1024, 1L)
				if(msize >= threshold)
				{
					cat(curobname, ":", msize, "Kb\n")
					if(is.list(curob))
					{
						showMemoryUsageList(curob, threshold, depth=depth-1, indent="  ")
					}
				}
			}
		}))
}


showMemoryUsageList<-function(lst, threshold, depth=1, indent="  ")
{
	if(depth>0)
	{
		invisible(sapply(seq_along(lst), function(curlsti){
				curob<-lst[[curlsti]]
				if(!is.function(curob))
				{
					msize<-round(object.size(curob)/1024, 1L)
					if(msize >= threshold)
					{
						curobname<-names(lst)[curlsti]
						if(is.null(curobname)) curobname<-curlsti
						cat(indent, "$", curobname, ":", msize, "Kb\n")
						if(is.list(curob))
						{
							showMemoryUsageList(curob, threshold, depth=depth-1, indent=paste(indent, "  ", sep=""))
						}
					}
				}
			}))
	}
}

demoMemUsage2<-function()
{
	showMemoryUsage(threshold=1000, depth=2)
	showMemoryUsage(threshold=500, depth=2)
	#showMemoryUsageList(dysp.cvres.mult[[14]][[10]], threshold=0, depth=2, indent="")
	gc(verbose=TRUE)
	memory.size()
}

#convert the result of system.time to something that can be used
tshort<-function(x)
{
	y <- x
	if (!is.na(y[4L]))
		y[1L] <- y[1L] + y[4L]
	if (!is.na(y[5L]))
		y[2L] <- y[2L] + y[5L]
	retval<-y[1L:3L]
	names(retval)<-c(gettext("user"), gettext("system"), gettext("elapsed"))
	return(retval)
#	paste(c(gettext("user"), gettext("system"), gettext("elapsed")), format(y[1L:3L], nsmall=2, digits=1), sep=": ", collapse=", ")
}

#convert the result of system.time to something that can be displayed/traced
# like: "user: 0.54, system: 2.62, elapsed: 3.26"
ttxt<-function(x)
{
	y <- tshort(x)
	paste(names(y), format(y, nsmall=2, digits=1), sep=": ", collapse=", ")
}


#tp could be something like "do.*" to find all functions starting with "do"
listFuncsOfType<-function(tp, envir=globalenv())
{
	nms<-ls(envir=envir, pattern=tp)
	funcs<-sapply(nms, function(nm){is.function(get(nm))})
	nms<-nms[funcs]
	sapply(nms, get)
}