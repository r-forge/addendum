
#start snowfall and run initial scripts on all instances
#antonov: if TRUE, use server paths, else use local paths
#parallel: if FALSE, run the work sequentially
#serverFolder / localFolder: local path where initial script are
#scriptsInSubFolderToRun: subpath/filenames to the initial scripts
#serverCPUs / localCPUs: number of CPUs to be used if parallel
doSf<-function(antonov=FALSE, parallel=TRUE,
	scriptsInSubFolderToRun=c("Dysphagia/ModelSelection.Prediction.MT.MICE.r",
		"Dysphagia/Dysphagia.ModelSelection.Prediction.MT.MICE.r"),
	serverCPUs=10, localCPUs=2, serverFolder="/home/nsabbe/Dropbox/Doctoraat/",
	localFolder="C:\\users\\nisabbe\\Documents\\@Doctoraat\\")
{
	require(snowfall)
	cpus<-ifelse(antonov, serverCPUs, localCPUs)
	if(antonov)
	{
		parallel<-TRUE
	}
	sfInit(parallel=parallel, cpus=cpus)
	cat("Started snowfall. Will now run scripts:\n")
	whereToRun<-ifelse(antonov, serverFolder, localFolder)
	whatToRun<-paste(whereToRun, scriptsInSubFolderToRun, sep="")
	print(whatToRun)
	sapply(whatToRun, sfSource)
}

#typical use:
#require(snowfall)
#doSf(antonov=TRUE)
#sfLibrary(snowfall)

demoMemUsage1<-function()
{
	#list memory size of one object:
	object.size

	memory.limit()
	#How many megabytes are available to R at the most
	#You can increase this, but only to a limit (on 32bit windows typically 2 Gb = 2047)
	#by passing a number of megabytes. Note: you cannot lower this!
	memory.size(TRUE)
	#Size of the biggest possible object you can allocate memory for
	#in current version of R (in Mb)
	memory.size()
	#How much memory is currently actually in use
	gc(verbose=TRUE)
	#clean up unused memory
}
