#code to properly print the source for a function
printFuncSource<-function(funcname)
{
  tfn<-get(funcname)
  attr(tfn, "source")[1]<-paste(funcname, "<-", attr(tfn, "source")[1], sep="")
  print.default(tfn)
}

#printFuncSource("cleanPrint")

#Get a vector containing the names of all functions currently loaded
getAllCurrentFuncNames<-function(dropSourceGeneratingFuncs=TRUE)
{
  funcs<-c()
  for(curnm in objects(.GlobalEnv))
  {
    #print(curnm)
    if(!is.element(curnm, c("printFuncSource", "getAllCurrentFuncNames",
			"currentFuncDependencyTree", "getAllFuncsInDependencyOrder", "newLine",
			"printFuncs")))
    {
      curobj<-get(curnm)
      if(is.function(curobj))
      {
        funcs<-c(funcs,curnm)
      }
    }
  }
  funcs
}

#getAllCurrentFuncNames()

currentFuncDependencyTree<-function(aggressive=TRUE, verbosity=0)
{
  funcs<-getAllCurrentFuncNames()

  mainfunc<-c()
  dependsonfunc<-c()
  l<-length(funcs)
  if(l > 0)
  {
	  for(i in 1:l)
	  {
	    localdependentfuncs<-funcs[i]
	    catif(verbosity>0, "->", localdependentfuncs, "<-\n")
	    src<-tryRet(attr(get(funcs[i]), "source"), errRet="nofunc")
	    if(src!= "nofunc")
	    {
		    for(j in 1:l)
		    {
		      toBeFound=ifelse(aggressive, funcs[j], paste(funcs[j],"(",sep=""))
		      if(length(grep(toBeFound,src,fixed=TRUE)))
		      {
		        localdependentfuncs<-c(localdependentfuncs,funcs[j])
		      }
		    }
		    localdependentfuncs<-unique(localdependentfuncs)

		    mainfunc<-c(mainfunc, rep(funcs[i], length(localdependentfuncs)))
		    dependsonfunc<-c(dependsonfunc, localdependentfuncs)
	    }
	  }
	  data.frame(mainfunc=mainfunc, dependsonfunc=dependsonfunc)
	}
	else
	{
		data.frame(mainfunc="none found", dependsonfunc="none found")
	}
}

getAllFuncsInDependencyOrder<-function()
{
  cfdt<-currentFuncDependencyTree()

  orderedfuncs<-vector("character")
  allfuncs<-as.character(unique(cfdt$mainfunc))
  #remove selfdependency
  cfdt<-cfdt[cfdt$mainfunc!=cfdt$dependsonfunc,]
  lastcount<-1
  while((lastcount>0) && (length(cfdt$mainfunc)>0))
  {
    #functions that are depended on, but do not depend on others
    locallyindependent<-! is.element(cfdt$dependsonfunc, cfdt$mainfunc)
    #either there was nothing left, or there were mutual dependencies...
    lastcount<-sum(locallyindependent)
    if(lastcount>0)
    {
      #add the 'local independent' functions to the ordered list
      #their own dependencies should already be in there!
      orderedfuncs<-c(orderedfuncs, as.character(unique(cfdt$dependsonfunc[locallyindependent])))
      #remove dependency on these functions
      cfdt<-cfdt[!locallyindependent,]
    }
    else
    {
      warning(paste("There were mutual dependencies between:", paste(cfdt$dependsonfunc, sep=", ")))
    }
  }
  #add the rest
  orderedfuncs<-c(orderedfuncs, allfuncs[!is.element(allfuncs, orderedfuncs)])
  data.frame(func=orderedfuncs, print=TRUE)
}

newLine<-function()
{
  cat("\n")
}

printFuncs<-function(dfr)
{
  for(curnm in dfr$func[dfr$print])
  {
    printFuncSource(curnm)
    newLine()
  }
}

demoCodeWorkSpace<-function()
{
	funcdfr<-getAllFuncsInDependencyOrder()
	printFuncs(funcdfr)
}


memorySize<-function(objectname)
{
  object.size(get(objectname))
}

getAllCurrentObjectMemoryUsage<-function()
{
  obs<-objects(.GlobalEnv)
  sizes<-sapply(obs, memorySize)
  data.frame(obj=obs, sz=sizes)
}

showCallStackObjects<-function()
{
  n<-sys.nframe()-1
  for(i in n:0)
  {
    cat("Level", i, "\n")
    f<-sys.frame(i)
    print(f)
    print(objects(f))
  }
  invisible()
}

#showCallStackObjects()