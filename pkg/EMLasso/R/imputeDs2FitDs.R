#' Reusable information to convert an imputed dataset to fitting form
#' 
#' Reusable information to convert an imputed dataset to fitting form
#' 
#' @param conversionData object that holds information to convert an imputed dataset
#' 	like \code{\link{dfrConversionProps}} or a custom implementation.
#' @param ds dataset for which the lambdas need to be found
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param \dots For specific implementations
#' @return dataset or matrix that can be used for fitting - depends on the implementation
#' @note abstracts away creation of conversionData: see \code{\link{EMLasso}}
#' @keywords impute dataset conversion
#' @export imputeDs2FitDs
imputeDs2FitDs<-function(conversionData,ds,verbosity=0,...) UseMethod("imputeDs2FitDs")
#' @rdname imputeDs2FitDs
#' 
#' @aliases imputeDs2FitDs imputeDs2FitDs.default
#' @method imputeDs2FitDs.default
#' @usage \method{imputeDs2FitDs}{default}(conversionData,ds,verbosity=0,...)
#' @return In this implementation, log the fact + return the incoming dataset \code{ds}
#' @seealso \code{\link{EMLasso}}
#' @S3method imputeDs2FitDs default
imputeDs2FitDs.default<-function(conversionData,ds,verbosity=0,...)
{
	catwif(verbosity > 0, "Conversion of imputed dataset using unknown conversion data.")
	return(ds)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases imputeDs2FitDs imputeDs2FitDs.dfrConversionProps
#' @method imputeDs2FitDs.dfrConversionProps
#' @usage \method{imputeDs2FitDs}{dfrConversionProps}(conversionData,ds,verbosity=0,...)
#' @return In this implementation, use \code{\link{factorsToDummyVariables}}
#' @seealso \code{\link{EMLasso}} \code{\link{factorsToDummyVariables}}
#' @S3method imputeDs2FitDs dfrConversionProps
imputeDs2FitDs.dfrConversionProps<-function(conversionData,ds,verbosity=0,...)
{
	return(factorsToDummyVariables(dfr=ds, betweenColAndLevel = conversionData$betweenColAndLevel, 
		dfrConvData=conversionData, verbosity=verbosity, ...))
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases imputeDs2FitDs.dfrConversionPropsEx
#' @method imputeDs2FitDs.dfrConversionPropsEx
#' @usage \method{imputeDs2FitDs}{dfrConversionPropsEx}(conversionData,ds,verbosity=0,...)
#' @return In this implementation, use \code{\link{factorsToDummyVariables}}
#' @seealso \code{\link{EMLasso}} \code{\link{factorsToDummyVariables}}
#' @S3method imputeDs2FitDs dfrConversionPropsEx
imputeDs2FitDs.dfrConversionPropsEx<-function(conversionData,ds,verbosity=0,...)
{
	retval<-factorsToDummyVariables(dfr=ds, betweenColAndLevel = conversionData$betweenColAndLevel, 
		dfrConvData=conversionData, verbosity=verbosity-5, ...)
	catwif(verbosity>5, "After factorsToDummyVariables, dim is:", dim(retval))
	if(verbosity > 20)
	{
		catw("The top left corner of the data looks like this:")
		print(retval[seq(min(nrow(retval), 10)), seq(min(ncol(retval), 15))])
	}
	if(exists("transformFunc", conversionData))
	{
		catwif(verbosity>5, "Found tranforming function so runnning it now")
		retval<-conversionData$transformFunc(retval, conversionData$transformData, verbosity=verbosity-1)
	}
	catwif(verbosity>5, "After transformations, dim is:", dim(retval))
	if(verbosity > 20)
	{
		catw("The top left corner of the data looks like this:")
		print(retval[seq(min(nrow(retval), 10)), seq(min(ncol(retval), 15))])
	}
	if(exists("scaleFunc", conversionData))
	{
		catwif(verbosity>5, "Found scaling function so running it now")
		retval<-conversionData$scaleFunc(retval, centers=conversionData$usedCenters, scales=conversionData$usedScales, verbosity=verbosity-1)
	}
	catwif(verbosity>5, "After scaling, dim is:", dim(retval))
	if(verbosity > 20)
	{
		catw("The top left corner of the data looks like this:")
		print(retval[seq(min(nrow(retval), 10)), seq(min(ncol(retval), 15))])
	}
	return(retval)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases imputeDs2FitDsProps
#' @method imputeDs2FitDsProps
#' @usage imputeDs2FitDsProps(object,ds,verbosity=0)
#' @param object typically the return value of a call to \code{imputeDs2FitDs}
#' @return Dataset to be used for fitting in EMLasso
#' @note There is no reason to include ellipsis as a parameter! If you think you need it,
#' 	look at the workaround through \code{normalImputationConversion}.
#' @seealso \code{\link{EMLasso}}
#' @export imputeDs2FitDsProps
imputeDs2FitDsProps<-function(object,ds,verbosity=0) UseMethod("imputeDs2FitDsProps")
#' @rdname imputeDs2FitDs
#' 
#' @aliases imputeDs2FitDsProps imputeDs2FitDsProps.default
#' @method imputeDs2FitDsProps.default
#' @usage \method{imputeDs2FitDsProps}{default}(object,ds,verbosity=0)
#' @return In this (default) implementation, return whatever was passed in (\code{object})
#' @seealso \code{\link{EMLasso}}
#' @S3method imputeDs2FitDsProps default
imputeDs2FitDsProps.default<-function(object,ds,verbosity=0)
{
	return(object)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases imputeDs2FitDsProps imputeDs2FitDsProps.normalImputationConversion
#' @method imputeDs2FitDsProps.normalImputationConversion
#' @usage \method{imputeDs2FitDsProps}{normalImputationConversion}(object,ds,verbosity=0)
#' @return In this (default) implementation, creat a \code{\link{dfrConversionProps}}
#' @seealso \code{\link{EMLasso}}
#' @S3method imputeDs2FitDsProps normalImputationConversion
imputeDs2FitDsProps.normalImputationConversion<-function(object,ds,verbosity=0)
{
	catwif(verbosity > 0, "[imputeDs2FitDsProps] was not passed along but needed, so will recalculate it")
	retval<-dfrConversionProps(dfr=ds, betweenColAndLevel=object$betweenColAndLevel, 
		includeBaseLevel=object$includeBaseLevel, verbosity=verbosity)
	mat<-factorsToDummyVariables(dfr=ds, dfrConvData=retval)
	catwif(verbosity>10, "After factorsToDummyVariables, mat has dimensions:", dim(mat))
	
	if((!is.null(object$scalingParams)) | (!is.null(object$transformParams)))
	{
		class(retval)<-paste(class(retval), c("Ex", ""), sep="")
		extranames<-character()
		if(!is.null(object$transformParams))
		{
			toAddTransformChecker<-function(curFunc, orgname, orgvals, verbosity=0)
			{
				if(verbosity>0)
				{
					catw("Checking NULL return for column", orgname)
					catw("curFunc is:")
					print(curFunc)
					catw("and the structure of the data to which it will be applied is:")
					str(orgvals)
				}
				toAdd<-getSafeFunction(curFunc, orgvals)
				curFunc<-getUnsafeFunction(curFunc)
				if(any(newIllegals(f=curFunc,x=orgvals)))
				{
					#This means that the function already has bad values in the inital
					#dataset. We will not include it!
					return(NULL)
				}
				return(toAdd)
			}
			if(exists("toAddTransformChecker", object$transformParams))
			{
				toAddTransformChecker<-object$transformParams$toAddTransformChecker
				object$transformParams$toAddTransformChecker<-NULL
			}
			if(exists("_AllNonFact", object$transformParams))
			{
				#special case: user wants to add a set of transforms to all continuous columns
				#in this case, this setting overrides anything that was specified for any column
				#This may change in later implementations
				catwif(verbosity > 1, "adding transformations for all non-factor columns")
				anf<-object$transformParams[["_AllNonFact"]]
				nfactcols<-unique(retval$newformdata$coln[!retval$newformdata$isfact])
				object$transformParams<-lapply(nfactcols, function(nfc){anf})
				names(object$transformParams)<-nfactcols
			}
			if(length(object$transformParams) > 0)
			{
				catwif(verbosity > 1, "check if any transformations return NULL")
				for(coli in seq_along(object$transformParams))
				{
					orgname<-names(object$transformParams)[coli]
					curob<-object$transformParams[[coli]]
					useThese<-sapply(curob, toAddTransformChecker, orgname, mat[,orgname], verbosity=verbosity-1, simplify=FALSE)
					areNull<-sapply(useThese, is.null)
					if(any(areNull))
					{
						catwif(verbosity > 1, "Some transformations for column", orgname, "were skipped:", names(curob)[areNull])
					}
					object$transformParams[[coli]]<-useThese[!areNull]
				}
				transleft<-sapply(object$transformParams, length)
				if(any(transleft <=0))
				{
					catwif(verbosity > 1, "No more transformations left for column(s)", names(object$transformParams)[transleft <=0])
				}
				object$transformParams<-object$transformParams[transleft>0]
			}
			if(length(object$transformParams) > 0)
			{
				catwif(verbosity > 5, "gathering final transformation information")
				transformData<-list()
				transformData$orgnames<-do.call(c, lapply(seq_along(object$transformParams), function(obi){rep(names(object$transformParams)[obi], length(object$transformParams[[obi]]))}))
				transformData$exes<-do.call(c, lapply(object$transformParams, names))
				transformData$newnames<-paste(transformData$orgnames, transformData$exes, sep="")
				transformData$useFuncs<-do.call(c, object$transformParams)
				retval$transformData<-transformData
				catwif(verbosity > 5, "creating transformation function")
				force(object$transformParams)
				transformFunc<-function(amat, transformData, verbosity=0)
				{
					catwif(verbosity > 10, "tranformFunc mat has the following dim and colnames at the start:\n\t", dim(amat), "\n\t", colnames(amat))
					newmat<-vapply(seq_along(transformData$orgnames), function(coli)
					{
						catwif(verbosity > 10,"*tranformFunc Working on transformation", coli, "/", length(transformData$orgnames))
						curorgname<-transformData$orgnames[coli]
						curex<-transformData$exes[coli]
						curnewname<-transformData$newnames[coli]
						catwif(verbosity > 10,"*tranformFunc orgcol:", curorgname, ", ex:", curex, ", newname:", curnewname)
						curFunc<-transformData$useFuncs[[coli]]
						
						curorgcol<-amat[,curorgname]
						catwif(verbosity > 10,"*tranformFunc applied to curorgcol of structure:")
						strif(verbosity > 10,curorgcol)
						return(curFunc(curorgcol))
					}, amat[,1])
					if(is.null(dim(newmat)))
					{
						#note: have checked this: this should only happen if each returned vector has length 1
						newmat<-matrix(newmat, ncol=length(transformData$orgnames))
					}
					catwif(verbosity > 10,"*tranformFunc newmat has the following dim:", dim(newmat))
					colnames(newmat)<-transformData$newnames
					catwif(verbosity > 10,"*and colnames:\n\t", colnames(newmat))
					amat<-cbind(amat, newmat)
					catwif(verbosity > 10,"*tranformFunc mat has the following dim and colnames:\n\t", dim(amat), "\n\t", colnames(amat))
					
					return(amat)
				}
				catwif(verbosity > 5, "created transformation function")
				retval$transformFunc<-transformFunc
				orgnames<-colnames(mat)
				if(verbosity > 20)
				{
					catw("Before applying transformation function as test, the top left corner of the matrix looks like:")
					print(mat[seq(min(nrow(mat), 10)), seq(min(ncol(mat), 15))])
				}
				mat<-transformFunc(mat, transformData, verbosity=verbosity-1)#so the extra columns are present!! See below for scalingParams
				if(verbosity > 20)
				{
					catw("After applying transformation function as test, the top left corner of the matrix looks like:")
					print(mat[seq(min(nrow(mat), 10)), seq(min(ncol(mat), 15))])
				}
				catwif(verbosity > 5, "finding transformed column names")
				extranames<-setdiff(colnames(mat), orgnames)
				retval$extranames<-extranames
			}
		}
		if(!is.null(object$scalingParams))
		{
			nfactcols<-unique(retval$newformdata$newcoln[!retval$newformdata$isfact])
			catwif(verbosity > 10, "nfactcols:", nfactcols)
			factcols<-unique(retval$newformdata$newcoln[retval$newformdata$isfact])
			catwif(verbosity > 10, "factcols:", factcols)
			extracols<-extranames
			catwif(verbosity > 10, "extracols:", extracols)
			scalecols<-setdiff(object$scalingParams$scale, c("_AllNonFact", "_AllFact", "_All", "_AllExtra"))
			catwif(verbosity > 10, "Scale columns without specials:", scalecols)
			if("_All" %in% object$scalingParams$scale)
			{
				scalecols<-c(scalecols, nfactcols, factcols, extracols)
			}
			else
			{
				if("_AllNonFact" %in% object$scalingParams$scale)
				{
					scalecols<-c(scalecols, nfactcols)
				}
				if("_AllFact" %in% object$scalingParams$scale)
				{
					scalecols<-c(scalecols, factcols)
				}
				if("_AllExtra" %in% object$scalingParams$scale)
				{
					scalecols<-c(scalecols, extracols)
				}
			}
			scalecols<-unique(scalecols)
			scales<-sapply(scalecols, function(curcn){rv<-sd(mat[,curcn], na.rm = TRUE);catwif(verbosity>10, "Scale for column", curcn, ":", rv);return(rv)})
			scales<-scales[!is.na(scales)]
			scales<-scales[!is.nan(scales)]
			scales<-scales[scales!=0]
			
			centercols<-setdiff(object$scalingParams$center, c("_AllNonFact", "_AllFact", "_All", "_AllExtra"))
			if("_All" %in% object$scalingParams$center)
			{
				centercols<-c(centercols, nfactcols, factcols, extracols)
			}
			else
			{
				if("_AllNonFact" %in% object$scalingParams$center)
				{
					centercols<-c(centercols, nfactcols)
				}
				if("_AllFact" %in% object$scalingParams$center)
				{
					centercols<-c(centercols, factcols)
				}
				if("_AllExtra" %in% object$scalingParams$center)
				{
					centercols<-c(centercols, extracols)
				}
			}
			centercols<-unique(centercols)
			printif(verbosity > 1, centercols)
			centers<-sapply(centercols, function(curcn){rv<-mean(mat[,curcn], na.rm = TRUE);catwif(verbosity>10, "Center for column", curcn, ":", rv);return(rv)})
			centers<-centers[!is.na(centers)]
			centers<-centers[!is.nan(centers)]

			retval$usedCenters<-centers
			retval$usedScales<-scales
			
			scaleFunc<-function(mat, centers, scales, verbosity=0)
			{
				for(curci in seq_along(centers))
				{
					curcn<-names(centers)[curci]
					curc<-centers[curci]
					catwif(verbosity>10, "Will now center column", curcn, "around center", curc)
					mat[,curcn]<-mat[,curcn]-curc
				}
				for(cursi in seq_along(scales))
				{
					cursn<-names(scales)[cursi]
					curs<-scales[cursi]
					catwif(verbosity>10, "Will now scale column", cursn, "around center", curs)
					mat[,cursn]<-mat[,cursn]/curs
				}
				return(mat)
			}
			scaleBackFunc<-function(mat, scales, centers, verbosity=0)
			{
				for(cursi in seq_along(scales))
				{
					cursn<-scalecols[cursi]
					curs<-scales[cursi]
					mat[,cursn]<-mat[,cursn]*curs
				}
				for(curci in seq_along(centers))
				{
					curcn<-centercols[curci]
					curc<-centers[curci]
					mat[,curcn]<-mat[,curcn]+curc
				}
				return(mat)
			}
			retval$scaleFunc<-scaleFunc
			retval$scaleBackFunc<-scaleBackFunc
		}
	}
	return(retval)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases normalImputationConversion normalImputationConversion-class
#' @method normalImputationConversion
#' @usage normalImputationConversion(betweenColAndLevel = "", includeBaseLevel=FALSE, scalingParams=NULL, transformParams=NULL)
#' @param betweenColAndLevel see \code{\link{dfrConversionProps}}
#' @param includeBaseLevel see \code{\link{dfrConversionProps}}
#' @param scalingParams list that may contain two items: "scale" and "center". Each are character vectors indicating which
#' 	columns need to be scaled/centered. You can also use any of the meta-columns: "_AllNonFact", "_AllFact", "_All", "_AllExtra"
#' @param transformParams list with an item per column that you want to apply transformations to (or you can use meta-column 
#' 	"_AllNonFact"). Each item is itself a list. The names are the extension that will be appended to the column name, the value
#' 	is the function that will be applied to the column.
#' @return object of class \code{normalImputationConversion}
#' @seealso \code{\link{EMLasso}}
#' @export normalImputationConversion
normalImputationConversion<-function(betweenColAndLevel = "", includeBaseLevel=FALSE, scalingParams=NULL, transformParams=NULL)
{
	retval<-list(betweenColAndLevel=betweenColAndLevel, includeBaseLevel=includeBaseLevel,
		scalingParams=scalingParams, transformParams=transformParams)
	class(retval)<-"normalImputationConversion"
	return(retval)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases typicalTransformations
#' @method typicalTransformations
#' @usage typicalTransformations(nm="_AllNonFact", addGon=FALSE)
#' @param nm name of the column or "_AllNonFact" (symbolically representing all non-factor columns) that these
#' 	transformations will be applied to.
#' @param addGon if \code{TRUE}, extra goniometrical transformations are added (\code{sin}, \code{cos} and \code{tan})
#' @return object that is ideally fit for use as parameter \code{transformParams} of \code{normalImputationConversion}
#' @export typicalTransformations
typicalTransformations<-function(nm="_AllNonFact", addGon=FALSE)
{
	rv<-list(
		list(
			Sq=function(x){return(x^2)},
			Thr=function(x){return(x^3)},
			Sqrt=illegalsCountered(sqrt),
			Exp=exp,
			Log=illegalsCalculatedConstCountered(log)
#			illegalsCountered(log, f2=constButWarnFunction(specialLegalX(log, x, smry=min, dflt=1e-10)))}
#			Log=function(x){illegals2Null(log,x)}
			)
		)
	names(rv)[1]<-nm
	if(addGon)
	{
		rv[[1]]$Sin<-sin
		rv[[1]]$Cos<-cos
		rv[[1]]$Tan<-tan#we assume that it is extremely unlikely to get illegal values here...
	}
	return(rv)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases typicalScaleAndCenter
#' @method typicalScaleAndCenter
#' @usage typicalScaleAndCenter()
#' @return object that is ideally fit for use as parameter \code{scalingParams} of \code{normalImputationConversion}
#' @export typicalScaleAndCenter
typicalScaleAndCenter<-function()
{
	return(list(center=c("_AllNonFact", "_AllExtra"), scale="_All"))
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases newIllegals
#' @method newIllegals
#' @usage newIllegals(f,x)
#' @param f function for which illegal results (\code{NA} or \code{NaN}) will be checked. You can also directly
#' 	pass along the return values of some function
#' @param x data for which the results of \code{f} will be checked
#' @return logical vector of the same length as \code{x}. \code{TRUE} for elements of \code{x} that resulted in an
#' 	'illegal' return value of \code{f}
#' @details 'Illegal' means that the result became \code{NA}, \code{NaN} or \code{infinite} where \code{x}
#' 	was none of those.
#' @note Warnings during the calling of \code{f} are suppressed
#' @export newIllegals
newIllegals<-function(f,x)
{
# 	tc<-match.call()
# 	print(tc)
	if(is.function(f))
	{
		rv<-suppressWarnings(f(x))
	}
	else
	{
		rv<-f
	}
	return(isIllegal(rv) & (! isIllegal(x)))
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases isIllegal
#' @method isIllegal
#' @usage isIllegal(x)
#' @return logical vector that holds \code{TRUE} for each 'illegal' element of \code{x}
#' @export isIllegal
isIllegal<-function(x)
{
# 	tc<-match.call()
# 	print(tc)
	is.na(x) | is.nan(x) | is.infinite(x)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases removeIllegals
#' @method removeIllegals
#' @usage removeIllegals(x)
#' @return copy of \code{x} where all the 'illegal' values have been removed
#' @export removeIllegals
#' @examples removeIllegals(c(1,NA,3,1/0,Inf))
removeIllegals<-function(x)
{
# 	tc<-match.call()
# 	print(tc)
	ill<-isIllegal(x)
	x[!ill]
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases illegalsCountered unsafefunction unsafefunction-class
#' @method illegalsCountered
#' @usage illegalsCountered(f, f2=constButWarnFunction(), f2OnAll=FALSE)
#' @param f2 function that is called for the items of \code{x} that give 'illegal' results
#' @param f2OnAll if \code{TRUE} (not the default), \code{f2} is reran on all items of \code{x}
#' 	instead of only on the ones given illegal results from \code{f}
#' @return creates a list with two functions (\code{safe} and \code{unsafe}). The \code{unsafe} 
#'  is simply \code{f}, the \code{safe} version calls \code{f}, but for items that become 'illegal', 
#' 	\code{f2} is called. The class of the result is "unsafefunction"
#' @note Warnings during the calling of \code{f} are suppressed
#' @export illegalsCountered
illegalsCountered<-function(f, f2=constButWarnFunction(), f2OnAll=FALSE)
{
# 	tc<-match.call()
# 	print(tc)
	force(f)
	force(f2)
	force(f2OnAll)
	sf<-function(x){
# 		tc<-match.call()
# 		print(tc)
		rv<-suppressWarnings(f(x))
		ill<-newIllegals(rv,x)
		if(any(ill))
		{
			if(f2OnAll)
			{
				rv<-f2(x)
			}
			else
			{
				rv[ill]<-f2(x[ill])
			}
		}
		return(rv)
	}
	retval<-list(unsafe=f, safe=sf)
	class(retval)<-"unsafefunction"
	return(retval)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases illegalsCalculatedConstCountered
#' @method illegalsCalculatedConstCountered
#' @usage illegalsCalculatedConstCountered(f, smry=min, dflt=1e-10)
#' @return The class of the result is "unsafefunction", but now has structure that wil allow
#' 	to calculate the constant from the first set of x's passed along (see \code{getSafeFunction})
#' @export illegalsCalculatedConstCountered
illegalsCalculatedConstCountered<-function(f, smry=min, dflt=1e-10)
{
# 	tc<-match.call()
# 	print(tc)
	retval<-list(unsafe=f, smry=min, dflt=dflt)
	class(retval)<-"unsafefunction"
	return(retval)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases print.unsafefunction
#' @method print.unsafefunction
#' @usage \method{print}{unsafefunction}(x,...)
#' @return nothing
#' @S3method print unsafefunction
print.unsafefunction<-function(x, ...)
{
	catw("Unsafe function:")
	print(x$unsafe)
	if(exists("safe", x))
	{
		catw("Safe function:")
		print(x$safe)
	}
	else
	{
		catw("With constant correction based on default:", x$dflt, "and summary:")
		print(x$smry)
	}
	invisible()
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases getUnsafeFunction
#' @method getUnsafeFunction
#' @usage getUnsafeFunction(object)
#' @return function (a unsafe version of it - see \code{illegalsCalculatedConstCountered} or \code{illegalsCountered})
#' @export getUnsafeFunction
getUnsafeFunction<-function(object) UseMethod("getUnsafeFunction")
#' @rdname imputeDs2FitDs
#' 
#' @aliases getUnsafeFunction.default
#' @method getUnsafeFunction.default
#' @usage \method{getUnsafeFunction}{default}(object)
#' @return In this implementation, simply return \code{object}
#' @S3method getUnsafeFunction default
getUnsafeFunction.default<-function(object)
{
# 	tc<-match.call()
# 	print(tc)
	return(object)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases getUnsafeFunction.unsafefunction
#' @method getUnsafeFunction.unsafefunction
#' @usage \method{getUnsafeFunction}{unsafefunction}(object)
#' @return In this implementation, simply return \code{object$unsafe}
#' @S3method getUnsafeFunction unsafefunction
getUnsafeFunction.unsafefunction<-function(object)
{
# 	tc<-match.call()
# 	print(tc)
	return(object$unsafe)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases getSafeFunction
#' @method getSafeFunction
#' @usage getSafeFunction(object,x)
#' @return function (a safe version of it - see \code{illegalsCalculatedConstCountered} or \code{illegalsCountered})
#' @export getSafeFunction
getSafeFunction<-function(object,x) UseMethod("getSafeFunction")
#' @rdname imputeDs2FitDs
#' 
#' @aliases getSafeFunction.default
#' @method getSafeFunction.default
#' @usage \method{getSafeFunction}{default}(object,x)
#' @return In this implementation, simply return \code{object}
#' @S3method getSafeFunction default
getSafeFunction.default<-function(object,x)
{
# 	tc<-match.call()
# 	print(tc)
	return(object)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases getSafeFunction.unsafefunction
#' @method getSafeFunction.unsafefunction
#' @usage \method{getSafeFunction}{unsafefunction}(object,x)
#' @return In this implementation, simply return \code{object$safe} if it is present, or build one from the other properties
#' @S3method getSafeFunction unsafefunction
getSafeFunction.unsafefunction<-function(object,x)
{
# 	tc<-match.call()
# 	print(tc)
	if(exists("safe", object)) return(object$safe)
	cnst<-specialLegalX(object$unsafe, x, smry=object$smry, dflt=object$dflt)
	f2<-constButWarnFunction(cnst)
	f<-object$unsafe
	force(f2)
	force(f)
	sf<-function(x){
# 		tc<-match.call()
# 		print(tc)
		rv<-suppressWarnings(f(x))
		ill<-newIllegals(rv,x)
		if(any(ill))
		{
			rv[ill]<-f2(x[ill])
		}
		return(rv)
	}
	return(sf)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases illegals2Null
#' @method illegals2Null
#' @usage illegals2Null(f,x)
#' @return wrapper function around \code{f} that will return \code{NULL} if any of \code{f(x)} is turned into \code{NA} or \code{NaN}.
#' @export illegals2Null
illegals2Null<-function(f,x)
{
# 	tc<-match.call()
# 	print(tc)
	rv<-suppressWarnings(f(x))
	if(any(newIllegals(rv,x))) return(NULL)
	return(rv)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases constButWarnFunction
#' @method constButWarnFunction
#' @usage constButWarnFunction(cnst=0, warn=TRUE)
#' @param cnst constant that will be repeated as return value (defaults to 0)
#' @param warn if \code{TRUE} (default), each time this function is used, it will display a message
#' @return function that will return the right nr of repeats of the constant. Depending 
#' 	on the \code{warn} value, it will display a message that this occurred or not.
#' @export constButWarnFunction
constButWarnFunction<-function(cnst=0, warn=TRUE)
{
# 	tc<-match.call()
# 	print(tc)
	force(cnst)
	force(warn)
	rv<-function(x)
	{
# 		tc<-match.call()
# 		print(tc)
		if(length(x) > 0)
		{
			if(warn)
			{
				cat("\n\n************\n", curfnfinder(skipframes = 0, extraPrefPerLevel = "\n\t", retStack=TRUE), "\n")
				catw("Called constButWarnFunction with constant", cnst, "for values:", x)
				cat("************\n\n")
			}
		}
		rep(cnst, length(x))
	}
	return(rv)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases specialLegalX
#' @method specialLegalX
#' @usage specialLegalX(f, x, smry=min, dflt=1e-10)
#' @param smry summary function (like \code{min}, the default, or similar)
#' @param dflt if the calculated summary still fails, this value is taken
#' @return a single value that is either the calculated summary for the legal \code{x} and \code{f(x)}
#' 	or \code{dflt}.
#' @export specialLegalX
specialLegalX<-function(f, x, smry=min, dflt=1e-10)
{
# 	tc<-match.call()
# 	print(tc)
	ill<-newIllegals(f,x)
	x2<-removeIllegals(x[!ill])
	replc<-suppressWarnings(smry(x2))
	if(isIllegal(replc)) replc<-dflt
	return(replc)
}
#' @rdname imputeDs2FitDs
#' 
#' @aliases illegalToSmryLegalFunction
#' @method illegalToSmryLegalFunction
#' @usage illegalToSmryLegalFunction(f, smry=min, dflt=1e-10, warn=TRUE)
#' @return a function that is a wrapper around \code{f} which replaces illegal values with the return value
#' 	for the summary value of the legal \code{x}s.
#' @export illegalToSmryLegalFunction
illegalToSmryLegalFunction<-function(f, smry=min, dflt=1e-10, warn=TRUE)
{
# 	tc<-match.call()
# 	print(tc)
	force(f)
	force(smry)
	force(dflt)
	force(warn)
	rv<-function(x)
	{
# 		tc<-match.call()
# 		print(tc)
		ill<-newIllegals(f,x)
		if(any(ill))
		{
			newx<-specialLegalX(f, x, smry=smry, dflt=dflt)
			catwif(warn, "Called illegalToMinLegalFunction with constant", newx, "for values:", x[ill])
			x[ill]<-newx
		}
		return(f(x))
	}
	return(rv)
}