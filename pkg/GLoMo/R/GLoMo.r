#rcats will randomly (or if it is feasible, definedly) draw combinations
#of categories for the categorical variables, and note their relevance for
#each of the original rows.
#assumes dfr is wellformed, i.e.: only factors and numeric columns.
#also, all factors should be at the start of the columns (this may not be
#necessary anymore)

#note: see use of old version in full.glmnet.EM.fit
#note: see similar code in firstProcessingOfDfrForInitialFit.GLoMo

#known issues:
#need to check if there are no categorical columns
#if there are too many categorical columns, a subset of the possible combinations
#   is sampled, but the probabilities returned are the original ones
#a set of categorical values could be in the data.frame twice, if it can be 'attained'
#   from two original rows: note that this will then have different probabilities (!)
#Is it relevant to return the probability based on categoricals alone (probably yes)


 

#SECTION: non-exported helper functions




# .quickNumMatrix<-function(dfr) UseMethod(".quickNumMatrix")
# #need to check whether this is still any different from as.matrix.data.frame
# #in practice
# .quickNumMatrix.data.frame<-function(dfr){
# 	retval<-matrix(unlist(dfr), ncol=ncol(dfr))
# 	dimnames(retval)<-dimnames(dfr)
# 	return(retval)
# }
# .quickNumMatrix.numdfr<-function(dfr){return(as.matrix(dfr))}
# .quickNumMatrix.default<-function(dfr){return(as.matrix(dfr))}

# .matBack2OrgClass<-function(objWithClass, mat, catCols, levelList, colnms=NULL,
# 	verbosity=0) UseMethod(".matBack2OrgClass")
# 	
# .matBack2OrgClass.data.frame<-function(objWithClass, mat, catCols, levelList,
# 	colnms=NULL, verbosity=0)
# {
# 	.mat2dfr(mat=mat, catCols=catCols, levelList=levelList, colnms=colnms,
# 		verbosity=verbosity)
# }
# 
# .matBack2OrgClass.numdfr<-function(objWithClass, mat, catCols, levelList,
# 	colnms=NULL, verbosity=0)
# {
# 	posInCatCols<-match(seq(ncol(mat)), catCols, nomatch=0)
# 	allLevels<-lapply(posInCatCols, function(ccn){
# 			if(ccn > 0) return(levelList[[ccn]]) else return(character(0))
# 		})
# 	colnames(mat)<-colnms
# 	names(allLevels)<-colnms
# 	retval<-list(mat=mat, lvls=allLevels)
# 	class(retval)<-"numdfr"
# 	return(retval)
# }


.hasNA<-function(dfr)
{
	return(any(is.na(dfr)))
}

.contDataAsMat<-function(dfr){
	catcols<-findCatColNums(dfr)
	return(as.nummatrix(dfr[,-catcols]))
}



#END OF SECTION: non-exported helper functions






#optimization: best option is to look at mat2dfr... see comments there.
#   This is only relevant for data.frame...
rCatsInDfr<-function(dfr, maxFullNACatCols=6, howManyIfTooMany=1000,
	onlyCategorical=FALSE, weightsName="weights", orgriName="orgri",
	reweightPerRow=FALSE, verbosity=0,...)
{
	catCols<-findCatColNums(dfr)
	ordCols<-rep(FALSE, ncol(dfr))
	ordCols[findOrderedColNums(dfr)]<-TRUE
	dfrl<-dfr[,catCols, drop=FALSE]
	if(onlyCategorical)
	{
		dfr<-dfrl
		catCols<-seq(length(catCols))
	}
	orgnames<-colnames(dfr)
	catwif(verbosity>0, "find marginal probabilities")
	probs<-marginalProbPerCat(dfrl)#one item for every categorical column

	toAddCols<-NULL
	if((!is.null(weightsName)) && (nchar(weightsName)>0))
	{
		toAddCols<-c(toAddCols, weightsName)
	}
	if((!is.null(orgriName)) && (nchar(orgriName)>0))
	{
		toAddCols<-c(toAddCols, orgriName)
	}

	naLevels<-allLevels(dfrl)
	for(i in seq_along(probs))
	{
		unusedLevels<-(probs[[i]] == 0)
		if(sum(unusedLevels) > 0)
		{
			catwif(verbosity > 0, "Unused levels detected in column", names(naLevels)[i], ".")
			catwif(verbosity > 0, "Unused levels are:")
			printif(verbosity > 0, naLevels[[i]][unusedLevels])
			catwif(verbosity > 0, "Better to remove these up front; will try to ignore them now.")
			naLevels[[i]]<-naLevels[[i]][!unusedLevels]
			probs[[i]]<-probs[[i]][!unusedLevels]
		}
	}
	dfr<-as.nummatrix(dfr)
	catwif(verbosity>0, "dfr is now a matrix of dimension:", dim(dfr), "and class",
		class(dfr))
	catwif(verbosity>0, "while dfrl now has class", class(dfrl), "and dimension:",
		dim(dfrl))
	#so, from here on, dfr is a matrix!!! while dfrl only holds the categorical 
	#   cols but still has the same class as dfr originally had!
	
	naLevelNums<-lapply(naLevels, function(curlvls){seq(length(curlvls))})
	catwif(verbosity>0, "start producing new rows")
	newrows<-lapply(seq(nrow(dfrl)), function(ri)
	{
		catwif(verbosity>1, "row", ri, "/", nrow(dfrl))
		currow<-dfrl[ri,,drop=TRUE]
		curnas<-which(is.na(currow))
		if(length(curnas) > maxFullNACatCols)
		{
			catwif(verbosity>1, "too many categoricals missing")
			#if these are 3-level categories, this means already >= 2187 possible 
			#combinations in this case, we draw howManyIfTooMany random ones from the
			#marginal multinomials
			stopifnot(howManyIfTooMany > 0)
			if(is.null(weightsName) || (nchar(weightsName)==0))
			{
				catwif(verbosity>1, "no need to calculate weights")
				catvals<-sapply(curnas, function(ci){
						sample.int(length(naLevels[[ci]]), howManyIfTooMany, replace=TRUE,
							prob=probs[[ci]])
					})
				if(length(curnas)==1)
				{
					catvals<-matrix(catvals, ncol=1)
				}
				else if(howManyIfTooMany==1)
				{
					catvals<-matrix(catvals, nrow=1)
				}
				catCombProbs<-rep(1, howManyIfTooMany)
			}
			else
			{
				catwif(verbosity>1, "needed to calculate weights")
				catAll<-lapply(curnas, function(ci){
						rv<-sample.int(length(naLevels[[ci]]), howManyIfTooMany,
							replace=TRUE, prob=probs[[ci]])
						relvpr<-probs[[ci]][rv]
						return(list(val=rv, pr=relvpr))
					})
				catvals<-sapply(catAll, '[[', "val")
				catMargprobs<-sapply(catAll, '[[', "pr")
				if(length(curnas)==1)
				{
					catvals<-matrix(catvals, ncol=1)
					catMargprobs<-matrix(catMargprobs, ncol=1)
				}
				else if(howManyIfTooMany==1)
				{
					catvals<-matrix(catvals, nrow=1)
					catMargprobs<-matrix(catMargprobs, nrow=1)
				}
				catCombProbs<-unlist(apply(catMargprobs, 1, prod))
				if(reweightPerRow)
				{
					catCombProbs<-catCombProbs/sum(catCombProbs)
					#make the weights sum to 1
				}
			}
		}
		else if(length(curnas) > 0)
		{
			catwif(verbosity>1, "few categoricals missing")
			catvals<-dfr2mat(expand.grid(naLevelNums[curnas]))
			if((!is.null(weightsName)) && (nchar(weightsName)>0))
			{
				catwif(verbosity>1, "needed to calculate weights")
				catMargprobs<-expand.grid(probs[curnas])
				catwif(verbosity>1, "will generate", nrow(catMargprobs), "observations")
				catCombProbs<-apply(catMargprobs, 1, prod)
			}
		}
		if(length(curnas) > 0)
		{
			retval<-dfr[rep(ri, length(catCombProbs)),, drop=FALSE]
			retval[,catCols[curnas]]<-catvals
		}
		else
		{
			catwif(verbosity>1, "no categoricals missing")
			catCombProbs<-1
			retval<-dfr[ri,, drop=FALSE]
		}

		if(((!is.null(weightsName)) && (nchar(weightsName)>0)) ||
			((!is.null(orgriName)) && (nchar(orgriName)>0)))
		{
			toAdd<-rep(ri, length(catCombProbs))
			catwif(verbosity>1, "need to add either weight or orgri")
			if((!is.null(weightsName)) && (nchar(weightsName)>0))
			{
				catwif(verbosity>1, "need to add weight")
				toAdd<-catCombProbs
				if((!is.null(orgriName)) && (nchar(orgriName)>0))
				{
					catwif(verbosity>1, "need to add orgri too")
					toAdd<-cbind(toAdd, ri)
				}
			}
			else
			{
				#now we know ((!is.null(orgriName)) && (nchar(orgriName)>0))
				catwif(verbosity>1, "need to add orgri")
				toAdd<-ri
			}
			retval<-cbind(retval, toAdd)
			colnames(retval)<-c(orgnames, toAddCols)
		}
		return(retval)
	})
	catwif(verbosity>1, "combine the rows")
	resmat<-do.call(rbind, newrows)
	
	catwif(verbosity>1, "turn resulting matrix into ", class(dfrl), " again")
	result<-matBack2OrgClass(dfrl, mat=resmat, catCols=catCols, levelList=naLevels,
		ord=c(ordCols, rep(FALSE, length(toAddCols))), colnms=c(orgnames, toAddCols), verbosity=verbosity-1)
	return(result)
}

rCatsAndCntInDfr<-function(dfr, maxFullNACatCols=6, howManyIfTooMany=1000,
	weightsName="weights", orgriName="orgri", reweightPerRow=FALSE, verbosity=0,...)
{
	catCols<-findCatColNums(dfr)
	numCols<-ncol(dfr)
	contCols<-(seq(numCols))[-catCols]

	#we expect that the original dfr will be smaller, so we fill the continuous 
	#columns first, since they are filled with the mean anyway
	cln<-0
	for(i in contCols)
	{
		cln<-cln+1
		catwif(verbosity>0, "continuous column (", i, "):", cln, "/",
			length(contCols))
		curcol<-dfr[,i,drop=TRUE]
		wherenas<-which(is.na(curcol))
		if(length(wherenas) > 0)
		{
			colm<-mean(curcol, na.rm=TRUE)
			dfr[wherenas,i]<-colm
		}
	}

	catwif(verbosity>0, "categorical columns")
	retval<-rCatsInDfr(dfr=dfr, maxFullNACatCols=maxFullNACatCols,
		howManyIfTooMany=howManyIfTooMany, onlyCategorical=FALSE,
		weightsName=weightsName, orgriName=orgriName, reweightPerRow=reweightPerRow,
		verbosity=verbosity-1,...)
		
	return(retval)
}


#Altered this from fitPredictorModel.GLoMo. Turned loop code into matrix 
#operations; known issue: what if there are no/all factor columns??
#
#IMPORTANT CHANGE: we foresee that most of the times, there will be only one
#observation per cell (i.e.: per mid).
#If this is the case, use the unpooled (co)variance matrix. This will always
#be an overestimate of the true pooled(co)variance matrix! Because this will
#only create more variance in the generated predictors, this should not affect
#the results negatively.
#A possible result is that the algorithm may converge somewhat slower (as more
#'different' data is generated. For relatively few missing values, this should
#not be an issue (especially for few missing continuous values)
#
#Because it is rather hard to check (especially for all covariances) whether
#the above is the case, we will (for now?) always use the global covmat.
# -> arranged by parameter pooledCov
#
#Food for thought: does it still make sense to use the weights then??

GLoMo<-function(dfr, weights=rep(1,dim(dfr)[1]), uniqueIdentifiersPerRow=NULL,
	separator=",", pooledCov=TRUE, verbosity=0,...)
{
	stopifnot(!.hasNA(dfr))
	numCols<-ncol(dfr)
	factorCols<-findCatColNums(dfr)
	numCat<-length(factorCols)
	stopifnot(numCols!=numCat)
	stopifnot(numCat>0)
	dfrDim<-dim(dfr)
	catwif(verbosity > 0, class(dfr), "(", dfrDim, "), nr of factorCols=",
		length(factorCols))
	nobs<-dfrDim[1]
	numCont<-dfrDim[2] - numCat

	if(is.null(uniqueIdentifiersPerRow))
	{
		catwif(verbosity > 0,
			"uniqueIdentifiersPerRow were not provided so recalculating.")
		require(addendum)
		uniqueIdentifiersPerRow<-categoricalUniqueIdentifiers(dfr,
			separator=separator, na.becomes=NA)
	}
	catwif(verbosity > 0, "uniqueIdentifiersPerRow OK")

	weights<-weights/sum(weights) #make them sum to 1

	catwif(verbosity > 0, "Unique uniqueIdentifiersPerRow.")
	uids<-uniqueCharID(uniqueIdentifiersPerRow, needSort=TRUE,
		includeOccurrence=FALSE, impNr=1)
	catwif(verbosity > 0, length(uids), "found.")
	firstOccurrenceOfEachUMidInDfr<-match(uids, uniqueIdentifiersPerRow)
	
	uniqueFactorCombinationsAndContinuousMeans<-dfr[firstOccurrenceOfEachUMidInDfr,]
	
#	factuid<-dfr[firstOccurrenceOfEachUMidInDfr, factorCols]
#holds the factor variables for each unique mid
#	colnames(factuid)<-colnames(dfr)[factorCols]
	
	#note:
	#uniqueIdentifiersPerRow holds the unique identifiers for every row in the dfr
	#uids holds the unique values!

	catwif(verbosity > 0, "Find the pihat + means")
	#2011/07/14: changed to matrix operations!
	#2011/08/10: changed to sparse matrix operations!
	rowinds<-match(uniqueIdentifiersPerRow, uids)
	colinds<-seq_along(uniqueIdentifiersPerRow)
	matw<-sparseMatrix(i=rowinds,
		j=colinds, x=weights) #note: we are certain all uniqueIdentifiersPerRow and all uids are in the data, so no need to specify dims
	contDataAsMat<-.contDataAsMat(dfr)
	pihat<-rowSums(matw)
	#2011/10/25 for the below to work, we must use _re_weighted per UID, i.e. per row!!
	urs<-unique(rowinds)
	weightsums<-sapply(urs, function(curr){sum(weights[rowinds==curr])})
	mapri2u<-match(rowinds, urs)
	rweights<-weights/weightsums[mapri2u]
	matw<-sparseMatrix(i=rowinds,
		j=colinds, x=rweights) #note: we are certain all uniqueIdentifiersPerRow and all uids are in the data, so no need to specify dims
	themeans<-as.matrix(matw %*% contDataAsMat)
	
	contCols<-seq(ncol(dfr))[-factorCols]
	contnames<-colnames(dfr)[contCols]
	uniqueFactorCombinationsAndContinuousMeans[,contCols]<-themeans

#	umeans<-cbind(pihat=pihat, themeans)
#	#umeans should now hold 1 row for every unique mid
#	#it should hold the pihats as the first col, and the means for every cont var
#	colnames(umeans)<-c("pihat", contnames)

	if(! pooledCov)
	{
		stop("GLoMo for non-pooled covariance matrix has not been implemented yet.")
	}
	catwif(verbosity > 0, "Getting centralized covariances")
	omegahat<-cov.wt(contDataAsMat, wt=weights, method="ML")$cov
	dimnames(omegahat)<-list(rowvar=contnames, colvar=contnames)
	catwif(verbosity > 0, "Got centralized covariances")

	#the conversion to data.frame is perhaps unnecessary and may be a performance
	#hog in the case of numdfr... -> check to improve on this
	#2011/08/24: changed the function so that this conversion no longer takes place.
	#now, uniqueFactorCombinationsAndContinuousMeans has the same class as dfr had.
	#note: keep in sync with combineGLoMos
	retval<-list(uid=uids, pihat=pihat, omegahat=omegahat, orgdatadim=dim(dfr),
		uniqueFactorCombinationsAndContinuousMeans=uniqueFactorCombinationsAndContinuousMeans,
		factorCols=factorCols, guidSeparator=separator, invomega=invertSymmetric(omegahat, careful=FALSE))
	class(retval)<-"GLoMo"
	return(retval)
}

getGuidData<-function(glomo, dfr, guidPerObservation=NULL, whichHaveMissingCat,
	verbosity=0)
{
	if(is.null(guidPerObservation))
	{
		catwif(verbosity>0, "guidPerObservation was not passed along")
		require(addendum)
		guidPerObservation<-categoricalUniqueIdentifiers(dfr, 
			separator=glomo$guidSeparator, na.becomes="\\d+", verbosity=verbosity-1)
	}
	else
	{
		stopifnot(length(guidPerObservation)==nrow(dfr))
	}
	#we expect match to be a lot faster + avoid lapply
	if(missing(whichHaveMissingCat)) whichHaveMissingCat<-grepl("\\d+", guidPerObservation, fixed=TRUE)

	orgWithMissing<-lapply(guidPerObservation[whichHaveMissingCat], grep, glomo$uid)
	orgWithoutMissing<-as.list(match(guidPerObservation[! whichHaveMissingCat], glomo$uid))
	possibleGlomoGuidPerObs<-list(length(guidPerObservation))
	#not certain if the below assignments will work...
	possibleGlomoGuidPerObs[whichHaveMissingCat]<-orgWithMissing
	possibleGlomoGuidPerObs[!whichHaveMissingCat]<-orgWithoutMissing
	names(possibleGlomoGuidPerObs)<-names(guidPerObservation)

	retval<-list(guidPerObservation=guidPerObservation,
		possibleGlomoGuidPerObs=possibleGlomoGuidPerObs,
		separator=glomo$guidSeparator)
	class(retval)<-"GuidData"
	return(retval)
}


#is only called from within predict.GLoMo to handle the unlikely case
#of a row that has no matching row(s) in the GLoMo uids
#Note: this really only is unlikely in the setting that I plan to use it in,
#i.e.: impute in the original data from a GLoMo fit to it.
randomFillAndRepeatDataRow<-function(currow, obsneeded, levelslist, newdata)
{
	fcols<-which(sapply(levelslist, length) > 0)
	curnacols<-which(is.na(currow))
	retval<-currow[rep(1, obsneeded),]
	for(curnacol in curnacols)
	{
		if(curnacol %in% fcols)
		{
			#pick a completely random level
			retval[,curnacol]<-sample.int(length(levelslist[[curnacol]]),
				size=obsneeded, replace=TRUE)
		}
		else
		{
			#whatever: unweighted mean and sd
			coldata<-newdata[,curnacol, drop=TRUE]
			uwmean<-mean(coldata, na.rm=TRUE)
			uwsd<-sd(coldata, na.rm=TRUE)
			retval[,curnacol]<-rnorm(obsneeded, mean=uwmean, sd=uwsd)
		}
	}
	return(retval)
}

reusableDataForGLoMoSampling<-function(glomo, dfr, forrows=seq(nrow(dfr)),
	guiddata=NULL, verbosity=0)
{
	if(is.null(guiddata) | (!is(guiddata, "GuidData")))
	{
		catwif(verbosity > 0,
			"guiddata were not (completely) provided so recalculating.")
		guiddata<-getGuidData(glomo, dfr, guidPerObservation=guiddata)
#		cattif(verbosity > 10, "predict.GLoMo: guiddata str: ")
#		if(verbosity > 10) str(guiddata)
	}
	catwif(verbosity > 0, "getting reusable data.")
	catcols<-glomo$factorCols
	numCat<-length(catcols)
	cntcols<-seq(ncol(dfr))[-catcols]
	numCont<-length(cntcols)
	
	perrow<-lapply(seq_along(forrows), function(i){
			currowi<-forrows[i]
			#note: improvement possible: if no missing values in a row then these
			#calculations are not needed, just return an empty list
			if((length(forrows) > 1) & (verbosity > 1))
			{
				catw("working on row", match(currowi, forrows), "/", length(forrows))
			}
			currow<-dfr[currowi,]
			glomorowsforcurrow<-guiddata$possibleGlomoGuidPerObs[[currowi]]
			whichCntColNotNA<-which(!is.na(currow[, cntcols, drop=TRUE]))
			whichCntColNA<-(1:numCont)[-whichCntColNotNA]
			presentCntColsInDfr<-cntcols[whichCntColNotNA]
			missingCntColsInDfr<-cntcols[whichCntColNA]
			if(verbosity > 5)
			{
				catw("currowi:", currowi)
				catw("glomorowsforcurrow:", glomorowsforcurrow)
				catw("whichCntColNotNA:", whichCntColNotNA)
				catw("whichCntColNA:", whichCntColNA)
				catw("presentCntColsInDfr:", presentCntColsInDfr)
				catw("missingCntColsInDfr:", missingCntColsInDfr)
			}
			if(length(whichCntColNotNA) > 0)
			{
				omega<-glomo$omegahat[whichCntColNotNA,whichCntColNotNA, drop=FALSE]

				#note in the 1,2 notation, 1 refers to missing data (NA), while 2 refers
				#to present data (notNA)
				if((length(whichCntColNotNA) == numCont) & (exists("invomega", glomo)))
				{
					invSig22<-glomo$invomega
					catwif(verbosity > 1, "Using glomo$invomega of dimension", dim(invSig22))
				}
				else
				{
					invSig22<-invertSymmetric(omega, careful=FALSE)
					catwif(verbosity > 1, "Inverted omega of dimension", dim(invSig22))
				}
				
				presentXs<-matrix(unlist(currow[, presentCntColsInDfr, drop=TRUE]), nrow=1)
				if(length(whichCntColNA) != numCont)
				{
					sig11<-glomo$omegahat[whichCntColNA,whichCntColNA, drop=FALSE]
					sig12<-glomo$omegahat[whichCntColNA,whichCntColNotNA, drop=FALSE]
					a<-currow[,presentCntColsInDfr, drop=TRUE]
					useSigma<-sig11 - sig12 %*% invSig22 %*% t(sig12)
					sigLeft<-sig12 %*% invSig22
				}
				else
				{
					a<-NULL
					useSigma<-glomo$omegahat
					sigLeft<-NULL
				}
			}

			if(length(glomorowsforcurrow) > 1)
			{
				if(length(whichCntColNotNA) == 0)
				{
					#if all continuous values are missing, no need for conditional on them
					deltas<-glomo$pihat[glomorowsforcurrow]
				}
				else
				{
					deltas<-sapply(glomorowsforcurrow, function(curcatrow){
							pi.c<-glomo$pihat[curcatrow]
							relvMus<-matrix(unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[
								curcatrow,presentCntColsInDfr,drop=TRUE]), ncol=1)
							#relvMus contains for each relevant cell and for each continuous column with missing data
							# the mean value.
							#Note: typically, in the first iteration of an EMLasso, these will all be equal, as the 
							#missing values are imputed with the marginal mean
							
# 							if(verbosity > 5)
# 							{
# 								catw("dimension and class of invSig22:", dim(invSig22), "->", class(invSig22))
# 								catw("dimension and class of relvMus:", dim(relvMus), "->", class(relvMus))
# 								catw("dimension and class of presentXs:", dim(presentXs), "->", class(presentXs))
# # 								.debug.tmp$invSig22<<-invSig22
# # 								.debug.tmp$relvMus<<-relvMus
# # 								.debug.tmp$presentXs<<-presentXs
# 							}
							partr<-invSig22 %*% relvMus         #notation refers to near page
							part1<-presentXs %*% partr        #349 in Analysis of Incomplete
							part2<- -1/2*t(relvMus) %*% partr   #Multivariate Data
							delta.c<-part1+part2+log(pi.c)
							return(delta.c)
						})
					#apparantly, occasionally these deltas are _really_ big, which
					#prevents the exp from working.
					#solution: subtract a common term from all of them (= amounts to 
					#   dividing them by a common factor after exponentiation)
					maxd<-max(deltas)
					if(maxd > 100)
					{
						subtr<-maxd-100
						deltas<-deltas - subtr
					}
					#Note: if all relvMus were equal, then so will all part1 and part2,
					#so: every individual delta is really a constant times pi.c
					deltas<-exp(deltas) #p349 in Analysis of Incomplete Multivariate Data
				}
				deltasum<-sum(deltas)
				probs<-deltas/deltasum
				#Note if all relvMus were equal, then the common factor is divided away
				#so probs are simply rescaled versions of pi.c (the cell probabilities)
				#so that they sum to 1
			}
			else
			{
				probs<-NULL
			}

			retval<-list(
				a=a,                   #notation refers to near page 349 in
				useSigma=useSigma,     #Analysis of Incomplete Multivariate Data
				sigLeft=sigLeft,       #
				probs=probs,
				whichCntColNotNA=whichCntColNotNA,
				whichCntColNA=whichCntColNA,
				presentCntColsInDfr=presentCntColsInDfr,
				missingCntColsInDfr=missingCntColsInDfr
			)
			class(retval)<-"ReusableDataForGLoMoSamplingForOneRow"
			return(retval)
		})
	retval<-list(guiddata=guiddata, forrows=forrows, perrow=perrow)
	class(retval)<-"ReusableDataForGLoMoSampling"
	return(retval)
}

#If newdata is NULL, nobs means the number of observations to predict (generate)
#if not, it holds the number of observations to generate per original 
#observation that holds missing data
predict.GLoMo<-function(object, nobs=1, newdata=NULL, forrows=seq(nrow(newdata)),
	reusabledata=NULL, returnRepeats=FALSE, returnSelectedGlomoRows=FALSE,
	verbosity=0,...)
{
	#NOTE: the combinations of categorical values in GLoMo are always unique!
	glomo<-object #to make it more recognizable in the following code
	if(is.null(newdata) & (!is.null(reusabledata)))
	{
		warning("Don't provide reusabledata if newdata is NULL. It will be ignored.")
		reusabledata<-NULL #just in case
	}
	cntcols<-seq(ncol(newdata))[-glomo$factorCols]
	if(is.null(newdata))
	{
		catwif(verbosity > 1, "_fully_ predicting dataset.")
		probs<-glomo$pihat
		howofteniseachglomorowsampled<-as.vector(rmultinom(1, nobs, prob=probs))
		glomorowsforcurrow<-which(howofteniseachglomorowsampled > 0)
		howofteniseachglomorowsampled<-howofteniseachglomorowsampled[glomorowsforcurrow]
		glomorowschosen<-rep(glomorowsforcurrow, howofteniseachglomorowsampled)
		retval<-glomo$uniqueFactorCombinationsAndContinuousMeans[glomorowschosen,]
		firstposofeachglomorowinresult<-cumsum(c(1, howofteniseachglomorowsampled))[
			-(length(howofteniseachglomorowsampled)+1)]
		lastposofeachglomorowinresult<-cumsum(howofteniseachglomorowsampled)
		for( i in seq_along(glomorowsforcurrow))
		{
			catwif(verbosity > 2,
				"working on glomorow", i, "/", length(glomorowsforcurrow))
			curglomorowi<-glomorowsforcurrow[i]
			howmanysamplesforcurglomorow<-howofteniseachglomorowsampled[i]
			useMu<-unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[
				curglomorowi,cntcols, drop=TRUE])
			gen<-qrmvnorm(howmanysamplesforcurglomorow, mean=useMu,sigma=glomo$omegahat)
			allposinresforcurglomorow<-seq(from=firstposofeachglomorowinresult[i],
				to=lastposofeachglomorowinresult[i])
			retval[allposinresforcurglomorow, cntcols]<-gen
		}
		#note: in this case, returnRepeats has no meaningful interpretation
		if(returnSelectedGlomoRows)
		{
			return(list(predicted=retval, glomorowsused=glomorowschosen))
		}
		else
		{
			return(retval)
		}
	}
	#now we also check that the class of the newdata matches the glomo!!
	#if it doesn't, transform it.
	if(any(class(newdata) != class(glomo$uniqueFactorCombinationsAndContinuousMeans)))
	{
		warning("Unmatching classes between glomo and dfr. Will try to coerce dfr.")
		if(inherits(glomo$uniqueFactorCombinationsAndContinuousMeans, "data.frame"))
		{
			catwif(verbosity > 1, "coercing newdata to data.frame.")
			newdata<-as.data.frame(newdata)
		}
		else if(inherits(glomo$uniqueFactorCombinationsAndContinuousMeans, "numdfr"))
		{
			catwif(verbosity > 1, "coercing newdata to numdfr.")
			newdata<-numdfr(newdata)
		}
	}
	if(length(nobs) != length(forrows))
	{
		if(length(nobs) != 1) stop("Unsupported nobs passed along.")
		catwif(verbosity > 0, "readjusting nobs.")
		nobs<-rep(nobs, length(forrows))
	}
	if(is.null(reusabledata) | (!is(reusabledata, "ReusableDataForGLoMoSampling")))
	{
		catwif(verbosity > 0,
			"reusabledata were not (completely) provided so recalculating.")
		reusabledata<-reusableDataForGLoMoSampling(glomo=glomo, dfr=newdata,
			forrows=forrows, guiddata=reusabledata, verbosity=verbosity-1)
	}
	levelslist<-allLevels(newdata)
	predPerRow<-lapply(forrows, function(currowi){
			howManiethRow<-match(currowi, forrows)
			if((length(forrows) > 1) & (verbosity > 1))
			{
				catw("working on row", howManiethRow, "/", length(forrows))
			}
			currow<-newdata[currowi, ]
			indexInReusableData<-match(currowi, reusabledata$forrows)
			curreusabledata<-reusabledata$perrow[[indexInReusableData]]
			curguiddata<-reusabledata$guiddata$possibleGlomoGuidPerObs[[
				indexInReusableData]]
			if(sum(is.na(currow)) == 0)
			{
				catwif(verbosity > 1,
					"no data was missing in the original row (", currowi, ").")
				retval<-newdata[currowi, ]
				if(returnSelectedGlomoRows)
				{
					catwif(verbosity > 5, "will use as glomorowsused:", curguiddata)
					return(list(predicted=retval, glomorowsused=curguiddata))
					#note, in this case, curreusabledata should hold only one value!
				}
				else
				{
					return(retval)
				}
			}
			#which o/t rows in the pimeanhat can be chosen is stored in catrowsallowed
			glomorowsforcurrow<-reusabledata$guiddata$possibleGlomoGuidPerObs[[currowi]]
			howmanysamplesforcurrow<-nobs[howManiethRow]
			if(length(glomorowsforcurrow) == 0)
			{
				#in fact this should never happen
				catwif(verbosity > 1, "row has no matching glomorows.")
				warning(paste("predict.GLoMo: Row passed along for which there are no valid predictions. There is no matching combination of categories in the GLoMo object. The rownumber was", currowi, ". Will simply pick random values."))
				retval<-randomFillAndRepeatDataRow(currow=currow, 
					obsneeded=howmanysamplesforcurrow, levelslist=levelslist,
					newdata=newdata)
				if(returnSelectedGlomoRows)
				{
					catwif(verbosity > 5, "will use as glomorowsused:", character(0))
					return(list(predicted=retval, glomorowsused=character(0)))
				}
				else
				{
					return(retval)
				}
			}
			else if(length(glomorowsforcurrow) == 1)
			{
				catwif(verbosity > 1, "row has 1 matching glomorow.")
				howofteniseachglomorowsampled<-howmanysamplesforcurrow
			}
			else
			{
				catwif(verbosity > 1, "row has multiple matching glomorow.")
				probs<-curreusabledata$probs #these probabilities are conditional on the
					#data that is present in currow (categorical AND continuous)
				catwif(verbosity > 5, "their probabilities are: ", probs)
				catwif(verbosity > 5,
					"and we need: ", howmanysamplesforcurrow, "samples.")
				howofteniseachglomorowsampled<-as.vector(rmultinom(1,
					howmanysamplesforcurrow, prob=probs))
			}
			#by now, howofteniseachglomorowsampled holds how many times each of the 
			#rows indicated by glomorowsforcurrow are selected
			#we remove unselected rows from both:
			glomorowsforcurrow<-glomorowsforcurrow[howofteniseachglomorowsampled>0]
			howofteniseachglomorowsampled<-howofteniseachglomorowsampled[
				howofteniseachglomorowsampled>0]
			#now we get a vector of all the (possibly repeated) row nrs in pimeanhat
			glomorowschosen<-rep(glomorowsforcurrow, howofteniseachglomorowsampled)
#			cattif(verbosity > 5, "	predict.GLoMo: glomorowschosen:", glomorowschosen)
			#And a starting point for the return values
			retval<-glomo$uniqueFactorCombinationsAndContinuousMeans[glomorowschosen,]
#			cattif(verbosity > 5, "	predict.GLoMo: retval structure for now:")
#			if(verbosity > 5) str(retval)
			#if no continuous data missing, we always use the values in original row:
			cntcols<-seq(ncol(newdata))[-glomo$factorCols]
#			cattif(verbosity > 5, "	predict.GLoMo: cont col indexes in dfr:", cntcols)
			if(sum(is.na(currow[,cntcols, drop=TRUE])) == 0)
			{
				catwif(verbosity > 1, "no missing continuous data!")
				orgcont<-currow[rep(1, howmanysamplesforcurrow),cntcols]
#				if(verbosity > 5)
#				{
#					catt("orgcont str:")
#					str(orgcont)
#					catt("retval str:")
#					str(retval)
#					catt("cntcols: ", cntcols)
#				}
				retval[,cntcols]<-orgcont #replace all continuous values with original
#				catwif(verbosity > 1, "passed no missing continuous data!")
			}
			else
			{
				#We also need to sample continuous values (conditional on the
				#categoricals) if we get here.
				#First, fill out the values that will not change (i.e. that were present
				#in the original row)
				retval[,curreusabledata$presentCntColsInDfr]<-currow[
					rep(1, howmanysamplesforcurrow),curreusabledata$presentCntColsInDfr]
				firstposofeachglomorowinresult<-cumsum(c(1, howofteniseachglomorowsampled))[
					seq_along(howofteniseachglomorowsampled)]
				lastposofeachglomorowinresult<-cumsum(howofteniseachglomorowsampled)
				for( i in seq_along(glomorowsforcurrow))
				{
					catwif(verbosity > 2, "working on glomorow", i, "/",
						length(glomorowsforcurrow))
					curglomorowi<-glomorowsforcurrow[i]
					howmanysamplesforcurglomorow<-howofteniseachglomorowsampled[i]
					catwif(verbosity > 5, "Need", howmanysamplesforcurglomorow, "samples")
					useMu<-unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[
						curglomorowi,cntcols, drop=TRUE])
# 					#TMPNS
# 					catw("Original useMu:")
# 					print(useMu)
# 					#TMPNS
					if(length(curreusabledata$whichCntColNA) != length(cntcols))
					{
						#so part of the continuous data is already present
						#	(and reused in each sample)
						mu1<-useMu[curreusabledata$whichCntColNA]
						mu2<-useMu[curreusabledata$whichCntColNotNA]
						useMu<-unlist(mu1 + as.vector(curreusabledata$sigLeft %*%
							matrix(unlist(curreusabledata$a - mu2), ncol=1)))
# 						#TMPNS
# 						catw("sigleft:")
# 						print(curreusabledata$sigLeft)
# 						catw("a-mu2:")
# 						print(matrix(unlist(curreusabledata$a - mu2), ncol=1))
# 						#TMPNS
						#note: a is the value in the row, mu2 is the mean from the GLoMo. It is common in the 
						#first iteration of an EMLasso that these are the same (as missing values are imputed
						#with the mean).
# 						#TMPNS
# 						catw("Adapted useMu (to already present continuous data):")
# 						print(useMu)
# 						#TMPNS
					}
					catwif(verbosity > 2,
						"Past parameter calculation. Will now generate conditional normal.")
					gen<-qrmvnorm(howmanysamplesforcurglomorow, mean=useMu,
						sigma=curreusabledata$useSigma) #normal, each row = one simulation
# 					#TMPNS
# 					catw("Generated normal data")
# 					print(gen)
# 					#TMPNS
					allposinresforcurglomorow<-seq(from=firstposofeachglomorowinresult[i],
						to=lastposofeachglomorowinresult[i])
					retval[allposinresforcurglomorow,
						curreusabledata$missingCntColsInDfr]<-gen
				}
			}
			if(returnSelectedGlomoRows)
			{
				catwif(verbosity > 5, "will use as glomorowsused:", glomorowschosen)
				return(list(predicted=retval, glomorowsused=glomorowschosen))
			}
			else
			{
				return(retval)
			}
		})
	#debug.predPerRow<<-predPerRow
	#now recombine the 'subdatasets' in predPerRow, just like it's done at
	#the end of rCatsInDfr
	if(returnSelectedGlomoRows)
	{
		result<-combineSimilarDfrList(lapply(predPerRow, "[[", "predicted"))
		#this should be OK for numdfr, but probably slow for data.frame
		glomorowsused<-do.call(c, lapply(predPerRow, "[[", "glomorowsused"))
	}
	else
	{
		result<-combineSimilarDfrList(predPerRow)
		#this should be OK for numdfr, but probably slow for data.frame
	}
	if(!is.null(rownames(predPerRow)))
	{
		rownames(predPerRow)<-postfixToMakeUnique(rownames(predPerRow))
	}
	if(returnRepeats)
	{
		numRepPerRow<-sapply(seq_along(forrows), function(curi){
				currow<-newdata[forrows[curi], ]
				if(sum(is.na(currow)) == 0) return(1) else return(nobs[curi])
			})
		names(numRepPerRow)<-as.character(forrows)
		retval<-list(predicted=result, numRepPerRow=numRepPerRow)
		if(returnSelectedGlomoRows) retval$glomorowsused<-glomorowsused
		return(retval)
	}
	if(returnSelectedGlomoRows)
	{
		return(list(predicted=result, glomorowsused=glomorowsused))
	}
	return(result)
}

#will have to see whether this really provides a speedup
#maybe doing the grep again with the reduced uids will be faster than this
updateGuidData<-function(oldglomo, newglomo, oldrowsused=seq(nrow(oldglomo$uid)),
	oldguiddata)
{
	oldrowsused<-unique(oldrowsused)
	oldtonew<-sapply(oldglomo$uid[oldrowsused], function(curolduid){
			match(curolduid, newglomo$uid)
		})

	oldguiddata$possibleGlomoGuidPerObs<-lapply(oldguiddata$possibleGlomoGuidPerObs,
		function(oldrows){
			oldtonew[match(oldrows, oldrowsused)]
		})
	return(oldguiddata)
}

validateFunction.acceptall<-function(attempts, otherData, forrow, verbosity=0)
{
	return(seq(nrow(attempts$predicted)))
}

validateFunction.useprob<-function(attempts, otherData, forrow, verbosity=0)
{
	theprob<-otherData[forrow]
	if(is.null(theprob) || (theprob < 0) || (theprob > 1))
	{
		catwif(verbosity > 0, "invalid probability for row", forrow, ". Using 50%.")
		theprob<-0.5
	}
	res<-sample.int(2, size=nrow(attempts$predicted), replace=TRUE,
		prob=c(1-theprob, theprob))
	return(which(res==2))
}

validateFunction.default<-function(attempts, otherData, forrow, verbosity=0)
{
	if(is.null(otherData)) otherData<-rep(0.5, max(forrow))
	return(validateFunction.useprob(attempts, otherData, forrow,
		verbosity=verbosity))
}

#validateFunction, like the examples above, is a function that must return the
#		indices (rownumbers) of rows that are accepted
#->only supported for 1 row at a time


predict.conditional<-function(object, nobs=1, dfr, forrows, 
	validateFunction=validateFunction.default, guiddata=NULL,
	otherData=NULL, initialSuccessRateGuess=0.5, verbosity=0,
	minimumSuccessRate=0.001,...) UseMethod("predict.conditional")

predict.conditional.GLoMo<-function(object, nobs=1, dfr, forrows, 
	validateFunction=validateFunction.default, guiddata=NULL,
	otherData=NULL, initialSuccessRateGuess=0.5, verbosity=0,
	minimumSuccessRate=0.001,...)
{
	if(length(forrows)!=1) stop("In the GLoMo version of predict.conditional, only one row should be passed!")
	forrow<-forrows #This method used to have a parameter forrow -> makes it easier like this
	glomo<-object #to make it more recognizable in the following code
	catwif(verbosity > 0, "for row", forrow)
	if(! .hasNA(dfr[forrow,]))
	{
		catwif(verbosity > 0, "no NAs found")
		if(is.null(guiddata))
		{
			guiddata<-getGuidData(glomo, dfr[forrow,], guidPerObservation=NULL)
			guidToFind<-guiddata$guidPerObservation[1]
		}
		else
		{
			guidToFind<-guiddata$guidPerObservation[forrow]
		}
		return(list(predicted=dfr[forrow, ], glomorowsused=match(guidToFind,
			glomo$uid)))
	}
	successes<-0
	attempts<-0
	successRateSoFar<-initialSuccessRateGuess
	tryAtATime<-as.integer((nobs-successes) / successRateSoFar)
	if(tryAtATime < (nobs-successes)) tryAtATime<-(nobs-successes)
	if(tryAtATime < 1) tryAtATime<-1
	catwif(verbosity > 0, "get reusable Data")
	reusabledata<-reusableDataForGLoMoSampling(glomo=glomo, dfr=dfr,
		forrows=forrow, guiddata=guiddata, verbosity=verbosity-1)
	acceptedRows<-NULL
	acceptedGLoMoRowsRows<-NULL
	howManyLoops<-0
	lastLoop<-FALSE
	while(successes < nobs)
	{
		howManyLoops<-howManyLoops+1
		catwif(verbosity > 1, "start of loop", howManyLoops)
		catwif(verbosity > 5, "will try", tryAtATime, "unconditional predictions")
		catwif(verbosity > 5,
			"successes:", successes, "/", attempts, "->", successRateSoFar)
		newAttempts<-predict(glomo, nobs=tryAtATime, newdata=dfr, forrows=forrow,
			reusabledata=reusabledata, returnRepeats=FALSE, 
			returnSelectedGlomoRows=TRUE, verbosity=verbosity-1)
		catwif(verbosity > 1, "will validate results now", howManyLoops)
		newAttemptValidity<-validateFunction(newAttempts, otherData, forrow,
			verbosity=verbosity-1)
		newlyAccepted<-length(newAttemptValidity)
		if(lastLoop && (newlyAccepted < (nobs - successes)))
		{
			#we had sworn that this would be the last loop, so 'accept' randomly
			#what is still needed.
			catwif(verbosity > 0, "Emergency break: randomly accepting some observations because convergence did not occur yet", howManyLoops)
			stillOpen<-seq(tryAtATime)
			if(newlyAccepted > 0) stillOpen<-stillOpen[-newAttemptValidity]
			fakeAccepted<-sample(stillOpen, nobs - successes - newlyAccepted)
			newAttemptValidity<-sort(c(newAttemptValidity, fakeAccepted))
			newlyAccepted<-nobs - successes
		}
		if(newlyAccepted > 0)
		{
			if(newlyAccepted > nobs - successes)
			{
				#note: there used to be seq instead of sample.int here, but the attempts 
				#are grouped per glomorow, so in some cases this is a bad idea...
				newAttemptValidity<-newAttemptValidity[sample.int(newlyAccepted, nobs - successes, replace=FALSE) ]
				newlyAccepted<-nobs - successes
			}
			successes<-successes+newlyAccepted
			if(is.null(acceptedRows))
			{
				acceptedRows<-newAttempts$predicted[newAttemptValidity,]
				acceptedGLoMoRowsRows<-newAttempts$glomorowsused[newAttemptValidity]
			}
			else
			{
				acceptedRows<-rbind(acceptedRows, newAttempts$predicted[
					newAttemptValidity,])
				acceptedGLoMoRowsRows<-c(acceptedGLoMoRowsRows,
					newAttempts$glomorowsused[newAttemptValidity])
			}
		}
		attempts<-attempts+tryAtATime
		if(newlyAccepted == 0)
		{
			successRateSoFar<-successRateSoFar/2
		}
		else
		{
			successRateSoFar<-successes/attempts
		}
		if((newlyAccepted == 0) & (successRateSoFar < minimumSuccessRate))
		{
			#there have been too many failures already, so ensure not too many get
			#generated + make sure the rest is accepted anyhow in the next loop.
			successRateSoFar<-minimumSuccessRate
			lastLoop<-TRUE
		}

		tryAtATime<-as.integer((nobs-successes) / successRateSoFar)
		#cattif(verbosity > 5, "Finally: tryAtATime", tryAtATime)
		if(tryAtATime < (nobs-successes)) tryAtATime<-(nobs-successes)
		if(tryAtATime < 1) tryAtATime<-1
	}
	return(list(predicted=acceptedRows, glomorowsused=acceptedGLoMoRowsRows))
}

predict.conditional.allrows.GLoMo<-function(object, nobs=1, dfr, 
	forrows=seq(nrow(dfr)), validateFunction=validateFunction.default, 
	guiddata=NULL, otherData=NULL, initialSuccessRateGuess=0.5, verbosity=0,
	minimumSuccessRate=0.001, ...)
{
	glomo<-object #to make it more recognizable in the following code
	if(length(nobs) != length(forrows))
	{
		if(length(nobs) != 1) stop("Unsupported nobs passed along.")
		catwif(verbosity > 0, "readjusting nobs.")
		nobs<-rep(nobs, length(forrows))
	}
	if(is.null(guiddata) | (!is(guiddata, "GuidData")))
	{
		catwif(verbosity > 0,
			"guiddata were not (completely) provided so recalculating.")
		guiddata<-getGuidData(glomo, dfr, guidPerObservation=guiddata)
	}
	#note: overhead in this lapply call takes 2 seconds!!
	#that means: tim spent outside of the predict.conditional.GLoMo !!
	predPerRow<-lapply(seq_along(forrows), function(currowi){
			predict.conditional.GLoMo(object=glomo, nobs=nobs[currowi],
				dfr=dfr, forrows=forrows[currowi], validateFunction=validateFunction, 
				guiddata=guiddata,otherData=otherData,
				initialSuccessRateGuess=initialSuccessRateGuess, verbosity=verbosity-1,
				minimumSuccessRate=minimumSuccessRate)
		})
	result<-combineSimilarDfrList(lapply(predPerRow, "[[", "predicted"))
	#this should be OK for numdfr, but probably slow for data.frame
	repsPerRow<-sapply(predPerRow, function(resCurRow){nrow(resCurRow$predicted)})
	glomorowsused<-do.call(c, lapply(predPerRow, "[[", "glomorowsused"))
	return(list(predicted=result, glomorowsused=glomorowsused, repsperrow=repsPerRow))
}

#idea to make numdfr even more efficient, especially in terms of memory:
#remember which 'original' row each row refers to, and keep only the 'replacement'
#values !!! This way, when a row is repeated 20 times and only one missing value
#was there, there only needs to be one copy of the repeated values, and 20 
#different 'replacement' values!!! Need to check this!
#However: this may render the name 'numdfr' somewhat unsatisfying
#Note: this was implemented to a certain extent in numdfr.rep


#note: quite a few assumptions are made on the uniformity of the GLoMos passed in!!
combineGLoMos<-function(..., listOfGLoMos=NULL, verbosity=0)
{
	allGLoMos<-c(list(...), listOfGLoMos)
	N<-length(allGLoMos)
	catwif(verbosity > 0, "length of allGLoMos: ", N)
	numUidsPerGLoMo<-sapply(allGLoMos, function(curGLoMo){length(curGLoMo$uid)})
	catwif(verbosity > 0, "numUidsPerGLoMo: ", numUidsPerGLoMo)
	probFactorPerGLoMo<-ifelse(numUidsPerGLoMo==0, 1, 1/numUidsPerGLoMo)
	
	catwif(verbosity > 0, "allUids")
	allUids<-do.call(c, lapply(allGLoMos, "[[", "uid"))
	catwif(verbosity > 0, length(allUids), "Uids found.")
	uniqueUids<-sort(unique(allUids))
	catwif(verbosity > 0, length(uniqueUids), "unique Uids found.")
	catwif(verbosity > 0, "matchPerUniqueUid")
	matchPerUniqueUid<-sapply(allGLoMos, function(curGLoMo){match(uniqueUids,
		curGLoMo$uid)})
	#matchPerUniqueUid now holds a column per GLoMo and a row per uniqueUid.
	#if the uniqueUid does not occur in that GLoMo, it holds NA, otherwise it
	#holds the rowindex of that uniqueUid in that GLoMos uid
	catwif(verbosity > 0, "newPihat")
	newPihat<-apply(matchPerUniqueUid, 1, function(curMatchRow){
			nonNas<-!is.na(curMatchRow)
			pis<-mapply(function(ri, glomo, prf){glomo$pihat[ri]*prf},
				curMatchRow[nonNas], allGLoMos[nonNas], probFactorPerGLoMo[nonNas])
			curpi<-sum(unlist(pis))
			return(curpi)
		})
	#very _temporary_ solution: simply average out the covariance matrices!!
	#based on the concept of pooled variance (?):
	#http://en.wikipedia.org/wiki/Pooled_variance
	#may need to somehow incorporate sample size (or can I rely on nearly equal
	#sample sizes for now ?
	catwif(verbosity > 0, "combinedOmegaHat")
	omegaHatList<-lapply(allGLoMos, "[[", "omegahat")
	orgDim<-dim(allGLoMos[[1]]$omegahat)
	combinedOmegaHat<-array(do.call(c, omegaHatList), dim=c(orgDim, N))
	newOmegahat<-rowMeans(combinedOmegaHat, dims=length(orgDim))
	#another dubious one: need to think this over! -> probably not really an issue
	catwif(verbosity > 0, "allorgdims")
	allorgdims<-sapply(allGLoMos, "[[", "orgdatadim")
	newOrgdims<-rowMeans(allorgdims)
	#disregarding sample size again below
	colcount<-ncol(allGLoMos[[1]]$uniqueFactorCombinationsAndContinuousMeans)
	newFactorCols<-allGLoMos[[1]]$factorCols
	cntcls<-seq(colcount)
	if(length(newFactorCols) > 0) cntcls<-cntcls[-newFactorCols]
	catwif(verbosity > 0, "newUniqueFactorCombinationsAndContinuousMeans")
	newUniqueFactorCombinationsAndContinuousMeans<-apply(matchPerUniqueUid, 1,
		function(curMatchRow){
			nonNas<-!is.na(curMatchRow)
			usedRowList<-mapply(
				function(ri, glomo){
					rv<-glomo$uniqueFactorCombinationsAndContinuousMeans[ri,,drop=FALSE]
					return(rv)
					},
				curMatchRow[nonNas], allGLoMos[nonNas], SIMPLIFY=FALSE)
			retrow<-usedRowList[[1]]
			cntmat<-sapply(usedRowList,
				function(curnumdfr){return(unlist(curnumdfr[1, cntcls, drop=TRUE]))})
			#cntmat now holds a matrix with 1 row per cnt var and one col per 'used row'
			if(is.null(dim(cntmat)))
			{
				#This is what happens if there was only one continuous variable
				retrow[1,cntcls]<-mean(cntmat)
			}
			else
			{
				retrow[1,cntcls]<-rowMeans(cntmat)
			}
			return(retrow)
		})
	catwif(verbosity > 0, "newUniqueFactorCombinationsAndContinuousMeans combine")
	newUniqueFactorCombinationsAndContinuousMeans<-combineSimilarDfrList(
		newUniqueFactorCombinationsAndContinuousMeans)
	newGuidSeparator<-allGLoMos[[1]]$guidSeparator
	
	retval<-list(uid=uniqueUids, pihat=newPihat, omegahat=newOmegahat, 
		orgdatadim=newOrgdims,
		uniqueFactorCombinationsAndContinuousMeans=newUniqueFactorCombinationsAndContinuousMeans,
		factorCols=newFactorCols, guidSeparator=newGuidSeparator, invomega=invertSymmetric(newOmegahat, careful=FALSE))
	class(retval)<-"GLoMo"
	return(retval)
}

if(FALSE)
{
	require(addendum)
	require(NumDfr)
	setwd("C:/users/nisabbe/Documents/My Dropbox/Doctoraat/Bayesian Lasso")
	load("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
	source("GLoMo.R")
	save.image("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
}











if(FALSE)
{
	require(addendum)
	require(NumDfr)
	setwd("C:/users/nisabbe/Documents/My Dropbox/Doctoraat/Bayesian Lasso")
	load("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
	source("GLoMo.R")
	save.image("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
}




#SECTION: testing code
if(FALSE)
{
	require(addendum)
	aDfr<-generateTypicalIndependentDfr(100,100,150,catProbs=randomProbabilities,
		minn=2, maxn=4)
	aDfr.MD<-randomNA(aDfr, 0.05)

	aNDfr.MD<-numdfr(aDfr.MD)

	catw("Conversion 1 way time used: ", ttxt(system.time(tst<-numdfr(aDfr.MD))))
	#Conversion one way time used:  user: 0.05, system: 0.00, elapsed: 0.04
	#(hardly worth the mention!)

	catw("Full time used: ", ttxt(system.time(aNDfr.RF<-rCatsInDfr(aNDfr.MD,
		verbosity=0))))
	#Current implementation:
	#Full time used:  user: 2.58, system: 0.03, elapsed: 2.62
	#(a lot better than it was!!)

	catw("Full time used: ", ttxt(system.time(aDfr.RF<-rCatsInDfr(aDfr.MD,
		verbosity=0))))
	#This is my best implementation so far based on data.frame:
	#Full time used:  user: 22.22, system:  2.64, elapsed: 24.98




	require(addendum)
	require(NumDfr)

	aDfr.RFC<-rCatsAndCntInDfr(aDfr.MD, verbosity=1)
	colnames(aDfr.RFC)
	.hasNA(aDfr.RFC[,seq(ncol(aDfr.RFC)-2)])
	aNDfr.RFC<-rCatsAndCntInDfr(aNDfr.MD, verbosity=1)
	colnames(aNDfr.RFC)
	.hasNA(aNDfr.RFC[,seq(ncol(aNDfr.RFC)-2)])



	aGLoMo.RF<-GLoMo(aDfr.RFC[,seq(ncol(aDfr.RFC)-2)],
		weights=aDfr.RFC[,"weights", drop=TRUE], verbosity=10)
	aNDfr.RFC2<-numdfr(aDfr.RFC)
	aGLoMo.RF.N2<-GLoMo(aNDfr.RFC2[,seq(ncol(aNDfr.RFC2)-2), returnAsMatrix = FALSE],
		weights=aNDfr.RFC2[,"weights", drop=TRUE], verbosity=10)

	aGLoMo.RF.N<-GLoMo(aNDfr.RFC[,seq(ncol(aNDfr.RFC)-2), returnAsMatrix = FALSE],
		weights=aNDfr.RFC[,"weights", drop=TRUE], verbosity=10)



	aNDfr.MD.guids<-getGuidData(glomo=aGLoMo.RF.N, dfr=aNDfr.MD,
		guidPerObservation=NULL)
	a.nsamplesperrow<-sample.int(10, size=nrow(aNDfr.MD), replace=TRUE)
	aNDfr.MD.pred<-predict(aGLoMo.RF.N, nobs=a.nsamplesperrow, newdata=aNDfr.MD,
		reusabledata=aNDfr.MD.guids, returnRepeats=TRUE,
		returnSelectedGlomoRows=TRUE, verbosity=10)



	aNDfr.MD.predcond<-predict.conditional.allrows.GLoMo(glomo=aGLoMo.RF.N,
		nobs=a.nsamplesperrow, dfr=aNDfr.MD, forrows=seq(nrow(aNDfr.MD)),
		validateFunction=validateFunction.useprob, guiddata=aNDfr.MD.guids,
		otherData=rep(0.5, nrow(aNDfr.MD)), initialSuccessRateGuess=0.7, verbosity=20)
	str(aNDfr.MD.predcond)



	tmpeml<-EMLasso.1l.lognet.cv_parallel_1
	listOfGLoMos<-lapply(tmpeml$actualfits,
		function(curfit){curfit$fitinfo$glomo})

	class(listOfGLoMos[[1]])
	str(listOfGLoMos[[1]])

	tstglmo<-do.call(combineGLoMos, c(listOfGLoMos, verbosity=6))

}




if(FALSE)
{
	require(addendum)
	require(NumDfr)
	setwd("C:/users/nisabbe/Documents/My Dropbox/Doctoraat/Bayesian Lasso")
	load("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
	source("GLoMo.R")
	save.image("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
}
