displayPreds<-function(res, dfr, outcol, ...) UseMethod("displayPreds")
displayPreds.default<-function(res, dfr, outcol, item, ...)
{
	if(is.list(res))
	{
		if("preds" %in% names(res))
		{
			class(res)<-"CVRes"
			displayPreds(res, dfr, outcol,...)
		}
		else
		{
			displayPreds(res[[item]], dfr, outcol,...)
		}
	}
	else
	{
		res<-list(preds=res)
		class(res)<-"CVRes"
		displayPreds(res, dfr, outcol,...)
	}
}
displayPreds.CVRes<-function(res, dfr, outcol, ...)
{
	resdfr<-data.frame(trueVal=dfr[,outcol], predicted=res$preds)
	resdfr$ssq<-(resdfr$predicted-resdfr$trueVal)^2
	invisible(edit(resdfr))
}

#in contrast with the binary version, I immediately return the evaluation
#here.
doFormulaContCV<-function(dfr, outcol, fitFunc, fitAndPredictContinuous,
	formulaPattern="X", evaluatePredictions=evaluatePredictions.lms,
	includePreds=FALSE, includeDesc=FALSE, verbosity=0, ffDesc,...)
{
	if((missing(ffDesc)) || (is.null(ffDesc)))
	{
		ffDesc<-as.character(match.call()$fitFunc)
	}
	#Solution for often occurring variants ->fitAndPredictContinuous.General
	#instead of passing a function, pass a a list w at most 3 members (see above)
	#alternatively, just pass the type argument of predict (as a string)
	if(is.list(fitAndPredictContinuous))
	{
		if(verbosity > 0) cat("fitAndPredictContinuous was a list\n")
		if(!("fitFunc" %in% names(fitAndPredictContinuous)))
		{
			fitAndPredictContinuous$fitFunc<-fitFunc
		}
	}
	else if(is.character(fitAndPredictContinuous))
	{
		if(verbosity > 0) cat("fitAndPredictContinuous was a character\n")
		#assume it was the predictType that was really passed along
		fitAndPredictContinuous<-list(fitFunc=fitFunc,
			predictType=fitAndPredictContinuous)
	}
	#do tenfold crossvalidation
	if(verbosity > 0) cat("Will start crossvalidating now\n")

	predOut<-crossPredictContinuous(dfr=dfr, outcol=outcol,
		fitAndPredictContinuous=fitAndPredictContinuous, verbosity=verbosity-1,
		formulaPattern=formulaPattern,...)

	retval<-createCVResult(predOut, dfr, outcol, evaluatePredictions,
		includePreds, includeDesc, fitFunc, ffDesc, formulaPattern,
			xparlist=list(...))
	return(retval)
}

crossPredictContinuous<-function(dfr, outcol, fold=10, fitAndPredictContinuous,
	verbosity=0,...)
{
	numObs<-dim(dfr)[1]
	rndgrps<-similarSizeGroups(fold, numObs)
	if(length(outcol)==1)
	{
		predOut<-dfr[,outcol]
	}
	else
	{
		if(verbosity > 0)
		{
			cat("***Special case in crossPredictContinuous: outcol was not 1 string")
			cat(", so assuming it is the outcome itself.\n")
		}
		predOut<-outcol
		orgoutcol<-outcol
	}
	for(i in seq(fold))
	{
		if(length(outcol)!=1)
		{
			outcol<-orgoutcol[rndgrps!=i]
		}
		if(verbosity > 0) cat("Currently crossvalidating", i, "/", fold, "\n")
		if(is.list(fitAndPredictContinuous))
		{
			if(verbosity > 0) cat("General fitAndPredictContinuous for list!\n")
			predOut[rndgrps==i]<-fitAndPredictContinuous.General(dfr[rndgrps!=i,],
				dfr[rndgrps==i,], outcol=outcol,
				orgList=fitAndPredictContinuous,
				verbosity=verbosity-1, ...)
		}
		else
		{
			predOut[rndgrps==i]<-fitAndPredictContinuous(dfr[rndgrps!=i,],
				dfr[rndgrps==i,],	outcol=outcol, verbosity=verbosity-1, ...)
		}
	}
	return(predOut)
}



fitAndPredictContinuous.General<-function(trainDfr, valDfr, outcol,
	orgList, formulaPattern=".", verbosity=0,...)
{
	fitFunc<-orgList$fitFunc
	predictType<-orgList$predictType
	predictMember<-orgList$predictMember
	postProcessPred<-orgList$postProcessPred
	orgList$fitFunc<-NULL
	orgList$predictType<-NULL
	orgList$predictMember<-NULL
	orgList$postProcessPred<-NULL

	if(verbosity > 0)
	{
		cat("In fitAndPredictContinuous.General.\n")
		cat("\tdim(trainDfr):", dim(trainDfr), "\n")
		cat("\tdim(valDfr):", dim(valDfr), "\n")
		if(length(outcol) > 1)
		{
			cat("\tlength(outcol):", length(outcol), "\n")
		}
		else
		{
			cat("\toutcol:", outcol, "\n")
		}
		cat("\tformulaPattern:", formulaPattern, "\n")
		if(length(list(...)) > 0)
		{
			cat("Extra params:(", length(list(...)), ")\n")
			showParList(list(...))
			cat("End extra parameters\n")
		}
		else
		{
			cat("NO Extra params\n")
		}
		if(length(orgList) > 0)
		{
			cat("orgList:(", length(orgList), ")\n")
			showParList(orgList)
			cat("End orgList\n")
		}
	}
	gf <- basisExpansionFormula(trainDfr, outcol,
		patternNumericCols=formulaPattern)
	if(verbosity > 0) cat("Formula obtained:\n", gf, "\n")

	curres<-fitFunc(formula(gf),data=trainDfr,...)
	if((missing(predictType)) || (is.null(predictType)) || (predictType==""))
	{
		pred<-predict(curres, newdata=valDfr)
	}
	else
	{
		pred<-predict(curres, newdata=valDfr, type=predictType)
	}
	if((! missing(predictMember)) && (! is.null(predictMember)) &&
		(predictMember!= "."))
	{
		if(verbosity > 0) cat("Use predictMember: ", predictMember, "\n")
		pred<-unlist(pred[predictMember])
	}
	if((! missing(postProcessPred)) && (! is.null(postProcessPred)) &&
		(is.function(postProcessPred)))
	{
		if(verbosity > 0) cat("Postprocessing prediction\n")
		orgList$pred<-pred
		orgList<-c(orgList, list(...))
		pred<-do.call(postProcessPred, orgList)
	}
	if(verbosity > 0)
	{
		cat("->fitAndPredictContinuous.General Results in pred with structure:\n")
		print(str(pred))
	}
	return(pred)
}

createCVResult<-function(predOut, dfr, outcol, evaluatePredictions,
	includePreds=FALSE, includeDesc=FALSE,
	fitFunc, ffDesc, formulaPattern, xparlist)
{

	retval<-evaluatePredictions(predOut, dfr[,outcol])
	if(includePreds | includeDesc)
	{
		retval<-list(evaluated=retval)
		if(includePreds)
		{
			retval$preds<-predOut
		}
		if(includeDesc)
		{
			retval$desc<-getFormulaCVDescription(fitFunc, ffDesc, formulaPattern,
				xparlist)
			class(retval)<-"CVRes"
		}
	}
	return(retval)
}

evaluatePredictions.lms<-function(predicted, realval)
{
	return(sqrt(mean((predicted-realval)^2)))
}

getFormulaCVDescription<-function(fitFunc, ffDesc, formulaPattern, xparlist)
{
	expa<-simpleDescription(xparlist)
	if(is.null(expa)) expa<-""
	expat<-ifelse(expa=="","no extra parameters",paste("extra parameters", expa))
	if(!is.character(ffDesc))
	{
		cat("Unexpected in getFormulaCVDescription: ffDesc was not character.\n")
		ffDesc<-as.character(match.call()$fitFunc)
		if(!is.character(ffDesc))
		{
			cat("--->fitFunc could also not be converted to character...\n")
			ffDesc<-"unknown function (?)"
		}
	}
	paste(ffDesc, " with formula pattern ", formulaPattern,
		", and ", expat, sep="")
}

print.CVRes<-function(x,...)
{
	cat(x$desc, "\n")
	cat("\tEvaluated:", x$evaluated, "\n")
}






doFormulaCV<-function(dfr, outcol, fitFunc, fitAndPredictBinary,
	mainname=paste("m", as.character(match.call()$fitFunc), sep=""),
	classname=paste(as.character(match.call()$fitFunc), "ex", sep=""),
	formulaPattern="X", passMainRes=FALSE, verbosity=0,...)
{
	#cat("main=", mainname, ", class=", classname, "\n")
	if((! missing(fitFunc)) && (! is.null(fitFunc)) && (is.function(fitFunc)))
	{
		gf<-basisExpansionFormula(dfr, outcol, patternNumericCols=formulaPattern)
		if(verbosity > 2)
		{
			cat("Formula used in doFormulaCV:\n", gf, "\n")
		}
		#add timeout here if mainres takes too long
		stopIfRanTooLong({
			mainres<-fitFunc(formula(gf),data=dfr,...)
		})
	}
	else
	{
		mainres<-"no main result requested"
		cat(mainres, "\n")
	}
	#little solution for often occurring variants -> fitAndPredictBinary.General
	#instead of passing a function, pass a a list w at most 3 members (see above)
	#alternatively, just pass the type argument of predict (as a string)
	if(is.list(fitAndPredictBinary))
	{
		if(verbosity > 0) cat("fitAndPredictBinary was a list\n")
		if(!("fitFunc" %in% names(fitAndPredictBinary)))
		{
			fitAndPredictBinary$fitFunc<-fitFunc
		}
	}
	else if(is.character(fitAndPredictBinary))
	{
		if(verbosity > 0) cat("fitAndPredictBinary was a character\n")
		#assume it was the predictType that was really passed along
		fitAndPredictBinary<-list(fitFunc=fitFunc, predictType=fitAndPredictBinary)
	}
	#do tenfold crossvalidation
	if(verbosity > 0) cat("Will start crossvalidating now\n")
	if(passMainRes)
	{
		if(verbosity > 1) cat("Passing along the main result...\n")
		predOut<-crossPredictBinary(dfr=dfr, outcol=outcol,
			fitAndPredictBinary=fitAndPredictBinary, verbosity=verbosity-1,
			formulaPattern=formulaPattern, mainRes=force(mainres),...)
	}
	else
	{
		if(verbosity > 1) cat("Ignoring the main result...\n")
		predOut<-crossPredictBinary(dfr=dfr, outcol=outcol,
			fitAndPredictBinary=fitAndPredictBinary, verbosity=verbosity-1,
			formulaPattern=formulaPattern,...)
	}
	retval<-list(mainres, cv=predOut)
	names(retval)[1]<-mainname
	class(retval)<-classname
	return(retval)
}

crossPredictBinary<-function(dfr, outcol, fold=10, fitAndPredictBinary,
	verbosity=0,...)
{
	numObs<-dim(dfr)[1]
	rndgrps<-similarSizeGroups(fold, numObs)
	if(length(outcol)==1)
	{
		predOut<-dfr[,outcol]
	}
	else
	{
		if(verbosity > 0)
		{
			cat("***Special case in crossPredictBinary: outcol was not 1 string, ")
			cat("so assuming it is the outcome itself.\n")
		}
		predOut<-outcol
		orgoutcol<-outcol
	}
	if(is.factor(predOut))
	{
		lvls<-levels(predOut)
	}
	else
	{
		lvls<-sort(unique(predOut))
	}
	for(i in seq(fold))
	{
		if(length(outcol)!=1)
		{
			outcol<-orgoutcol[rndgrps!=i]
		}
		if(verbosity > 0) cat("Currently crossvalidating", i, "/", fold, "\n")
		stopIfRanTooLong({
				if(is.list(fitAndPredictBinary))
				{
					if(verbosity > 0) cat("General fitAndPredictBinary for list!\n")
					predOut[rndgrps==i]<-fitAndPredictBinary.General(dfr[rndgrps!=i,],
						dfr[rndgrps==i,], outcol=outcol, lvls=lvls,
						fitFunc=fitAndPredictBinary$fitFunc,
						predictType=fitAndPredictBinary$predictType,
						predictMember=fitAndPredictBinary$predictMember,
						verbosity=verbosity-1, ...)
				}
				else
				{
					predOut[rndgrps==i]<-fitAndPredictBinary(dfr[rndgrps!=i,],
						dfr[rndgrps==i,],	outcol=outcol, lvls=lvls, verbosity=verbosity-1,
						...)
				}
			})
	}
	return(predOut)
}






#should cover a lot of cases wothout having to explicitly write a
#fitAndPredictBinary function for it...
fitAndPredictBinary.General<-function(trainDfr, valDfr, outcol, lvls, fitFunc,
	predictType, predictMember, formulaPattern=".", verbosity=0,...)
{
		gf <- basisExpansionFormula(trainDfr, outcol,
			patternNumericCols=formulaPattern)
		curres=fitFunc(formula(gf),data=trainDfr,...)
		pred<-predict(curres, newdata=valDfr, type=predictType)
		if((! missing(predictMember)) && (! is.null(predictMember)) &&
			(predictMember!= "."))
		{
			if(verbosity > 0) cat("Use predictMember: ", predictMember, "\n")
			pred<-unlist(pred[predictMember])
			if(verbosity > 0) cat("Results in pred with structure: ", str(pred), "\n")
		}
		if(is.numeric(pred))
		{
			if(verbosity > 0) cat("Numeric predictions, assuming probabilities...\n")
			#assume these are probabilities
			return(lvls[ifelse(pred > 0.5, 2, 1)])
		}
		else
		{
			return(pred)
		}
}
