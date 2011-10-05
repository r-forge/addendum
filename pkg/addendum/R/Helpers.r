#When you have a list where each item has a sub-item with the
#given name, this function returns alist with all those subitems
getListItemsAsColumn<-function(lst, name)
{
	sapply(lst, function(curli){
			return(curli[[name]])
		})
}

#returns a vector based on fromWhat, without the elements in removeWhat
#verbose TRUE gives feedback on how many elements actually removed
removeItems<-function(removeWhat,fromWhat, verbose=FALSE)
{
	if(verbose)
	{
		cat("Try to remove\n")
		print(removeWhat)
		cat("From\n")
		print(fromWhat)
	}
	found<-unique(do.call(c, lapply(removeWhat, function(w){
			if(is.na(w)){which(is.na(fromWhat))}else{which(fromWhat==w)}
		})))
	if(length(found) > 0)
	{
		if(verbose) cat("Removing", length(found), "items from the data set.\n")
		return(fromWhat[-found])
	}
	else
	{
		return(fromWhat)
	}
}

removeCols<-function(dfr, colsToRemove, verbose=FALSE)
{
	cn<-colnames(dfr)
	ncn<-removeItems(colsToRemove, cn, verbose=verbose)
	dfr[,ncn]
}

#same functionality as in sapply: changes list to vector / matrix if applicable
#answer: list, e.g. return value of lapply, one item for each element of X
#X: vector/list that holds the names (like what you pass to sapply) -> only
#   relevant if USE.NAMES=TRUE
#USE.NAMES: see sapply
doSimplify<-function(answer,X=NULL, USE.NAMES = TRUE)
{
  if (USE.NAMES && (!is.null(X)) && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (length(answer) && length(common.len <- unique(unlist(lapply(answer,
    length)))) == 1L) {
    if (common.len == 1L)
      unlist(answer, recursive = FALSE)
    else if (common.len > 1L)
      array(unlist(answer, recursive = FALSE), dim = c(common.len,
        length(X)), dimnames = if (!(is.null(n1 <- names(answer[[1L]])) &
        is.null(n2 <- names(answer))))
        list(n1, n2))
    else answer
  }
  else answer
}

#Find univariate outliers in the boxplot sense (outside the whiskers, i.e. 1.5
#   times the IQR
outlierIndices<-function(cvar)
{
	bps<-boxplot(cvar, plot=FALSE)$stats[,1]
#	sumVar<-summary(cvar);
#	Q1<-sumVar[2]; Q3<-sumVar[5];
#	IQR<-as.numeric(Q3-Q1);
#	whiskl<-Q1-1.5*IQR
#	whiskr<-Q3+1.5*IQR
#	whiskl<-min(cvar[cvar>=whiskl])
#	whiskr<-max(cvar[cvar<=whiskr])
	list(indices=which((cvar<bps[1])|(cvar>bps[5])), Q1=bps[2], Q3=bps[4],
		IQR=bps[4]-bps[2], whiskl=bps[1], whiskr=bps[5])
}

#Plot a histogram including a density estimation (in red)
histAndDensity<-function(x, ...)
{
  retval<-hist(x, freq=FALSE, ...)
  lines(density(x, na.rm=TRUE), col="red")
  invisible(retval)
}

#Scatterplot + smoothed version(in red)
plotAndSmooth<-function(x,y,plotfunc=plot,...)
{
  plotfunc(x,y,...)
  lines(safeLowess(x,y),col="red")
  invisible()
}

#lowess wrapper that handles missing values etc. (by not including them)
safeLowess<-function(x,y,...)
{
  invalid<-is.na(x) | is.nan(x) | is.infinite(x) | is.na(y) | is.nan(y) | is.infinite(y)
  if(sum(invalid)>0)
  {
    lowess(x[!invalid],y[!invalid],...)
  }
  else
  {
    lowess(x,y,...)
  }
}

#QQ-plot including qqline (in red)
qqWithPred<-function(x,...)
{
  qqnorm(x, ...)
  qqline(x, col="red")
}

#Given a number of plots to display at the same time, find a layout that fits
#that many and is more or less square
squareLikeLayoutForNGraphs<-function(n, useLayout=TRUE, traceRes=FALSE)
{
  side<-ceiling(sqrt(n))
  oside<-ceiling(n / side)
  #c(side, oside)
  if(traceRes)
	{
		cat("For", n, ":", side, "x", oside,"\n")
	}
  if(useLayout)
  {
	  rest<-side*oside-n
	  cont<-c(1:n, rep(0,rest))
	  m<-matrix(cont, nrow=oside, byrow=TRUE)
	  layout(m)
  }
  else
  {
  	par(mfrow=c(side, oside))
  }
  #layout.show(n)

  c(side, oside)
}

#Plot all the relations between any column in dfr to the given column
#comparingColName. It is possible the indicate some particular column relations
# (markCols), or only showa subsetof the column relations (onlyMarkCols).
#Finally, you can make a set of observations, defined by a vector of rowindices, 
#be indicated in a different color (e.g.: outlying observations)
#Decides which type of graph is most appropriate.
plotAllScatters<-function(comparingColName, dfr, ylab, markCols=c(),
	onlyMarkCols=FALSE, indicateObs=c(), excludeOrg=FALSE,
	avoidUnivariateOutliers=FALSE)
{
	markColor<-"green"
	indicateColor<-"orange"
	stdColor<-"black"
	extraLineColor<-"red"

	if(avoidUnivariateOutliers)
	{
		dfr<-removeUnivariateOutliers(dfr, verbosity=0)
	}

  comparingCol<-dfr[,comparingColName]
  useCols<-colnames(dfr)
  if(onlyMarkCols)
  {
    useCols<-markCols
    markCols<-c()
  }
  useCols<-unique(useCols)
  if(excludeOrg)
  {
		if(comparingColName %in% useCols)
		{
			useCols<-useCols[-match(comparingColName, useCols)]
		}
  }
  n<-length(useCols)
  squareLikeLayoutForNGraphs(n)
  for(colnm in useCols)
  {
    curcol<-dfr[,colnm]
    par(mar=c(4,1,1,1))
    txtcol<-stdColor
    if(is.element(colnm, markCols))
    {
      txtcol<-markColor
    }
    if(colnm==comparingColName)
    {
      if(is.factor(curcol))
      {
        barplot(table(curcol), main = "", xlab=colnm)
      }
      else
      {
        hist(curcol, freq=FALSE, main = "", xlab=colnm)
        lines(density(curcol, na.rm=TRUE), col=extraLineColor)
      }
    }
    else
    {
      if(is.factor(curcol))
      {
        if(is.factor(comparingCol))
        {
          ttt<-table(data.frame(curcol,comparingCol))
          tt<-data.frame(ttt)
          colnames(tt)<-c(colnm, comparingColName, "Freq")
          #print(names(tt))
          tt[,colnm]<-as.factor(tt[,colnm])
          tt[,comparingColName]<-as.factor(tt[,comparingColName])

          ppin <- par("pin")
          xsize<-ppin[1] / (length(levels(curcol)) + 1)
          ysize<-ppin[2] / (length(levels(comparingCol)) + 1)
          sunflowerplot(tt[,colnm],tt[,comparingColName], number=tt$Freq,
            xlim = c(0.5,length(levels(curcol)) + 0.5),
            ylim = c(0.5,length(levels(comparingCol)) + 0.5),
            xlab = colnm, size=min(xsize,ysize)/2, main="")
        }
        else
        {
          boxplot(comparingCol~curcol, xlab=colnm, ylab=ylab, col.lab=txtcol,
						main="")
          if(length(indicateObs) > 0)
          {
            boxplot(comparingCol[indicateObs]~curcol[indicateObs], xlab="",
							ylab="", add=TRUE, col=indicateColor, main="")
          }
        }
      }
      else
      {
        if(is.factor(comparingCol))
        {
          boxplot(curcol~comparingCol, xlab=colnm, ylab=ylab, col.lab=txtcol,
						main="",horizontal=TRUE)
          if(length(indicateObs) > 0)
          {
            boxplot(curcol[indicateObs]~comparingCol[indicateObs], xlab="",
							ylab="", add=TRUE, col=indicateColor, main="",horizontal=TRUE)
          }
        }
        else
        {
          plot(curcol, comparingCol, xlab=colnm, ylab=ylab, col.lab=txtcol,
						main="")
          validDots<-(!is.infinite(curcol)) & (!is.na(curcol)) &
						(!is.nan(curcol)) & (!is.infinite(comparingCol)) &
						(!is.na(comparingCol)) & (!is.nan(comparingCol))
          scuc<-curcol[validDots]
          scoc<-comparingCol[validDots]
          lines(lowess(scuc, scoc),col="red")
          if(length(indicateObs) > 0)
          {
            points(curcol[indicateObs], comparingCol[indicateObs], xlab="",
							ylab="", col=indicateColor)
          }
        }
      }
    }
  }
  invisible(layout(1))
}

#Sunflowerplot for use in smartplot thus in smartpairs
#Note: slightly different versions of this function available, fitting different
#contexts
plotTwoCats<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = FALSE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, ...)
{
  ttt<-table(data.frame(x,y))
  tt<-data.frame(ttt)
  #print(names(tt))
  tt$x<-as.factor(tt$x)
  tt$y<-as.factor(tt$y)

  ppin <- par("pin")
  xsize<-ppin[1] / (length(levels(x)) + 1)
  ysize<-ppin[2] / (length(levels(y)) + 1)
  sunflowerplot(tt$x,tt$y, number=tt$Freq,
    xlim = c(0.5,length(levels(x)) + 0.5),
    ylim = c(0.5,length(levels(y)) + 0.5),
    log = log, main = main, sub = sub,
    xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
    panel.first = panel.first, panel.last = panel.last, asp = asp,
    size=min(xsize,ysize)/2,
    ...)

  invisible()
}

#new version that doesn't use sunflower plots but more easily interpreted one:
#in this version, color indicates membership
#TO DO: use cex or similar to size the text according to the room available...
plotTwoCats2<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = FALSE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, ...)
{
  ttt<-table(data.frame(x,y))
  tt<-data.frame(ttt)
  #print(names(tt))
  tt$x<-as.factor(tt$x)
  tt$y<-as.factor(tt$y)
  usr<-par("usr"); on.exit(par(usr))

	minFreq<-min(tt$Freq)
	maxFreq<-max(tt$Freq)
	numX<-length(levels(tt$x))
	numY<-length(levels(tt$y))
	#par(usr=c(0, numX, 0, numY))
  ppin <- par("pin")
  xsize<-ppin[1] / (length(levels(x)) + 1)
  ysize<-ppin[2] / (length(levels(y)) + 1)
	plot(as.numeric(x),as.numeric(y),
		xlim=c(0.5,length(levels(x)) + 0.5),
		ylim=c(0.5,length(levels(y)) + 0.5),
    log = log, main = main, sub = sub,
    xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
    panel.first = panel.first, panel.last = panel.last, asp = asp, ...)


	sapply(seq(numX), function(xi){
			sapply(seq(numY), function(yi){
					frq<-tt$Freq[(as.numeric(tt$x)==xi) & (as.numeric(tt$y)==yi)]
					freqpct<-(frq-minFreq)/(maxFreq-minFreq)
					clr<-rgb(freqpct, 1-freqpct, 0, 1)
					rect(-0.5+xi, -0.5+yi, 0.5+xi, 0.5+yi, col=clr)
					text(xi, yi, labels=frq)
					invisible()
				})
			invisible()
		})
  invisible()
}

#Boxplot for use in smartplot thus in smartpairs
#Note: slightly different versions of this function available, fitting different
#contexts
plotBox<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, horizontal=FALSE, ...)
{
  boxplot(y~x,names=rep("", length(levels(x))), xlim = xlim, ylim = ylim,
		log = log, main = main, sub = sub,
    xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
    panel.first = panel.first, panel.last = panel.last, asp = asp, notch=FALSE,
    show.names=FALSE, horizontal=horizontal, ...)
}

#For use in smartpairs: decide which scatter-like plot is most appropriate for 
#two variables, and plot this one.
#Diagonal (see smartpairs): barplot (categorical) / histogram (continuous)
#Rest:
#   * 2 categoricals: sunflowerplot
#   * 1 categorical: grouped boxplots
#   * 2 continuous: scatter
#Note: slightly different versions of this function available, fitting different
#contexts
smartplot<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, diagon=FALSE, twoCatPlot=plotTwoCats,
		verbosity=0, ...)
{
  if(diagon)
  {
    #assume we are on the diagonal...
    #may be nice to plot a histogram then?
    if(is.factor(x))
    {
    	cattif(verbosity > 0, "smartplot, diagonal, factor")
      barplot(table(x), main = main, sub = sub,
        xlab = xlab, ylab = ylab, ann = ann, axes = axes, axisnames=FALSE,
        panel.first = panel.first, panel.last = panel.last, asp = asp, ...)
    }
    else
    {
    	cattif(verbosity > 0, "smartplot, diagonal, non-factor")
      hist(x, freq=FALSE, main = main, sub = sub,
        xlab = xlab, ylab = ylab, ann = ann, axes = axes,
        panel.first = panel.first, panel.last = panel.last, asp = asp, ...)
      lines(density(x, na.rm=TRUE), col="red")
    }
  }
  else
  {
    if(is.factor(x))
    {
      if(is.factor(y))
      {
				cattif(verbosity > 0, "smartplot, off diagonal, 2 factors")
        twoCatPlot(x, y, type = type, xlim = xlim, ylim = ylim, log = log,
					main = main, sub = sub, xlab = xlab, ylab = ylab, ann = ann,
					axes = axes, frame.plot = frame.plot, panel.first = panel.first,
					panel.last = panel.last, asp = asp, ...)
      }
      else
      {
				cattif(verbosity > 0, "smartplot, off diagonal, 1 factor (x)")
        plotBox(x,y, xlim = xlim, ylim = ylim, log = log, main = main,
					sub = sub, xlab = xlab, ylab = ylab, ann = ann, axes = axes,
					frame.plot = frame.plot, panel.first = panel.first,
					panel.last = panel.last, asp = asp, ...)
      }
    }
    else if(is.factor(y))
    {
			cattif(verbosity > 0, "smartplot, off diagonal, 1 factor (y)")
      plotBox(y,x, horizontal=TRUE, xlim = xlim, ylim = ylim, log = log,
				main = main, sub = sub, xlab = xlab, ylab = ylab, ann = ann,
				axes = axes, frame.plot = frame.plot, panel.first = panel.first,
				panel.last = panel.last, asp = asp, ...)
    }
    else
    {
			cattif(verbosity > 0, "smartplot, off diagonal, numeric")
      x<-as.numeric(x)
      y<-as.numeric(y)
      plot(x,y, type = type, xlim = xlim, ylim = ylim, log = log, main = main,
				sub = sub, xlab = xlab, ylab = ylab, ann = ann, axes = axes,
				frame.plot = frame.plot, panel.first = panel.first,
				panel.last = panel.last, asp = asp, ...)
      lines(safeLowess(x,y),col="red", xlim = xlim, ylim = ylim,
        panel.first = panel.first, panel.last = panel.last, asp = asp, ...)
    }
  }
  invisible()
}

#For use in smartpairs: draw the right kind of axis at the side
smartAxis<-function(x = NULL, at = NULL, ..., side, labels = NULL,
	otherIsFactor = FALSE, last = FALSE)
{
  if(is.factor(x))
  {
    Axis(x=(1:length(levels(x))), at=(1:length(levels(x))), ..., side=side,
			labels = as.character(levels(x)))
  }
  else
  {
    Axis(x=x, at=at, ..., side=side, labels = labels)
  }
}

removeUnivariateOutliers<-function(x, setNA=TRUE, verbosity=0)
{
	nc<-dim(x)[2]
	indices<-lapply(seq(nc), function(i){
		if(sum(is.numeric(x[,i])) > 0)
		{
			rv<-outlierIndices(x[,i])$indices
			if(verbosity > 0)
			{
				if(length(rv) > 0)
				{
					cat("The following observations are outliers for column", i, ":\n")
					print(rv)
				}
			}
			rv
		}
		else
		{
			if(verbosity > 0) cat("Non numeric column", i, "ignored.\n")
			NULL
		}
	})
	if(setNA)
	{
		hasOut<-which(sapply(indices, length)>0)
		for(i in hasOut)
		{
			if(verbosity > 0) cat("Setting NAs for column", i, "\n")
			rws<-indices[[i]]
			x[rws, i]<-NA
		}
	}
	else
	{
		outlierRows<-unique(do.call(c, indices))
		if(length(outlierRows) > 0)
		{
			x<-x[-outlierRows,]
		}
	}
	return(x)
}

#Draws a huge matrix showing the one-to-one relatioships between variables in
# a dataframe.
#This is very similar to pairs, but uses smarter graphs for some cases (factors)
smartpairs<-function (truex, labels, panel = points, ..., lower.panel = panel,
    upper.panel = panel, diag.panel = NULL, text.panel = textPanel,
    label.pos = 0.5 + has.diag/3, cex.labels = NULL, font.labels = 1,
    row1attop = TRUE, gap = 1, localPlot=defaultLocalPlot,
		localAxis=defaultLocalAxis,	twoCatPlot=plotTwoCats2, verbosity=0,
		avoidUnivariateOutliers=FALSE)
{
		if(avoidUnivariateOutliers)
		{
			truex<-removeUnivariateOutliers(truex, verbosity=verbosity)
		}
    x<-truex
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x,
        y, txt, cex = cex, font = font)
    defaultLocalAxis <- function(side, x, y, xpd, bg, col = NULL, main,
      oma, ...) {
        if (side%%2 == 1)
        {
            smartAxis(x, side = side, xpd = NA, otherIsFactor=is.factor(y), ...)
				}
        else
				{
						smartAxis(y, side = side, xpd = NA, otherIsFactor=is.factor(x), ...)
				}
    }
    defaultLocalPlot <- function(..., main, oma, font.main, cex.main){
			smartplot(..., twoCatPlot=twoCatPlot, verbosity=verbosity-1)}
    localLowerPanel <- function(..., main, oma, font.main, cex.main){
			lower.panel(...)}
    localUpperPanel <- function(..., main, oma, font.main, cex.main){
			upper.panel(...)}
    localDiagPanel <- function(..., main, oma, font.main, cex.main){
			diag.panel(...)}
    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for (i in seq_along(names(x))) {
            if (is.factor(x[[i]]) || is.logical(x[[i]]))
                x[[i]] <- as.numeric(x[[i]])
            if (!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    }
    else if (!is.numeric(x))
        stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel))
        diag.panel <- match.fun(diag.panel)
    if (row1attop) {
        tmp <- lower.panel
        lower.panel <- upper.panel
        upper.panel <- tmp
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
    }
    nc <- ncol(x)
    if (nc < 2)
        stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels))
            labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels))
        has.labs <- FALSE
    oma <- if ("oma" %in% nmdots)
        dots$oma
    else NULL
    main <- if ("main" %in% nmdots)
        dots$main
    else NULL
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main))
            oma[3L] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    for (i in if (row1attop)
        1L:nc
    else nc:1) for (j in 1L:nc) {
				cattif(verbosity > 0, "smartPairs plotting (", i, ", ", j, ")")
				if(length(colnames(truex)) > 0)
				{
					cattif(verbosity > 0, "i.e. (", colnames(truex)[i], ", ",
						colnames(truex)[j], ")")
				}
        dia<-ifelse(i==j,TRUE,FALSE)
        localPlot(truex[, j], truex[, i], xlab = "", ylab = "", axes = FALSE,
            type = "n",diagon=dia, ...)
        if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
            box()
            firstvec<-truex[, j]
            secondvec<-truex[, i]
            if((i == nc) && (j == nc) && (nc%%2))
            {
            }
            if (i == 1 && (!(j%%2) || !has.upper || !has.lower))
                localAxis(1 + 2 * row1attop, firstvec, secondvec,
                  ...)
            if (i == nc && (j%%2 || !has.upper || !has.lower))
            {
              if(j == nc )
              {
                localAxis(3 - 2 * row1attop, firstvec, secondvec,last=TRUE,...)
              }
              else
              {
                localAxis(3 - 2 * row1attop, firstvec, secondvec,...)
              }
            }
            if (j == 1 && (!(i%%2) || !has.upper || !has.lower))
                localAxis(2, firstvec, secondvec, ...)
            if (j == nc && (i%%2 || !has.upper || !has.lower))
            {
              if(i == nc )
              {
                localAxis(4, firstvec, secondvec,last=TRUE, ...)
              }
              else
              {
                localAxis(4, firstvec, secondvec, ...)
              }
            }
            mfg <- par("mfg")
            if (i == j) {
                if (has.diag)
                  localDiagPanel(as.vector(x[, i]), ...)
                if (has.labs) {
                  par(usr = c(0, 1, 0, 1))
                  if (is.null(cex.labels)) {
                    l.wid <- strwidth(labels, "user")
                    cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                  }
                  text.panel(0.5, label.pos, labels[i], cex = cex.labels,
                    font = font.labels)
                }
            }
            else if((!is.factor(truex[[i]])) && (!is.factor(truex[[j]])))
            {
              if (i < j)
                  localLowerPanel(as.vector(x[, j]), as.vector(x[,
                    i]), ...)
              else localUpperPanel(as.vector(x[, j]), as.vector(x[,
                  i]), ...)
            }
            if (any(par("mfg") != mfg))
                stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
    }
    if (!is.null(main)) {
        font.main <- if ("font.main" %in% nmdots)
            dots$font.main
        else par("font.main")
        cex.main <- if ("cex.main" %in% nmdots)
            dots$cex.main
        else par("cex.main")
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}

#Tries to execute the given expression.
#If it fails, return an object of class "try-error", holding the error message
#expr: the expression to be executed
#silent: if TRUE, the error message is not immediately displayed (only returned,
#		unless errRet is not NULL)
#errRet: if not NULL, this is returned when an error occurred. Unless silent was
#   FALSE (and the error was thus output), there is no way to recover 
#   information on what error occurred anymore...
tryRet<-function(expr, silent = FALSE, errRet = NULL)
{
  tryCatch(expr, error = function(e) {
    call <- conditionCall(e)
    if (!is.null(call)) {
      if (identical(call[[1L]], quote(doTryCatch)))
        call <- sys.call(-4L)
      dcall <- deparse(call)[1L]
      prefix <- paste("Error in", dcall, ": ")
      LONG <- 75L
      msg <- conditionMessage(e)
      sm <- strsplit(msg, "\n")[[1L]]
      w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L],
        type = "w")
      if (is.na(w))
        w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],
          type = "b")
      if (w > LONG)
        prefix <- paste(prefix, "\n  ", sep = "")
    }
    else prefix <- "Error : "
    msg <- paste(prefix, conditionMessage(e), "\n", sep = "")
    .Internal(seterrmessage(msg[1L]))
    if (!silent && identical(getOption("show.error.messages"),
      TRUE)) {
      cat(msg, file = stderr())
      .Internal(printDeferredWarnings())
    }
    if(is.null(errRet))
    {
      invisible(structure(msg, class = "try-error"))
    }
    else
    {
      errRet
    }
  })
}

#Given an outcome column name (outCol) and a set of predictor column names
#		(predCols), generate a model formula text
modelText<-function(outCol, predCols)
{
	if(length(predCols)>0)
	{
		predfrm<-paste(predCols, collapse=" + ")
	}
	else
	{
		predfrm<-"1"
	}
  paste(outCol, predfrm, sep=" ~ ")
}

#char version of formula for proper display
asString<-function(form)
{
  if(is.character(form))
  {
    return(form)
  }
  else
  {
    parts<-as.character(form)
    return(modelText(parts[2], parts[-c(1,2)]))
  }
}

#get the predictor column names from a formula
formulaCols<-function(form)
{
  parts<-as.character(as.formula(form))
  list(outcol=parts[2], predcols=parts[-c(1,2)])
}

#sped up version of rmvnorm, using a choleski decomposition of the covar matrix
#in quite a few cases we already know the chol decomp of sigma!
qrmvnormsqrt<-function (n, mean, cholsigma) 
{
	#see qrmvnorm for more info
	#This version assumes a choleski decomposed version of the covariance
	#matrix is already passed.
	#Note: pivot=TRUE is supposed!!!
	if(is.null(dim(mean)))
	{
		if(length(mean)==1)
		{
			#assume sigma is also simply a number then
			return(matrix(rnorm(n, mean=mean, sd=cholsigma), ncol=1))
		}
	}
	retval <- cholsigma
	o <- order(attr(retval, "pivot"))
	retval <- retval[, o]
  retval <- matrix(rnorm(n * ncol(cholsigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

#sped up version of rmvnorm, leaving out all sanity checks
qrmvnorm<-function (n, mean, sigma)
{
	#note: code copied from rmvnorm, + adapted : faster/more fit for my case
	#Change 1: if only 1 mean, simply use rnorm
	if(is.null(dim(mean)))
	{
		if(length(mean)==1)
		{
			#assume sigma is also simply a number then
			return(matrix(rnorm(n, mean=mean, sd=sqrt(sigma)), ncol=1))
		}
	}
	#change 2: leave out checks
	#Change 3: always use chol
	retval <- chol(sigma, pivot = TRUE)
	o <- order(attr(retval, "pivot"))
	retval <- retval[, o]

  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

allLevels<-function(x, onlyNonEmpty=FALSE) UseMethod("allLevels")
allLevels.data.frame<-function(x, onlyNonEmpty=FALSE){
	retval<-lapply(x, function(curcol){
			tmp<-levels(curcol)
			if(is.null(tmp)) tmp<-character(0)
			return(tmp)
		})
	if(onlyNonEmpty)
	{
		keep<-sapply(retval, length) > 0
		retval<-retval[keep]
	}
	return(retval)
}

dfrConversionProbs<-function(dfr, betweenColAndLevel)
{
#	allcurfn<-curfnfinder(skipframes=0, retStack=TRUE, extraPrefPerLevel="|")
#	catw(allcurfn, ": start. Avoid calling this is much as possible: reuse its result!")
	lvls<-allLevels(dfr)
	nc<-length(lvls)

	reps<-sapply(lvls, length)-1
	reps[reps<1]<-1

	repcols<-rep(seq_along(reps), reps)
	newlvls<-do.call(c, lapply(reps, function(currep){if(currep==1) 1 else (seq(currep)+1)}))

	orgfactcols<-which(reps>1)

	startoforgcolinnewmat<-cumsum(c(1, reps))[seq_along(reps)]
	mustmatchforfactcols<-do.call(c, lapply(reps[orgfactcols], seq))+1
	newcolsfromfact<-rep(startoforgcolinnewmat[orgfactcols], reps[orgfactcols]) + mustmatchforfactcols -2
	colexs<-do.call(c, lapply(lvls[orgfactcols], function(curlvls){if(length(curlvls) > 1) curlvls[-1] else ""}))
	coln<-rep(colnames(dfr), reps)
	newcoln<-coln
	newcoln[newcolsfromfact]<-paste(newcoln[newcolsfromfact], colexs, sep=betweenColAndLevel)

	retval<-list(
		lvls=lvls,
		reps=reps,
		orgfactcols=orgfactcols,
		startoforgcolinnewmat=startoforgcolinnewmat,
		betweenColAndLevel=betweenColAndLevel,
		newformdata=data.frame(
			repcols=repcols, #which original column number does the new row point to
			newlvls=newlvls, #which level(number) does the new row point to (1 for continuous)
			coln=coln, #what was the original column name
			newcoln=newcoln, #what is the extended column name
			isfact=(coln!=newcoln),
			stringsAsFactors=FALSE
		)
	)
	class(retval)<-"dfrConversionProbs"
	return(retval)
}

factorsToDummyVariables<-function(dfr, betweenColAndLevel = "", dfrConvData, verbosity=0,...)  UseMethod("factorsToDummyVariables")
#Typical use: for glmnet. Convert a dataframe to a matrix, where factor
#   columns are split into dummy variables (first level = reference)
#betweenColAndLevel: in the name of the dummy columns, what comes between the
#   original column name and the column level
factorsToDummyVariables.default<-function(dfr, betweenColAndLevel="", dfrConvData, verbosity=0,...)
{
	#note this version seems a lot faster than
	#dfrTmp<-model.frame(dfrPredictors, na.action=na.pass)
	#return(as.matrix(model.matrix(as.formula(form), data=dfrTmp))[,-1])
	if(missing(dfrConvData))
	{
		catwif(verbosity>0, "Need to recalculate dfrConvData: avoid!")
		dfrConvData<-dfrConversionProbs(dfr, betweenColAndLevel)
	}

	mat<-colsAsNumericMatrix(dfr)
	nr<-dim(mat)[1]
	retval<-mat[, dfrConvData$newformdata$repcols, drop=FALSE] #already the right size!
	facts<-dfrConvData$newformdata$isfact
	if(sum(facts) > 0)
	{
		catwif(verbosity > 0, "Categorical conversion needed for columns:", dfrConvData$newformdata$newcoln[facts])
		tocomp<-matrix(rep.int(dfrConvData$newformdata$newlvls[facts],nr),nr,byrow=TRUE)
		retval[,facts]<-as.integer(retval[,facts]==tocomp)
	}
	colnames(retval)<-dfrConvData$newformdata$newcoln
	return(retval)
}
#	nc<-dim(dfr)[2]
#	firstRow<-dfr[1,]
#	coln<-colnames(dfr)
#	retval<-do.call(cbind, lapply(seq(nc), function(ci){
#			if(is.factor(firstRow[,ci]))
#			{
#				lvls<-levels(firstRow[,ci])[-1]
#				stretchedcols<-sapply(lvls, function(lvl){
#						rv<-dfr[,ci]==lvl
#						mode(rv)<-"integer"
#						return(rv)
#					})
#				if(!is.matrix(stretchedcols)){
#					stretchedcols<-matrix(stretchedcols, nrow=1)}
#				colnames(stretchedcols)<-paste(coln[ci], lvls, sep=betweenColAndLevel)
#				return(stretchedcols)
#			}
#			else
#			{
#				curcol<-matrix(dfr[,ci], ncol=1)
#				colnames(curcol)<-coln[ci]
#				return(curcol)
#			}
#		}))
#	rownames(retval)<-rownames(dfr)
#	return(retval)
#}

#given a set of dummy column names (see factorsToDummyVariables), try to find
#   the column from which it originates
#dfr: original dfr (or at least one with the original column names)
#betweenColAndLevel: in the name of the dummy columns, what comes between the
#   original column name and the column level
originalColumnNamesFromDummyVars<-function(dummyVarNames, dfr,
	betweenColAndLevel="")
{
	nc<-dim(dfr)[2]
	firstRow<-dfr[1,]
	coln<-colnames(dfr)
	mapping<-do.call(rbind, lapply(seq(nc), function(ci){
			if(is.factor(firstRow[,ci]))
			{
				lvls<-levels(firstRow[,ci])[-1]
				return(data.frame(org=coln[ci], new=paste(coln[ci], lvls,
					sep=betweenColAndLevel), stringsAsFactors=FALSE))
			}
			else
			{
				return(data.frame(org=coln[ci], new=coln[ci], stringsAsFactors=FALSE))
			}
		}))
#	#mapping$new==dummyVarNames
#	#print(mapping)
#	#note: we are supposing there are no two 'original' column names where
#	#	one is the start of the other.
#	mapping<-sapply(colnames(dfr), function(cn)
#		{
#			dumVN<-substr(dummyVarNames, 1, nchar(cn))
#			matches<-which(dumVN==cn)
#			if(length(matches)>0)
#			{
#				retval<-data.frame(orgname=rep(cn, length(matches)),
#					dummyname=dummyVarNames[matches])
#			}
#			else
#			{
#				retval<-NULL
#			}
#			retval
#		})
#	mapping<-do.call(rbind, mapping)
#	#print(mapping)

	unfoundDummies<-setdiff(dummyVarNames, mapping$new)
	if(length(unfoundDummies) > 0)
	{
		mapping<-rbind(mapping, data.frame(org=rep(NA, length(unfoundDummies)),
			new=unfoundDummies, stringsAsFactors=FALSE))
	}
	colnames(mapping)<-c("orgname", "dummyname") #backwards compatibility
	mapping$orgname<-as.character(mapping$orgname)
	mapping$dummyname<-as.character(mapping$dummyname)
	mapping$rest<-substr(mapping$dummyname, nchar(mapping$orgname)+1+nchar(betweenColAndLevel),
		nchar(mapping$dummyname))

	mapping
}

#Given a dataframe row (dr), return a dataframe holding nrOfTimes copies
#   of that row
repeatDataRow<-function(dr, nrOfTimes)
{
	dr[rep(1, nrOfTimes),]
}

#displays heat map for a matrix (R).
#ncolors: in how many pieces is the range of R values subdivided
#showLegend: if TRUE, adds a legend mapping colors to R values
#roundLegend: if the legend is shown, round R values up to so many digits
showNumericalHeatMap<-function(R, ncolors=2, showLegend=TRUE, roundLegend=2)
{
	if(showLegend)
	{
		layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4,1))
	}
	rg<-range(R)
	mn<-rg[1]-((rg[2] - rg[1])/10)
	mx<-rg[2]+((rg[2] - rg[1])/10)
	brks<-seq(mn, mx, length.out = ncolors+1)
	cat("Numerical heat map range:", mn, "-", mx, "gives breaks:", "\n")
	print(brks)
	n<-dim(R)[1]
	#Notice that image interprets the z matrix as a table of f(x[i], y[j]) values,
	#so that the x axis corresponds to row number and the y axis to column number,
	#with column 1 at the bottom, i.e. a 90 degree counter-clockwise rotation of
	#the conventional printed layout of a matrix.
	rv<-image(1:n, 1:n, t(R)[1:n,n:1], col = heat.colors(ncolors), breaks=brks,
		xlab=paste("1-", n, sep=""), ylab=paste("1-", n, sep=""))
	if(showLegend)
	{
		brks[1]<-rg[1]
		brks[length(brks)]<-rg[2]
		leg<-sapply(seq(ncolors), function(curbr)
			{
				paste(round(brks[curbr], roundLegend),
					round(brks[curbr+1], roundLegend), sep=" - ")
			})
		plot(0,0,type="n", axes = FALSE, frame.plot = FALSE)
		legend("topleft", legend=leg, text.col=heat.colors(ncolors), bg="black")
		layout(1)
	}
	invisible(rv)
}

#function to find significant testresults when controlling the FDR at alpha
HB<-function(p,alpha=0.05) {
	# p: vector with p-values
	# Hochberg and Benjamini procedure for FDR at level alpha
	o<-order(p) # indices of sorted p
	p<-sort(p)
	m<-length(p)
	boundary<-(1:m)*alpha/m
	condition<-(p<=boundary)
	reject<-NULL
  if(sum(condition)>0)#any are smaller than the boundaries
	{
    q<-max((1:m)[condition]) #biggest position smaller than its boundary
  	reject<-o[1:q] # indices of rejected null hypotheses
  }
  return(reject)
}

#Three types of 'significant' results: no correction, FDR (HB) and FWER
#		(Bonferroni)
#sa: dataframe or similar with one row holding p-values
#rown: row name/number where the p-values are
#alpha: control FDR or similar at alpha level
#showSig: if TRUE, the results show the significant values, otherwise the non-
#   significant values are returned
#Name: for display, see print.checkSig
checkSig<-function(sa, rown, alpha, showSig=TRUE, Name=rown)
{
	pvals<-sa[rown,]
	sgn<-ifelse(showSig, 1, -1)

	HBPValsWhere<-HB(pvals, alpha) #Hochberg-Benjamini correction (FDR)
	HBPVals<-pvals[sgn*HBPValsWhere]
	names(HBPVals)<-colnames(sa)[sgn*HBPValsWhere]

	n<-length(pvals)
	BFPValsWhere<-HB(pvals, alpha/n) #Bonferroni correction (FWER)
	BFPVals<-pvals[sgn*BFPValsWhere]
	names(BFPVals)<-colnames(sa)[sgn*BFPValsWhere]

	StdPValsWhere<-which(pvals<alpha) #Uncorrected
	StdPVals<-pvals[sgn*StdPValsWhere]
	names(StdPVals)<-colnames(sa)[sgn*StdPValsWhere]

	rv<-list(HBPVals=HBPVals, BFPVals=BFPVals, StdPVals=StdPVals, Name=Name,
		Alpha=alpha)
	class(rv)<-"checkSig"
	rv
}

#display function for checkSig class
print.checkSig<-function(x,...)
{
	printNamedVectorAsCol<-function(nv){print(unlist(nv))}
	cat("Significance checks for measure", x$Name, ", with alpha=", x$Alpha,
		":\n")
	cat("1) When using Hochberg-Benjamini correction (FDR):\n")
	printNamedVectorAsCol(x$HBPVals)
	cat("2) When using Bonferroni correction (FWER):\n")
	printNamedVectorAsCol(x$BFPVals)
	cat("3) When using NO correction:\n")
	printNamedVectorAsCol(x$StdPVals)
}

#Invert a symmetric matrix
#careful: if you're not entirely sure the matrix is positive definite (or other
#		problems with the matrix set this to TRUE. If cholesky decomposition fails,
#		it will use solve next (which might also fail though). Probably slower.
#silent: if TRUE, the error occurring is in cholesky is hidden from the user
invertSymmetric<-function(symMat, careful=FALSE, silent=FALSE)
{
	if(careful)
	{
		#I expect this to slow things down considerably!!
		inv<-tryRet(chol2inv(chol(symMat)), silent = silent, errRet = NULL)
		if(class(inv)=="try-error")
		{
			inv<-solve(symMat)
		}
		return(inv)
	}
	else
	{
		return(chol2inv(chol(symMat)))
	}
}

.addendum_debugmode <- function() {
  .debugging <- FALSE

  list(
    get = function() .debugging,
    set = function(value) .debugging <<- value
  )
}
.actual_debugmode <- .addendum_debugmode()
.isaddendumDebugging<-function(){.actual_debugmode$get()}

setDebugmodeAddendum<-function(doDebug=TRUE){
	oldDebug<-.actual_debugmode$get()
	.actual_debugmode$set(doDebug)
	invisible(oldDebug)
}
.debugtxt<-function(...)
{
	if(.isaddendumDebugging())
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
.debugprt<-function(...)
{
	if(.isaddendumDebugging())
	{
		cat("**D:\n")
		print(...)
	}
}


.addendum_extraprefix <- function() {
  .extraprefix <- "\t"

  list(
    get = function() .extraprefix,
    set = function(value) .extraprefix <<- value
  )
}
.actual_extraprefix <- .addendum_extraprefix()
.extraprefix<-function(){.actual_extraprefix$get()}

setExtraPrefix<-function(xp="\t"){
	oldP<-.actual_extraprefix$get()
	.actual_extraprefix$set(xp)
	invisible(oldP)
}


mapCleanItem<-function(pattern, useItem)
{
	retval<-list(pattern=pattern, useItem=useItem)
	class(retval)<-"mapCleanItem"
	return(retval)
}

matches<-function(pattern, applyTo) UseMethod("matches")
matches.mapCleanItem<-function(pattern, applyTo)
{
	rv<-grepl(pattern$pattern, applyTo[1])
#	if(rv) .debugtxt("Item '", applyTo[1], "' matched pattern '", pattern$pattern, "'")
	return(rv)
}
applyTransform<-function(transf, applyTo) UseMethod("applyTransform")
applyTransform.mapCleanItem<-function(transf, applyTo)
{
#	.debugtxt("Applying transform w useItem", transf$useItem)
	if(is.numeric(transf$useItem))
	{
		rv<-applyTo[transf$useItem]
	}
	else
	{
		rv<-transf$useItem
	}
#	.debugtxt("-->transformed to", rv)
	return(rv)
}

typicalCleanItemList<-function()
{
	list(
		mapCleanItem("(.*apply)", 3),
		mapCleanItem("(do\\.call)", 2),
		mapCleanItem("(function)|(doTryCatch)|(tryCatchOne)|(tryCatch)|(try)", "FUN"),
		mapCleanItem("(source)|(eval\\.with\\.vis)", "###")
	)
}

applyCleanItemMatch<-function(towhat, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1))
{
	matchpos<-match(TRUE, sapply(cils, matches, towhat))
	if(is.na(matchpos)) tfi<-defaultCI else tfi<-cils[[matchpos]]
	return(applyTransform(tfi, towhat))
}

clean_onecall<-function(xt, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1))
{
	z<-strsplit(paste(xt, collapse="\t"), "\t")[[1]]
#	.debugtxt("z=", z)
	applyCleanItemMatch(z, cils=cils, defaultCI=defaultCI)
}

clean_cs <- function(x, cils=typicalCleanItemList(), defaultCI=mapCleanItem("(.*)", 1))
{
  val <- sapply(x, clean_onecall, cils, defaultCI)
#  .debugtxt("Cleaned up cs: ", val)
  val[grepl("\\<function\\>", val)] <- "FUN"
#  .debugtxt("Post <function>: ", val)
  val <- val[!grepl("(###|FUN)", val)]
#  .debugtxt("Post (###|FUN): ", val)
	unlist(val)
}

curfntester<-function()
{
	for(i in sys.parents())
	{
		cat("Working on", i, "\n")
		curcall<-sys.call(sys.parent(n=i))
		cat("curcall object:\n")
		print(curcall)
		mcall<-tryRet(match.call(call=curcall), silent=TRUE, errRet=list("No_Call"))
		cat("match.call object:\n")
		print(mcall)
		cat("match.call structure:\n")
		str(mcall)
		cat("probable name:'", mcall[[1]] , "'\n")
	}
}

curfnfinder<-function(skipframes=0, cils=typicalCleanItemList(),
	defaultCI=mapCleanItem("(.*)", 1), retIfNone="Not in function",
	retStack=FALSE, extraPrefPerLevel="\t")
{
	cs<-sys.calls()
	cs<-head(cs, -(skipframes+1))
	ccs<-clean_cs(cs, cils, defaultCI)
	#.debugtxt("After clean up of call stack:", ccs)
	if(length(ccs)==0)
	{
		return(retIfNone)
	}
	else if(retStack)
	{
		return(paste(ccs, collapse = extraPrefPerLevel))
	}
	else
	{
		rv<-ccs[length(ccs)] #last item
		if(length(ccs) > 1) rv<-paste(  paste(rep(extraPrefPerLevel, length(ccs)-1), collapse=""), rv, sep="")
		return(rv)
	}
}

.curfnfinder_old<-function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
	retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
	#.debugtxt("skipnames: ", skipnames)
	prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
			currv<-sys.call(sys.parent(n=i))[[1]]
			tocat<-currv
			if(is.function(tocat))
			{
#				debug.closure<<-currv
#				debug.sp<<-sys.parent(n=i)
#				debug.sc<<-sys.call(sys.parent(n=i))
#				debug.mc<-match.call(call=debug.sc)
				tocat<-"unknown function (closure)"
			}
			.debugtxt("*curfnfinder* for item", i, tocat)
			return(currv)
		})
	prefix[grep(skipnames, prefix)] <- NULL
	prefix<-gsub("function \\(.*", "do.call", prefix)
	if(length(prefix)==0)
	{
		return(retIfNone)
	}
	else if(retStack)
	{
		return(paste(rev(prefix), collapse = "|"))
	}
	else
	{
		retval<-as.character(unlist(prefix[1]))
		if(length(prefix) > 1)
		{
			retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
		}
		return(retval)
	}
}

#similar as cat, but appends current system time + a newline
catt<-function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
	append = FALSE)
{
	cat(..., format(Sys.time(), "(%Y-%m-%d %H:%M:%S)"), "\n", file = file, 
		sep = sep, fill = fill, labels = labels, append = append)
}

catw<-function(..., prefix=0, showCS=FALSE)
{
	if(is.numeric(prefix))
	{
		if(showCS)
		{
			prefix<-curfnfinder(skipframes=prefix+1, retStack=TRUE, extraPrefPerLevel="|") #note: the +1 is there to avoid returning catw
		}
		else
		{
			prefix<-curfnfinder(skipframes=prefix+1, extraPrefPerLevel=.extraprefix()) #note: the +1 is there to avoid returning catw
		}
		prefix<-paste(prefix, ":", sep="")
	}
	catt(prefix, ...)
}

#only output if condition is TRUE
catif<-function(cond=TRUE, ...)
{
	if(cond)
	{
		cat(...)
	}
}

#Combination of catif and catt
cattif<-function(cond=TRUE, ...)
{
	if(cond)
	{
		catt(...)
	}
}

catwif<-function(cond=TRUE, ..., prefix=0, showCS=FALSE)
{
	if(cond)
	{
		catw(..., prefix=prefix+1, showCS=showCS)
	}
}

#cattif(TRUE, "test", "hallo", c(1,2,3))

#only print if condition == TRUE
printif<-function(cond=TRUE, ...)
{
	if(cond)
	{
		print(...)
	}
}

#get colors for a set of values to display them
#x: values to translate to color
#mini: value that refers to the lowest possible value, i.e. the 'smallest' color
#maxi:...
#minFirstColor: minimum value for the first color in the gradient
#		(between 0 and 1)
#maxSecondColor: maximum value for the second color in the gradient 
#   (between 0 and 1)
#constThirdColor: third color is held constant at this value (between 0 and 1)
#returnAsString: if TRUE, return #00FF00 style, otherwise return rgb color
#orderColors: can hold any order of the 3 "r", "g" and "b" -> first, second and
#		third colors above
#verbosity: if > 0: display information on forming the colors, mainly for
#   debugging
getColorRange<-function(x, mini=min(x), maxi=max(x), minFirstColor=0.5,
	maxSecondColor=0.5, constThirdColor=0, returnAsString=FALSE,
	orderColors=c("r", "g", "b"), verbosity=0)
{
	if(any(x < mini))
	{
		stop("values are not allowed to be smaller than the minimum (", mini, ")")
	}
	if(any(x > maxi))
	{
		stop("values are not allowed to be bigger than the maximum (", maxi, ")")
	}
	x.pctOfMaxMinMin<-(x - mini)/(maxi - mini)
	fcFact<-1-minFirstColor
	on1Scale<-cbind(fcFact*x.pctOfMaxMinMin+minFirstColor,
		maxSecondColor*(1-x.pctOfMaxMinMin), constThirdColor)
	cattif(verbosity>0, "Color values on scale 1:")
	if(verbosity > 0)
	{
		print(on1Scale)
	}
	rOn1Scale<-on1Scale[,orderColors=="r"]
	gOn1Scale<-on1Scale[,orderColors=="g"]
	bOn1Scale<-on1Scale[,orderColors=="b"]
	cattif(verbosity>0, "Color values on scale 1, in order RGB:")
	if(verbosity > 0)
	{
		print(cbind(rOn1Scale,gOn1Scale,bOn1Scale))
	}
	#sprintf("%02X", 255)
	if(returnAsString)
	{
		r.s<-paste( sprintf("%02X", round(rOn1Scale*255)), sep="")
		g.s<-paste( sprintf("%02X", round(gOn1Scale*255)), sep="")
		b.s<-paste( sprintf("%02X", round(bOn1Scale*255)), sep="")
		return(paste("#", r.s, g.s, b.s, sep=""))
	}
	else
	{
		return(rgb(rOn1Scale, gOn1Scale, bOn1Scale, 1))
	}
}

#getColorRange(c(1,2,3), returnAsString=TRUE, orderColors=c("g", "b", "r"),
#	verbosity=1)

#find Direct incidences in a supposed order
#x: list holding objects to be compared (e.g. sets, models, ...)
#isAncestorOf: function that checks whether its lhs argument is an ancestor of
#   its rhs argument (i.e.: the order function)
#returnPosPairs:
#		* if "indices", return a matrix with two columns, child and parent, holding
#       indices within x
#		* if "items", return a list where each item is itself a list with members
#        child and parent, holding the items themselves
#   * otherwise, return a list where each item is itself a list with members
#         curx (actual child item) and parents (indices of parents within x)
#verbosity: if > 0, display progress (only passed to removeItems at this time)
findParentTree<-function(x, isAncestorOf=get(">"), returnPosPairs="indices",
	verbosity=0)
{
	allAncestors<-lapply(x, function(curx){
			list(curx=curx, ancestors=which(sapply(x, isAncestorOf, curx,
				USE.NAMES = FALSE)))
		})
	allParents<-lapply(allAncestors, function(ancestorsOfCurX){
			metaAncestorsOfCurX<-lapply(ancestorsOfCurX$ancestors,
				function(ancestorIndex){
					allAncestors[[ancestorIndex]]$ancestors
				})
			if(length(metaAncestorsOfCurX) > 0)
			{
				metaAncestorsOfCurX<-do.call(c, metaAncestorsOfCurX)
				metaAncestorsOfCurX<-unique(metaAncestorsOfCurX)
				list(curx=ancestorsOfCurX$curx,
					parents=removeItems(metaAncestorsOfCurX,ancestorsOfCurX$ancestors,
					verbose=(verbosity>1)))
			}
			else
			{
				list(curx=ancestorsOfCurX$curx, parents=NULL)
			}
		})
	if(! is.null(returnPosPairs))
	{
		if(returnPosPairs=="indices")
		{
			directLinks<-do.call(rbind, lapply(seq_along(allParents), function(curxi){
					if(length(allParents[[curxi]]$parents) > 0)
					{
						cbind(child=curxi, parent=unlist(allParents[[curxi]]$parents,
							use.names = FALSE))
					}
					else
					{
						return(NULL)
					}
				}))
			return(directLinks)
		}
		else if(returnPosPairs=="items")
		{
			directLinks<-do.call(c, lapply(allParents, function(parentsForCurX){
					lapply(parentsForCurX$parents, function(curParI){
							list(child=parentsForCurX$curx, parent=allParents[[curParI]]$curx)
						})
				}))
			return(directLinks)
		}
	}
	return(allParents)
}

#get a conservative estimate for the standard error of an AUC, given only AUC
seAUC<-function(AUCHat, nZero, nOne)
{
	#following Hanley en McNeil 1982
	Q1<-AUCHat/(2-AUCHat)
	Q2<-2*AUCHat^2 / (1+AUCHat)
	sqrt((AUCHat*(1-AUCHat) + (nZero-1)*(Q1 - AUCHat^2) +
		(nOne-1)*(Q2 - AUCHat^2)) / (nZero * nOne))
}

bootStrapAUC<-function(pos.scores, neg.scores, bootStrap)
{
	bs.pos<-sample(pos.scores,bootStrap,replace=TRUE)
	bs.neg<-sample(neg.scores,bootStrap,replace=TRUE)
	return(mean(bs.pos > bs.neg) + 0.5 * mean(bs.pos == bs.neg))
}

#calculate AUC for binary outcomes
#probs: (predicted) probabilities
#trueOnes: which items are really 'cases'
#trueZeroes: which items are really 'controls'
#bootStrap: if <= 0: use true observed AUC, else use given number of bootstrap
#   repetitions
#includeSE: if TRUE, also calculate the variance (note the misnamedness!!)
calcAUC.Binary<-function(probs, trueOnes, trueZeroes, bootStrap=0,
	includeSE=FALSE, verbosity=0) #note: does not accoun for NAs
{
	if(! is.logical(trueOnes))
	{
		#make it logical then
		trueOnes<-seq(length(probs)) %in% trueOnes
	}
	if(missing(trueZeroes))
	{
		trueZeroes<-!trueOnes
	}
	else
	{
		if(! is.logical(trueZeroes))
		{
			#make it logical then
			trueZeroes<-seq(length(probs)) %in% trueZeroes
		}
	}
	if(verbosity > 0)
	{
		catt("true zeroes:")
		print(trueZeroes)
		catt("true ones:")
		print(trueOnes)
	}
	NumTZ<-sum(trueZeroes)
	NumTO<-sum(trueOnes)
	if((NumTZ==0) | (NumTO==0))
	{
		AUC<-NA
		varAUC<-NA
	}
	else
	{
		if(bootStrap > 0)
		{
			pos.scores<-probs[trueOnes]
			neg.scores<-probs[trueZeroes]
			cattif(verbosity > 0, "For bootstrapping(", bootStrap, "); pos.scores / neg.scores:")
			if(verbosity > 0)
			{
				print(pos.scores)
				print(neg.scores)
			}
			#bs.pos<-sample(pos.scores,bootStrap,replace=T)
			#bs.neg<-sample(neg.scores,bootStrap,replace=T)
			AUC<-bootStrapAUC(pos.scores, neg.scores, bootStrap)#mean(bs.pos > bs.neg) + 0.5 * mean(bs.pos == bs.neg)
			if(includeSE)
			{
				aucs = replicate(1000,bootStrapAUC(pos.scores, neg.scores, bootStrap))
				varAUC<-var(aucs)
				if(varAUC==0)
				{
					#this should not turn up zero, and is somewhat conservative,
					#i.e. greater variance than occurs
					varAUC<-(seAUC(AUC, nZero=NumTZ, nOne=NumTO))^2
				}
			}
		}
		else
		{
			AUC<-sum(sapply(probs[trueZeroes], function(curControlProb)
				{
					numCaseBigger<-sum(probs[trueOnes] > curControlProb)
					numCaseEqual<-sum(probs[trueOnes] == curControlProb)
					numCaseBigger + (numCaseEqual/2)
				})) / ( (NumTZ) * (NumTO) )
			if(includeSE)
			{
				varAUC<-(seAUC(AUC, nZero=NumTZ, nOne=NumTO))^2
			}
		}
	}
	if(includeSE)
	{
		return(list(AUC=AUC, varAUC=varAUC))
	}
	else
	{
		return(AUC)
	}
}

#Given a vector of criteria (crit) and variances (varcrit) for these, find the
#index of the biggest item that is below the maximum criterion - a given times
#its se
#crit: vector of criterion values (e.g. AUCs)
#varcrit: known/estimated variance for each of the criterion values
#findMaxOf: for each item in crit, after subsetting for valid values based on 
#   crit, this is the value that is maximized within the remaining set. e.g. in 
#   glmnet find the smallest lambda that has AUC below maxAUC-seAUC. Note: if 
#   you want the smallest value just negate the values...
#timesSE: how many times should the SE be subtracted from the highest crit 
#   (could)be zero!!
#verbosity: if >0, progress info is displayed
pickBestConservative<-function(crit, varcrit, findMaxOf=crit, timesSE=1,
	verbosity=0)
{
	#finds the biggest findMaxOf value (i.e.: its index) for which the crit
	#is below or equal to (the maximum crit - its se * timesSE)
	if(length(crit) != length(varcrit)){
		stop("crit and varcrit should be of equal length")}
	if(length(crit) != length(findMaxOf)){
		stop("crit and findMaxOf should be of equal length")}
	mi<-which.max(crit)
	cattif(verbosity > 0, "Original criterium maximum found at position:", mi,
		", value:", crit[mi])
	thres<-crit[mi] - timesSE * sqrt(abs(varcrit[mi]))
	cattif(verbosity > 0, "Criterium threshold:", thres)
	mayberes<-which(crit <= thres)
	if(length(mayberes)==0)
	{
		cattif(verbosity > 0,
			"None were smaller in criterium, so returning the minimum:",
			crit[which.min(crit)], "at position", which.min(crit))
		return(which.min(crit)) #emergency escape
	}
	cattif(verbosity > 0,
		"The following indexes had smaller/equal values for the criterion:\n",
		mayberes)
	truecandidates<-findMaxOf[mayberes] <= findMaxOf[mi]
	if(sum(truecandidates) > 0)
	{
		mayberes<-mayberes[truecandidates]
		cattif(verbosity > 0,
			"Of these, the following had smaller findMaxOf:\n",
			mayberes)
	}
	else
	{
		cattif(verbosity > 0,
			"None were smaller in criterium and smaller in findMaxOf, so returning the minimum:",
			crit[which.min(crit)], "at position", which.min(crit))
		return(which.min(crit)) #emergency escape
	}
	subindex<-which.max(findMaxOf[mayberes])
	cattif(verbosity > 0, "The maximum value for findMaxOf was found at the:",
		subindex, "th position of these, i.e.:", mayberes[subindex], "with value:",
		findMaxOf[mayberes[subindex]])
	return(mayberes[subindex])
}

plot4d<-function(x,y,z, u, main="", xlab="", ylab="", zlab="", ulab="")
{
	require(rgl)#may need to install this package first

	#standard trick to get some intensity colors
	uLim<-range(u)
	uLen<-uLim[2] - uLim[1] + 1
	colorlut<-terrain.colors(uLen)
	col<-colorlut[u - uLim[1] + 1]

	open3d()#Open new device
	points3d(x=x, y=y, z=z,  col=col)
	aspect3d(x=1, y=1, z=1) #ensure bounding box is in cube-form
		#(scaling variables)
	#note: if you want to flip an axis, use -1 in the statement above

	axes3d() #Show axes
	title3d(main = main, sub=paste("Green is low", ulab, ", red is high"),
		xlab = xlab, ylab = ylab, zlab = zlab)
	invisible()
}

#makes it easy to generalize showNonZeroCoef below
coef.Matrix<-function(object,s=dim(object)[2],...)
{
	#cat("Hoorah my coef\n")
	object[,s,drop=FALSE]
}

showNonZeroCoef<-function(x, ...) UseMethod("showNonZeroCoef")
showNonZeroCoef.default<-function(x, atLeast=1, thres=0.00001,s=NULL,...)
{
	#typically used for lists, note that the form in the else
	#may lead to infinite recursion in some cases.
	#This should not happen in glmnet (where the call will be
	#dispatched to the Matrix version
	require(Matrix)
	#cat("default\n")
	if(is.list(x))
	{
		#assume they are all of the same length
		if(is.null(s))
		{
			resList<-lapply(x, coef,...) #allow default to kick in!
		}
		else
		{
			resList<-lapply(x, coef, s,...)
		}
		#cat("resList:\n")
		#print(resList)
		fullx<-do.call(cBind, resList)
		#cat("fullx:\n")
		#print(fullx)
		rv<-showNonZeroCoef(fullx,atLeast=atLeast, thres=thres)
		colnames(rv)<-names(x)
		rv
	}
	else
	{
		showNonZeroCoef(coef(x,...),atLeast=atLeast, thres=thres)
	}
}
showNonZeroCoef.glmnet<-function(x, atLeast=1, thres=0.00001,...)
{
	#cat("glmnet\n")
	showNonZeroCoef(coef(x,...),atLeast=atLeast, thres=thres)
}
showNonZeroCoef.cv.glmnet<-function(x, atLeast=1, thres=0.00001,
	s="lambda.1se",...)
{
	#cat("cv.glmnet\n")
	if(s=="lambda.all")
	{
		showNonZeroCoef(x$glmnet.fit,...)
	}
	else
	{
		showNonZeroCoef(coef(x,s=s,...),atLeast=atLeast, thres=thres)
	}
}
showNonZeroCoef.Matrix<-function(x, atLeast=1, thres=0.00001,...)
{
	#cat("Matrix\n")
	numPosPerCoef<-sapply(seq(dim(x)[1]), function(rn){
		sum(abs(as.vector(x[rn,])) > thres)})
	#cat("numPosPerCoef\n")
	#print(numPosPerCoef)
	#cat("atLeast: ", atLeast, "\n")
	showWhich<-which(numPosPerCoef >= atLeast)
	#cat("showWhich\n")
	#print(showWhich)
	x[showWhich,]
}

displayLinearFromNZC<-function(nZeroCoef, SEs, roundDigits)
{
	if(length(dim(nZeroCoef)) > 1)#also works if nZeroCoef has no dim (e.g. a vector)
	{
		nms<-colnames(nZeroCoef)
		vals<-unlist(nZeroCoef[1,])
	}
	else
	{
		nms<-names(nZeroCoef)
		vals<-unlist(nZeroCoef)
	}
	if(! is.null(SEs))
	{
		if(length(dim(SEs)) > 1)#also works if SEs has no dim (e.g. a vector)
		{
			sess<-unlist(SEs[1,])
		}
		else
		{
			sess<-unlist(SEs)
		}
		sess<-abs(round(1.96*sess, roundDigits))#not perfect, but close enough for most purposes
		sespart<-ifelse(is.na(sess), "", paste("(+/-", sess, ")", sep="") )
	}
	else
	{
		sespart<-rep("", length(vals) )
	}
	seps<-ifelse(vals<0, " -", " +")
	if(seps[1]==" +") seps[1]<-""
	vals<-abs(round(vals, roundDigits))
	nms[nms=="(Intercept)"]<-""

	return(paste(seps, vals, sespart , nms, sep=" ", collapse=""))
}

#ripped this from glmnet to be able to use it directly
gn.error.bars<-function (x, upper, lower, width = 0.02, ...)
{
	xlim <- range(x)
	barw <- diff(xlim) * width
	segments(x, upper, x, lower, ...)
	segments(x - barw, upper, x + barw, upper, ...)
	segments(x - barw, lower, x + barw, lower, ...)
	range(upper, lower)
}

#similar to the start of plot.cv.glmnet
getXIndices<-function(cvobj, xvar=c("norm", "lambda", "dev"), verbosity=0)
{
	require(glmnet)
	xvar = match.arg(xvar)
  switch(xvar, norm = {
		orgBeta<-getBeta(cvobj)
	  whichNZ = nonzeroCoef(orgBeta)#it appears there is a
			#	nonzeroCoef in glmnet!!!
	  theBeta = as.matrix(orgBeta[whichNZ, ])
		index <- apply(abs(theBeta), 2, sum)
  }, lambda = {
		index <- log(cvobj$lambda)
  }, dev = {
		index <- cvobj$glmnet.fit$dev.ratio
  })
  return(index)
}

getBeta<-function(object) UseMethod("getBeta")
getBeta.cv.glmnet<-function(object)
{
	object$glmnet.fit$beta
}
getBeta.glmnet<-function(object)
{
	object$beta
}

#add the crossvalidation plot to a recently created coefficient plot for glmnet
addCVPlot<-function(cvobj, xvar=c("norm", "lambda", "dev"), numTicks,
	smoothed=FALSE, errorbarcolor="darkgrey", centercolor="red",
	fillsidecolor="#0000ff22", verbosity=0)
{
	require(glmnet)
 	x<-getXIndices(cvobj, xvar=xvar)
	#yrange<-range(coef(cvobj$glmnet.fit))
	yrange<-par("yaxp")[1:2] #current outer limits of the axis
	truerange<-range(c(cvobj$cvup, cvobj$cvlo)) #outer limits of the true value
	scaleFact<-(yrange[2] - yrange[1])/(truerange[2] - truerange[1])
	yvalue<-function(untrans){yrange[1] + (untrans - truerange[1]) * scaleFact  }
	
	ioflambda.min<-match(cvobj$lambda.min, cvobj$lambda)
	ioflambda.1se<-match(cvobj$lambda.1se, cvobj$lambda)
	isofinterest<-sort(unique(ioflambda.min, ioflambda.1se))
	
	if(smoothed)
	{
		#let's first get the smoothed values.
		tmpdata<-data.frame(x=x, y=cvobj$cvm, yt=cvobj$cvup, yl=cvobj$cvlo)
		tmpwts<-1/((cvobj$cvsd)^2)
		res.lo<-loess(y~x, data=tmpdata, weights=tmpwts)
		tmpx<-seq(min(x), max(x), length.out=100)
		tmpres<-predict(res.lo, data.frame(x=tmpx), se=TRUE)
		tmpy<-yvalue(tmpres$fit)
		tmptop<-yvalue(tmpres$fit+tmpres$se)
		tmpse<-tmptop-tmpy
		tmpbot<-tmpy-tmpse

		#play extra safe: smooth the 'top' + add the se from the smoothing and
		#similar to the bottom
		tmptruetop<-predict(loess(yt~x, data=tmpdata), data.frame(x=tmpx), se=TRUE)#note: don't use the weights!
		tmptruebot<-predict(loess(yl~x, data=tmpdata), data.frame(x=tmpx), se=TRUE)#note: don't use the weights!
		tmptruetop<-tmptruetop$fit + tmptruetop$se
		tmptruebot<-tmptruebot$fit - tmptruebot$se
		struetop<-yvalue(tmptruetop)
		struebot<-yvalue(tmptruebot)

		scaledOut<-data.frame(x=tmpx, y=tmpres$fit, se=tmpres$se, scaledy=tmpy,
			scaledtop=tmptop, scaledbot=tmpbot, truetop=tmptruetop, truebot=tmptruebot,
			scaledtruetop=struetop, scaledtruebot=struebot)
		lines(tmpx, tmpy, col=centercolor, lty="dashed")
		lines(tmpx, struetop, col=errorbarcolor, lty="dashed")
		lines(tmpx, struebot, col=errorbarcolor, lty="dashed")
		polygon(c(tmpx, rev(tmpx)), c(struetop, rev(struebot)), border=NA, col=fillsidecolor)
		points(x[isofinterest], yvalue(cvobj$cvm[isofinterest]), pch = 20, col = centercolor)
	}
	else
	{
		scaledOut<-data.frame(cvup=yvalue(cvobj$cvup), cvlo=yvalue(cvobj$cvlo),
			cvm=yvalue(cvobj$cvm))
	  gn.error.bars(x, scaledOut$cvup, scaledOut$cvlo, width = 0.01,
			col = errorbarcolor)
	  points(x, scaledOut$cvm, pch = 20, col = centercolor)
	}
  axis(side = 4, at = seq(yrange[1], yrange[2], length.out=numTicks),
		labels = paste(round(seq(truerange[1], truerange[2], length.out=numTicks),
			2)),
		tick = TRUE, line = 0)
  abline(v = x[ioflambda.min], lty = 3)
  abline(v = x[ioflambda.1se], lty = 3)
	
	abline(h = scaledOut$cvlo[ioflambda.min], lty = 3)
  invisible(scaledOut)
}

simpleplot<-function(object,..., verbosity=0) UseMethod("simpleplot")
simpleplot.default<-function(object,..., verbosity=0) plot(object, ...)
simpleplot.cv.glmnet<-function(object,..., verbosity=0) plot(object$glmnet.fit, ...)

firstRepeatedAppearance<-function(cvobj, repsNeeded)
{
	if(repsNeeded < 1) repsNeeded<-1
	exNeeded<-repsNeeded-1
	theBeta<-getBeta(cvobj)
	firstAppearance<-apply(theBeta, 1, function(rw){
			occurring<-c(abs(rw) > 0, rep(TRUE,exNeeded))
			match(TRUE, sapply(seq(ncol(theBeta)), function(i){all(occurring[(i):(i+exNeeded)])}))
		})
	firstAppearance<-firstAppearance[!is.na(firstAppearance)]
	return(firstAppearance)
}

#combine the glmnet plot with the crossvalidation plot
plotex<-function(cvobj, xvar=c("norm", "lambda", "dev"), numTicks=5,
	lamIndexAxisCol="red", lamIndexAxisPos=NULL, legendPos="topright",
	legendCex=0.5, legendOf=20, smoothCV=FALSE, errorbarcolor="darkgrey",
	centercolor="red", fillsidecolor="#0000ff22", repsNeededForFirstOccurrence=3,
	..., verbosity=0)
{
	catwif(verbosity>0, "simple glmnet plot")
	simpleplot(cvobj, xvar, ..., verbosity=verbosity-1)
	catwif(verbosity>0, "adding cross validation plot")
	cvpsc<-addCVPlot(cvobj, xvar=xvar, numTicks=numTicks, smoothed=smoothCV,
		errorbarcolor=errorbarcolor, centercolor=centercolor,
		fillsidecolor=fillsidecolor, verbosity=verbosity-1)
	if(! is.null(lamIndexAxisCol))
	{
		catwif(verbosity>0, "adding lambda index axis")
		if(missing(lamIndexAxisPos)) lamIndexAxisPos<-par("yaxp")[1]
		addLamIndexAxis(cvobj, xvar=xvar, numTicks=numTicks,side=3,
			pos=lamIndexAxisPos, col=lamIndexAxisCol)
	}

	if(!is.null(legendPos))
	{
		firstAppearance<-firstRepeatedAppearance(cvobj, repsNeededForFirstOccurrence)
#		apply(theBeta, 1, function(rw){match(TRUE, abs(rw) > 0)})
#		firstAppearance<-firstAppearance[!is.na(firstAppearance)]
		orderOfFirstAppearance<-order(firstAppearance)[1:legendOf]
		whereAppearing<-firstAppearance[orderOfFirstAppearance]
		legendForVars<-names(firstAppearance)[orderOfFirstAppearance]
		namesAsInPlotCoef<-getBeta(cvobj)
		namesAsInPlotCoef<-rownames(namesAsInPlotCoef)[nonzeroCoef(namesAsInPlotCoef)]
		whereAsInPlotCoef<-match(legendForVars, namesAsInPlotCoef)
		cols<-rep(1:6, length.out=max(orderOfFirstAppearance))
#    catw("first 21 'column' items for legend:\n\t", names(firstAppearance)[1:21])
		useColors<-cols[whereAsInPlotCoef]
#		catwif(verbosity > 1, "useColors:")
#		printif(verbosity > 1, data.frame(varname=legendForVars, itemnr=orderOfFirstAppearance, color=useColors))
		legendForVars<-paste(legendForVars, " (", whereAppearing, ")", sep="")
		legend(legendPos, legend=legendForVars, text.col=useColors, cex=legendCex)
	}
	invisible(cvpsc)
}

addLamIndexAxis<-function(cvobj, xvar=c("norm", "lambda", "dev"), numTicks=5,...)
{
	ioflambda.min<-match(cvobj$lambda.min, cvobj$lambda)
	ioflambda.1se<-match(cvobj$lambda.1se, cvobj$lambda)
	isofinterest<-sort(c(ioflambda.min, ioflambda.1se))
	catw("Enforcing added indices", ioflambda.min, "and", ioflambda.1se)
 	x<-getXIndices(cvobj, xvar=xvar)
#   atdf = sort(unique(c(pretty(x), x[c(ioflambda.min, ioflambda.1se)])))
#   prettydf = trunc(approx(x = x, y = cvobj$df, xout = atdf, rule = 2)$y)
#   axis(3, at = atdf, label = prettydf, tcl = NA)
	
	useXforDF<-x[isofinterest]
	useDF<-trunc(approx(x = x, y = cvobj$glmnet.fit$df, xout = useXforDF, rule = 2)$y)
	#useDF<-cvobj$df[isofinterest]
	axis(3, at = useXforDF, label = useDF, tcl = NA)
	
 	useIndex<-as.integer(seq(from=1, to=length(x), length.out=numTicks))
	useIndex<-sort(unique(c(useIndex, ioflambda.min, ioflambda.1se)))
 	useX<-x[useIndex]
  axis(4, at = useX, labels = paste(useIndex), ...)
}

colsOfType<-function(dfr, type=c("factor", "char"))
{
	useFunc<-function(cl){sum(is.na(cl))}
	if(is.function(type))
	{
		useFunc<-type
	}
	else if(type=="factor")
	{
		useFunc<-is.factor
	}
	else if(type=="char")
	{
		useFunc<-is.character
	}
	#print(useFunc)
	if(inherits(dfr, "data.frame"))
	{
		sapply(dfr, useFunc)
	}
	else
	{
		numFacts<-sapply(seq(dim(dfr)[2]), function(curcn){useFunc(dfr[1,curcn])})
		return(numFacts)
	}
}


refactor<-function(x, labels)
{
	#assumes x is a vector of integers within the range of labels
	factor(x, levels=seq_along(labels), labels=labels)
}

#When you have a list of dataframes of the same format, and you want to combine
#them (as rows) into one...
#Not pretty, but a lot better than rbind!
combineSimilarDfrList<-function(dfrlst) UseMethod("combineSimilarDfrList")
combineSimilarDfrList.default<-function(dfrlst){do.call(rbind, dfrlst)}
combineSimilarDfrList.data.frame<-function(dfrlst)
{#http://stackoverflow.com/questions/5980240/performance-of-rbind-data-frame
	tempRes <- lapply(dfrlst, dfr2mat)
	result<-as.data.frame(do.call(rbind, tempRes))
	a <- dfrlst[[1]]
	f <- which(sapply(a, class)=="factor")
	#f <- which(sapply(a, is.factor))
	for(i in f) {
	  lev <- levels(a[[i]])
	  result[[i]] <- factor(result[[i]], levels=seq_along(lev), labels=lev)
	}
	colnames(result)<-colnames(dfrlst[[1]])
	return(result)
}

#older version of this
combineSimilarDfrList_prev<-function(dfrlst)
{#http://stackoverflow.com/questions/5980240/performance-of-rbind-data-frame
	n <- 1:ncol(dfrlst[[1]])
	names(n) <- names(dfrlst[[1]])
	as.data.frame(lapply(n, function(i) unlist(lapply(dfrlst, `[[`, i))))
}

naPos<-function(dfr)
{
	percol<-lapply(dfr, function(cl){which(is.na(cl))})
	maybenames<-colnames(dfr)
	do.call(rbind, lapply(names(percol), function(nm){
			curcolrows<-unlist(percol[nm])
			repcol<-rep(match(nm, maybenames), length(curcolrows))
			cbind(cl=repcol, rw=curcolrows)
		}))
}

similarSizeGroups<-function(ngroups, nobs, rand=TRUE)
{
	repsPerPart<-rep(nobs %/% ngroups, ngroups)
	rem<-nobs %% ngroups
	if(rem > 0)
	{
		repsPerPart[seq(rem)]<-repsPerPart[seq(rem)]+1
	}
	if(rand)
	{
		return(sample(rep(seq(ngroups), repsPerPart), size=nobs))
	}
	else
	{
		return(repsPerPart)
	}
}

#typical use: patternNumericCols = "s(X)"
#will result in model text like outCol ~ s(firstColName) + s(ThirdColName)
# + SecondColName
#if only the second column is nonnumeric.
basisExpansionFormula<-function(dfr, outCol, patternNumericCols="X",
	subPatternCol="X", fixed=TRUE, onlyCols=NULL,
	allowDot=(!is.null(onlyCols)),...)
{
	if(allowDot)
	{
		if((patternNumericCols=="X") | (patternNumericCols=="."))
		{
			#rhs<-paste(removeItems(outcol, colnames(dfr)), collapse=" + ")
			rhs<-"."
			return(paste(outCol, rhs, sep=" ~ "))
		}
	}
	isnumcol<-sapply(dfr, is.numeric)
	numCols<-colnames(dfr)[isnumcol]
	otherCols<-colnames(dfr)[!isnumcol]
	numCols<-removeItems(outCol, numCols)
	otherCols<-removeItems(outCol, otherCols)
	if(! is.null(onlyCols))
	{
		numCols<-intersect(numCols, onlyCols)
		otherCols<-intersect(otherCols, onlyCols)
	}
	if(length(numCols) > 0)
	{
		dblPattern1<-paste(subPatternCol, subPatternCol, sep=",") #X,X
		dblPattern2<-paste(subPatternCol, subPatternCol, sep=", ") #X, X
		#if these are found, replace this by all comma-separated numeric variables
		#e.g. for te(X, X, k=5) -> becomes te(X1, X2, X3, X4, k=5)
		if(grepl(dblPattern1, patternNumericCols, fixed=fixed, ...))
		{
			allNumCols<-paste(numCols, collapse=", ")
			convertedNumCols<-gsub(dblPattern1, allNumCols, patternNumericCols,
				fixed=fixed,...)
		}
		else if(grepl(dblPattern2, patternNumericCols, fixed=fixed, ...))
		{
			allNumCols<-paste(numCols, collapse=", ")
			convertedNumCols<-gsub(dblPattern2, allNumCols, patternNumericCols,
				fixed=fixed,...)
		}
		else
		{
			convertedNumCols<-sapply(numCols, function(cc){
					gsub(subPatternCol, cc, patternNumericCols, fixed=fixed,...)
				})
		}
	}
	else
	{
		convertedNumCols<-NULL
	}
	rhs<-paste(c(convertedNumCols, otherCols), collapse=" + ")
	if(gsub("[[:space:]]", "", rhs) == "") rhs<-"1" #ensure a valid formula
	return(paste(outCol, rhs, sep=" ~ "))
}

getTimeOut<-function(toname="usedTimeOut")
{
	mget(toname, envir=globalenv(), ifnotfound=0)[[1]]
}
setTimeOut<-function(to, toname="usedTimeOut")
{
	invisible(assign(toname, to, envir=globalenv()))
}

#setTimeOut(120)
#getTimeOut()

#testt<-system.time({for(i in 1:1000) cat(i, "\n")})
stopIfRanTooLong<-function(expr, to=getTimeOut(), verbosity=0)
{
	if(verbosity > 0) cat("stopIfRanTooLong with timeout: ", to, "\n")
	st<-system.time({
			rv<-eval(expr, envir=parent.frame())
		})
	if((to > 0) && (tshort(st)[3] >= to)){
		stop("Process took too long, so skipping the rest")}
	invisible(rv)
}




corWithCat<-function(dfr,...)
{
	facts<-which(sapply(dfr, is.factor))
	for(i in facts)
	{
		dfr[[i]]<-as.integer(dfr[[i]])
	}
	cor(dfr,...)
}

corWithoutCat<-function(dfr,...)
{
	facts<-which(sapply(dfr, is.factor))
	if(length(facts) > 0)
	{
		dfr<-dfr[,-facts]
	}
	cor(dfr,...)
}










simpleDescription<-function(xparlist, depth=-1)
{
	xparpart<-sapply(seq_along(xparlist), function(curpari){
			if(is.function(xparlist[[curpari]]))
			{
				return(paste("function:", names(xparlist)[curpari]))
			}
			else
			{
				curprt<-xparlist[[curpari]]
				if(! is.null(dim(curprt)))
				{
					curprttxt<-paste(class(curprt), " of dimensions (", dim(curprt)[1],
						", ", dim(curprt)[2], ")", sep="")
				}
				else if(is.list(curprt))
				{
					if(depth != 0)
					{
						curprttxt<-paste("[", simpleDescription(curprt, depth=depth-1),
							"]", sep="")
					}
					else
					{
						curprttxt<-paste("list of length ", length(curprt), sep="")
					}
				}
				else if(length(curprt) > 1)
				{
					curprttxt<-paste(class(curprt), " of length ", length(curprt), sep="")
				}
				else
				{
					curprttxt<-as.character(curprt)
				}

				paste(names(xparlist)[curpari], "=", curprttxt, sep="")
			}
		})
	paste(xparpart, collapse=", ")
}

showParList<-function(xparlist, depth=-1)
{
	invisible(cat(simpleDescription(xparlist, depth=depth), "\n"))
}










#Clean up an existing data.frame: make factor-like columns truly factors,
#and move all factor columns to the beginning of the data.frame
makeFactorsAndSetFrontal<-function(dfr, treatAsFactorWhenDiffValRate=0.05, verbosity=0)
{
	#Make specific columns factor
	#We look for columns that are character / have very few different values
	#and convert these to factors
	numObs<-dim(dfr)[1]
	numVars<-dim(dfr)[2]
	facts<-colsOfType(dfr, "factor")
	orgfacts<-which( facts)#    apply(dfr, 2, is.factor))
	orgchar<-which( colsOfType(dfr, "char"))#    apply(dfr, 2, is.character))
	rateDiffVals<-sapply(seq(numVars), function(vari){
			if(facts[vari])
			{
				return(length(levels(dfr[1, vari])))
			}
			else
			{
				fr<-factor(dfr[, vari])
				return(length(levels(fr[1])))
			}
		}) / numObs
	orgfewvals<-which(rateDiffVals < treatAsFactorWhenDiffValRate)
	if(verbosity > 0) orgfewnofact<-orgfewvals[!(orgfewvals %in% orgfacts)]#only for tracing
	newfactors<-unique(c(orgchar, orgfewvals))
	newfactors<-newfactors[!(newfactors %in% orgfacts)]
	if(verbosity > 0) cat("makeFactorsAndSetFrontal\n")
	if(verbosity > 0) cat("factors up front:", orgfacts, "(", length(orgfacts), ")", "\n")
	if(verbosity > 0) cat("charcols up front:", orgchar, "(", length(orgchar), ")", "\n")
	if(verbosity > 0) cat("columns with few diff values up front:", orgfewvals, "(", length(orgfewvals), ")", "\n")
	if(verbosity > 0) cat("columns with few diff values non-fact:", orgfewnofact, "(", length(orgfewnofact), ")", "\n")
	if(verbosity > 0) catt("new factors:", newfactors)
	for(i in newfactors)
	{
		dfr[,i]<-factor(dfr[,i])
	}
	#reorder columns if necessary
	allfacts<-sort(c(newfactors, orgfacts))
	restCols<-((1:numVars)[-allfacts])
	return(dfr[,c(allfacts,restCols)])
}

randomFillDS<-function(ds)
{
	#for all the categorical cols
	fakeData<-ds
	nrnas<-unlist(apply(fakeData, 2, function(curcol){sum(is.na(curcol))}))
	hasnas<-which(nrnas>0)
	for(i in hasnas)
	{
		nna<-nrnas[i]
		wherenas<-which(is.na(fakeData[,i]))
		wasChar<-FALSE
		if(is.character(fakeData[,i]))
		{
			fakeData[,i]<-factor(fakeData[,i])
			wasChar<-TRUE
		}
		if(is.factor(fakeData[,i]))
		{
			nlev<-length(levels(fakeData[,i]))
			fakeData[wherenas,i]<-levels(fakeData[,i])[sample.int(nlev, nna, replace=TRUE)]
			if(wasChar)
			{
				fakeData[,i]<-as.character(fakeData[,i])
			}
		}
		else
		{
			#for now: just use the mean for the missing values in this fake data
			colm<-mean(fakeData[,i], na.rm=TRUE)
			fakeData[wherenas,i]<-colm
		}
	}
	return(fakeData)
}

findCatColNums<-function(dfr) UseMethod("findCatColNums")
findCatColNums.data.frame<-function(dfr) {which(sapply(dfr, is.factor))}

colsAsNumericMatrix<-function(dfr) UseMethod("colsAsNumericMatrix")
colsAsNumericMatrix.default<-function(dfr) {as.numeric(dfr)}
colsAsNumericMatrix.data.frame<-function(dfr) {vapply(dfr, function(curcol){as.numeric(curcol)}, numeric(nrow(dfr)))}

marginalProbPerCat<-function(dfr)
{
	catCols<-findCatColNums(dfr)
	lapply(dfr[,catCols], function(curcol){
			tbl<-table(curcol)
			return(tbl/sum(tbl))
		})
}

randomString<-function(maxLength=100, minLength=1, alphabet=c(letters, LETTERS), separator="")
{
	minLength<-as.integer(max(minLength, 1))
	maxLength<-as.integer(max(maxLength, minLength))
	if(minLength == maxLength)
	{
		l<-minLength
	}
	else
	{
		l<-sample(minLength:maxLength, 1)
	}
	paste(sample(alphabet, l, replace=TRUE), collapse=separator)
}

randomStrings<-function(n, maxLength=100, minLength=1, alphabet=c(letters, LETTERS), separator="")
{
	replicate(n, randomString(maxLength=maxLength, minLength=minLength, alphabet=alphabet, separator=separator))
}

#note: if you do not expect there to be NA's in the data, pass na.becomes=NA, this
#   should be faster
categoricalUniqueIdentifiers<-function(dfr, separator=",", na.becomes="\\d+", verbosity=0)
{
	catwif(verbosity>0, "working for dfr of dimension:", dim(dfr))
	forCols<-findCatColNums(dfr)
	dfr<-colsAsNumericMatrix(dfr[,forCols])
	if(! is.na(na.becomes)) dfr[is.na(dfr)]<-na.becomes #this works!!
	rv<-apply(dfr, 1, paste, collapse=separator)
	return(rv)
}

#x: vector of characters containing identifiers, and may contain duplicates
#This function returns the set of unique identifiers and their first occurrence
# in x
#note: providing this as a separate function allows to optimize it later on
uniqueCharID<-function(x, needSort=FALSE, includeOccurrence=TRUE, impNr=1)
{
	stopifnot(is.character(x))
	if(needSort)
	{
		uniquex<-switch(impNr,sort(unique(x)),unique(sort(x)),rle(sort(x))$values)
	}
	else
	{
		uniquex<-switch(impNr,unique(x),unique(x),rle(sort(x))$values)
	}
	if(includeOccurrence)
	{
		return(list(uniquex=uniquex, firstOccurrence=match(uniquex, x)))
	}
	else
	{
		return(uniquex)
	}
	
}

#note: n is drawn uniformly between minn and maxn. For other purposes this may be unwanted.
randomProbabilities<-function(minn=2, maxn=minn)
{
	minn<-as.integer(max(minn, 2))
	maxn<-as.integer(max(maxn, minn))
	if(maxn == minn){n<-minn}else{n<-sample(minn:maxn, 1)}
	rv<-runif(n)
	rv/sum(rv)
}
#randomProbabilities(5,10)
#randomProbabilities(3)
#randomProbabilities()

randomCategoricalVector<-function(numObs, catProbs=rep(1/3,3), asFactor=FALSE, verbosity=0,...)
{
	if(is.function(catProbs))
	{
		cattif(verbosity>0, "Generate probabilities.")
		catProbs<-catProbs(...) #use the function to generate a vector of probabilities
		cattif(verbosity>0, "Resulting probabilities:\n\t", catProbs)
	}
	cattif(verbosity>0, "Number of items per category")
	numpercat<-rmultinom(1, numObs, catProbs)
	if(length(names(catProbs)) > 0){nms<-names(catProbs)}else{nms<-letters[seq(length(catProbs))]}
	names(catProbs)<-nms
	cattif(verbosity>0, "Draw them in order")
	sampledinorder<-rep(nms, numpercat)
	cattif(verbosity>0, "Shuffle them")
	rv<-sample(sampledinorder)
	if(asFactor) rv<-factor(rv)
	attr(rv, "probs")<-catProbs
	return(rv)
}
#randomCategoricalVector(20, catProbs=rep(1/3,3), verbosity=1)
#randomCategoricalVector(20, catProbs=randomProbabilities, verbosity=1, minn=3)

randomRandomNorm<-function(n, minmu=0, maxmu=minmu, minsig2=1, maxsig2=minsig2)
{
	maxmu<-max(maxmu, minmu)
	minsig2<-max(minsig2, 0)
	maxsig2<-max(maxsig2, minsig2)
	mu<-runif(1, minmu, maxmu)
	sig2<-runif(1, minsig2, maxsig2)
	rv<-rnorm(n, mean=mu, sd=sqrt(sig2))
	attr(rv, "mu")<-mu
	attr(rv, "sig2")<-sig2
	return(rv)
}
#randomRandomNorm(20)
#randomRandomNorm(20, minmu=-3, maxmu=3, minsig2=0, maxsig2=2)
#randomRandomNorm(20, minmu=-3, maxmu=3)

typicalRandomNorm<-function(n, absmu=10)
{
	absmu=abs(absmu)
	randomRandomNorm(n, minmu=-absmu, maxmu=absmu, minsig2=0, maxsig2=absmu)
}
#typicalRandomNorm(20)
#typicalRandomNorm(20, 0.5)
#typicalRandomNorm(20, 2)


#note: ... is only passed on to the function for the categorical variables, so
#   if you want extra params for continuous, you'll have to create a new function!
#You can also simply pass rnorm or similar as rcnt
generateTypicalIndependentDfr<-function(numCat, numCnt, numObs,
	catProbs=rep(1/3,3), rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=0,...)
{
	parmlist<-c(list(numObs=numObs, catProbs=catProbs, verbosity=verbosity-1), list(...))
	catcols<-replicate(numCat, do.call(randomCategoricalVector, parmlist))
	colnames(catcols)<-paste("cat", seq(numCat), sep="")
	cntcols<-replicate(numCnt, rcnt(numObs))
	colnames(cntcols)<-paste("cnt", seq(numCnt), sep="")
	rv<-data.frame(catcols, cntcols)
	if(doShuffle){rv<-rv[,sample(ncol(rv))]}
	return(rv)
}
#generateTypicalIndependentDfr(5,5,20,catProbs=randomProbabilities, verbosity=1, minn=2, maxn=4)

randomNA<-function(dfr, n, atMost=FALSE, tolerance=0.0001, verbosity=0)#maybe later use an "option" for tolerance??
{
	stopifnot(n>=0)
	nr<-nrow(dfr)
	totalAvailable<-nr*ncol(dfr)
	if(abs(as.integer(n)-n) > tolerance)
	{
		stopifnot(n < 1)		#a fraction is passed, so should be smaller than 1
		n<-as.integer(n*totalAvailable)
	}
	#n is now surely an integer between 0 and totalAvailable.
	to.set.na<-(sample.int(totalAvailable, n, replace=atMost))-1
	cattif(verbosity > 0, "length of positions chosen to be set NA:", length(to.set.na))
	for(pos in to.set.na)
	{
		curc<-(pos %/% nr)+1
		curr<-(pos %% nr)+1
		cattif(verbosity > 1, "set NA for position:", pos, ", i.e.:(", curr, ",", curc, ")")
		dfr[curr, curc]<-NA
	}
	dfr
}
#randomNA(iris, 50, verbosity=2)

display<-function(dfr) UseMethod("display")

display.default<-function(dfr)
{
	#invisible(edit(dfr))
	View(dfr)
}

dfr2mat<-function(dfr)
{
	orgnames<-colnames(dfr)
	dfr<-matrix(unlist(dfr), ncol=ncol(dfr)) #convert to matrix!!
	colnames(dfr)<-orgnames
	return(dfr)
}

toFactorCorrecting<-function(vr,crval="-9")
{
  if((! is.null(crval)) & (! is.na(crval))) vr[vr==crval]<-NA
  vr<-as.factor(as.character(vr))
  vr
}

toNumericCorrecting<-function(vr, crval="-9", replaceComma=TRUE)
{
  if((! is.null(crval)) & (! is.na(crval))) vr[vr==crval]<-NA
  vr<-as.character(vr)
  if(replaceComma) vr<-sub(",", ".", vr, fixed=TRUE)
  as.numeric(vr)
}

quickFactor<-function(x, labels)
{
	#should only be used if you know for certain that:
	#x holds integers between 1 and length(labels)
	if(! is.integer(x)) x<-as.integer(x)
	levels(x)<-labels
	class(x)<-"factor"
	x
}

#may not be the most efficient implementation (certainly not in memory)
#Advised to provide your own postfixcol: this should work better if you
postfixToMakeUnique<-function(cvect, separator=".", postfixcol=NULL, allowemptypostfix=TRUE)
{
	uvect<-unique(cvect)
	if(length(uvect)==length(cvect)) return(cvect) #was already unique so leave it unchanged
	numperu<-sapply(uvect, function(curu){sum(curu==cvect)}) #may be better to use table here?
	maxrep<-max(numperu)
	if(is.null(postfixcol)) #provide enough numbers then, and
	{
		if(allowemptypostfix)
		{
			postfixcol<-c("", as.character(seq(maxrep-1)))
		}
		else
		{
			postfixcol<-as.character(seq(maxrep))
		}
	}
	else
	{
		postfixcol<-unique(postfixcol) #just in case!!
		if(!allowemptypostfix)
		{
			postfixcol<-setdiff(postfixcol, "")
		}
		while(length(postfixcol) < maxrep)
		{
			#add some unique items
			newuniques<-setdiff(as.character(seq(maxrep)), postfixcol)
			postfixcol<-c(postfixcol, newuniques[seq(maxrep-length(postfixcol))])
		}
	}
	postfixcol<-paste(separator, postfixcol, sep="")
	if(allowemptypostfix)
	{
		postfixcol[postfixcol==separator]<-""
	}
	for(curu in uvect)
	{
		relv<-which(curu==cvect)
		cvect[relv]<-paste(cvect[relv], postfixcol[seq_along(relv)], sep="")
	}
	return(cvect)
}

makeNamesFormulaSafe<-function(nms)
{
	nms<-gsub("(:)|(\\^)|(\\*)|(\\+)|(-)|(\\()|(\\))|(~)|(%)", "_", nms) #avoid characters that may have a special meaning in formulas
	return(gsub("[[:space:]]", "", nms)) #avoid spaces and similar that could ruin the formula syntax.
}

makeDatasetFormulaSafe<-function(dfr) UseMethod("makeDatasetFormulaSafe")

makeDatasetFormulaSafe.data.frame<-function(dfr)
{
	for(i in seq(ncol(dfr)))
	{
		if(is.factor(dfr[[i]]))
		{
			levels(dfr[[i]])<-makeNamesFormulaSafe(levels(dfr[[i]]))
		}
	}
	colnames(dfr)<-makeNamesFormulaSafe(colnames(dfr))
	return(dfr)
}

findRepsPerRow<-function(newdfr, orgdfr, failIfOrgNotFound=TRUE)
{
	resrn<-rownames(newdfr)
	orgrn<-rownames(orgdfr)
	orgpat<-paste("^", orgrn, ".*$", sep="")
	matchespernewrow<-lapply(resrn, function(newrn){
			rv<-which(sapply(orgpat, grepl, newrn)) #which of the original rownames match?
			longest<-which.max(nchar(orgrn[rv]))
			return(rv[longest]) #just return the first matching one (room for improvement?)
		})
	matchespernewrow<-do.call(c, matchespernewrow)
	if(length(matchespernewrow) != length(resrn))
	{
		if(failIfOrgNotFound)
		{
			stop("Could not find original for one of the rownames.")
		}
		else
		{
			catw("Could not find original for one of the rownames.")
		}
	}
	matchespernewrow<-rle(matchespernewrow)
	repsperrow<-matchespernewrow$lengths
	names(repsperrow)<-matchespernewrow$values
	return(repsperrow)
}

reduce<-function(object, ...) UseMethod("reduce")
reduce.default<-function(object, ...) return(object) #default: do nothing!

unreduce<-function(object, ...) UseMethod("unreduce")
unreduce.default<-function(object, ...) return(object) #default: do nothing!

reduce.data.frame<-function(object, orgdfr, repsperrow=NULL, keeponlyusedrows=FALSE, ...)
{
	.debugtxt()
	if(is.null(repsperrow))
	{
		repsperrow<-findRepsPerRow(object, orgdfr)
	}
	if(sum(repsperrow) != nrow(object))
	{
		stop("repsperrow total length(", sum(repsperrow), ") is not the same as number of rows in object (", nrow(object), ")")
	}
	usedrows<-as.integer(names(repsperrow))
	if(keeponlyusedrows)
	{
		uniqueusedrows<-unique(usedrows)
		usedrows<-match(usedrows, uniqueusedrows)
		orgdfr<-orgdfr[uniqueusedrows,]
		names(repsperrow)<-as.character(usedrows)
	}
	startofreps<-cumsum(c(1, repsperrow))
	#cat("startofreps:", startofreps)
	repdata<-lapply(seq_along(repsperrow), function(i){
			curorgrow<-usedrows[i]
			curnas<-which(is.na(orgdfr[curorgrow,]))
			names(curnas)<-colnames(orgdfr)[curnas]
			currws<-seq(startofreps[i], startofreps[i+1]-1)
			curreps<-object[currws, curnas]
			list(orgrow=curorgrow, nacols=curnas, repdata=curreps, names=rownames(object)[currws])
		})
	usemap<-rep(seq_along(usedrows), repsperrow)
	#note: so usemap contains 1 item for each "exterior row". It always holds the
	#row_index_ within orgdata that this exterior row comes from.
	rv<-list(repdata=repdata, orgdata=orgdfr, keptonlyusedrows=keeponlyusedrows,
		map=usemap)
	class(rv)<-paste(class(object), "rep", sep=".")
	return(rv)
}

unreduce.data.frame.rep<-function(object, ...)
{
	.debugtxt()
	repdata<-object$repdata
	orgdata<-object$orgdata
	parts<-lapply(repdata, function(currepdata){
			reporgrows<-rep(currepdata$orgrow,length(currepdata$names))
			rv<-orgdata[reporgrows,]
			if(length(currepdata$nacols) > 0)
			{
				rv[,currepdata$nacols]<-currepdata$repdata
			}
			rownames(rv)<-currepdata$names
			return(rv)
		})
	combineSimilarDfrList(parts)
}

do.parallel<-function(i, functionname, paramname, logdir, savedir=logdir,
	logorsavename= paste(functionname, "parallel", sep="_"), postprocessname=NULL,
	verbosity)#no default for verbosity!! Otherwise missing may not work
{
	param<-mget(paramname, as.environment(-1), ifnotfound=list(result=NULL), inherits=TRUE)[[1]]
	if(is.null(param))
	{
		return(paste("An error occurred: object '", paramname, "' could not be found."))
	}
	if(missing(verbosity))
	{
		#often occurring situation: the verbosity is really in the param
		#note, if used from run.parallel, this should never occur
		if(!is.null(param$verbosity))
		{
			verbosity<-param$verbosity
		}
		else
		{
			verbosity=0
		}
	}
#	if(verbosity > 0){} #try to send a message to the main out ?

	logorsavename<-paste(logorsavename, i, sep="_")
	if((verbosity>0) & (!is.null(logdir)))
	{
		setExtraPrefix(" ")
		sink(paste(logdir, logorsavename, ".txt", sep=""))
		on.exit(sink()) #restore normal logging at the end of the function
	}
	catwif(verbosity > 0, "Getting current parameter set.")
	curparam<-param[i] # see e.g. "[.EMLasso.1l.lognet.cv.param"
	catwif(verbosity > 3, "It resulted in this object (str):")
	if(verbosity > 3)
	{
		str(curparam, max.level=1)
	}
	catwif(verbosity > 0, "Will now actually perform the call for '", functionname, "'.")
	retval<-try(do.call(functionname, as.list(curparam)))
	catwif(verbosity > 0, "Call for '", functionname, "' finished.")
	if(verbosity > 0)
	{
		if(inherits(retval, "try-error"))
		{
			catw("Unfortunately, there was an error in the call - this will be returned")
			print(retval)
			traceback() #don't know if this will work, but it's worth a try
		}
	}
	if((verbosity>0) & (!is.null(savedir)))
	{
		catwif(verbosity > 0, "Saving current result.")
		assign(logorsavename, retval)
		save(list=logorsavename, file=paste(savedir, logorsavename, ".saved", sep=""))
	}
	#idea here: avoid returning big object: perhaps it's faster to just save it
	#and then later get it back (?)

	if(! is.null(postprocessname))
	{
		postprocess<-mget(postprocessname, as.environment(-1), ifnotfound=list(result=NULL), inherits=TRUE)[[1]]
		if(is.null(postprocess))
		{
			catw("postprocessname '", postprocessname, "' was passed along but could not be found. No postprocessing occurs.")
		}
		else
		{
			retval<-tryRet(postprocess(retval, curparam, i, verbosity-1), errRet=retval)
		}
		#retval<-reduce.cv.1l.emlasso(retval, param, orgdfr, ..., verbosity=verbosity-2)
	}

	return(retval)
}

run.parallel<-function(..., paramcreationname, functionname, paramname, logdir,
	savedir=logdir, logorsavename= paste(functionname, "parallel", sep="_"),
	postprocessname=NULL)
{
	require(snowfall)
  if(!sfIsRunning())
  {
    stop("run.parallel can only be used when a snowfall cluster is running.")
  }
	parlist<-list(...)
	if(!is.null(parlist$verbosity))
	{
		verbosity<-parlist$verbosity
	}
	else
	{
		verbosity<-0
	}

	catwif(verbosity > 0, "Converting parameters.")
	params<-do.call(paramcreationname, parlist)
	catwif(verbosity>0, "Assigning and exporting to all processors.")
	assign(paramname, params)
	sfExport(list=paramname)
	ivalues<-seq(length(params)) #see e.g. length.EMLasso.1l.lognet.cv.param
	catwif(verbosity > 0, "need to process a total of", length(ivalues), "items in parallel.")
	catwif(verbosity > 0, "Actual processing.")
	parts<-sfLapply(ivalues, do.parallel, functionname=functionname,
		paramname=paramname, logdir=logdir, savedir=savedir,
		logorsavename=logorsavename, postprocessname=postprocessname,
		verbosity=verbosity)
	catwif(verbosity > 0, "Actual processing finished.")
	return(parts)
}

scaleNonFactors<-function(dfr, colgroups=NULL, checkunique=FALSE, returnAttributes=FALSE)
{
	colns<-colnames(dfr)
	allcolnsingroups<-do.call(c, as.list(colgroups))
	if(checkunique)
	{
		if(length(unique(allcolnsingroups)) != length(allcolnsingroups))
		{
			stop("Some column(s) are in more than one group in scaleNonFactors")
		}
	}
	restcolns<-setdiff(colns, allcolnsingroups)
	if(length(restcolns) > 0)
	{
		#add the other columns as individual groups
		colgroups<-c(colgroups, as.list(restcolns))
	}
	for(i in seq_along(colgroups))
	{
		actualcols<-colgroups[[i]][!sapply(dfr[, colgroups[[i]], drop=FALSE], is.factor)]
		if(length(actualcols) > 1)
		{
			curdata<-do.call(c, dfr[, actualcols, drop=FALSE])
			curdata<-scale(curdata)
			curcenter<-attr(curdata, "scaled:center")
			curscale<-attr(curdata, "scaled:scale")
			curdata<-as.data.frame(matrix(curdata, nrow=nrow(dfr)))
			dfr[,actualcols]<-curdata
			for(curcol in actualcols)
			{
				attr(dfr[[curcol]], "scaled:center")<-curcenter
				attr(dfr[[curcol]], "scaled:scale")<-curscale
			}
		}
		else if(length(actualcols) == 1)
		{
			dfr[[actualcols]]<-scale(dfr[[actualcols]])
		}
	}
	if(! returnAttributes) return(dfr)
	scaleattributes<-sapply(dfr, function(curcol){
			rv<-c(center=attr(curcol, "scaled:center"), scale=attr(curcol, "scaled:scale") )
			if(is.null(rv)) rv<-c(center=0, scale=1 )
			return(rv)
		})
	return(list(dfr=dfr, scaleattributes=scaleattributes))
}



logit<-function(p, adjust)
{
	if(sum((p==0)|(p==1))>0)
	{
		if((missing(adjust)) || (adjust < 0) || (adjust > 1)) adjust<-0.025
		adjust<-abs(adjust)
		p[p==0]<-adjust
		p[p==1]<-1-adjust
	}
	log(p/(1-p))
}

expit<-function(x)
{
	tmp<-exp(x)
	tmp/(1+tmp)
}

invwhich<-function(indices, outlength, useNames = TRUE)
{
	rv<-rep(FALSE, outlength)
	if(length(indices) > 0)
	{
		rv[indices]<-TRUE
		if(useNames) names(rv)[indices]<-names(indices)
	}
	return(rv)
}