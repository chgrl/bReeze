plot.weibull <- 
function(x, show.ak=FALSE, ...) {
### plotting fitted weibull distribution from weibull object
		
	if(is.null(attr(x, "call")$mast)) stop("Source mast object of ", substitute(x), " could not be found")
	mast <- get(attr(x, "call")$mast)
	v.set <- attr(x, "call")$v.set
	subset <- attr(x, "call")$subset
	unit <- attr(mast$sets[[v.set]]$data$v.avg, "unit")
	
	# subset
	start.end <- subset.int(mast$timestamp, subset)
	start <- start.end[1]
	end <- start.end[2]
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#3182BD"
	if(any(names(plot.param)=="col.lab")) col.lab <- plot.param$col.lab
	else col.lab <- "black"
	if(any(names(plot.param)=="col.axis")) col.axis <- plot.param$col.axis
	else col.axis <- "black"
	if(any(names(plot.param)=="col.leg")) col.leg <- plot.param$col.leg
	else col.leg <- "black"
	if(any(names(plot.param)=="col.ticks")) col.ticks <- plot.param$col.ticks
	else col.ticks <- "black"
	if(any(names(plot.param)=="col.box")) col.box <- plot.param$col.box
	else col.box <- "black"
	if(any(names(plot.param)=="border")) border <- plot.param$border
	else border <- "white"
	if(any(names(plot.param)=="line")) line <- plot.param$line
	else line <- "#E41A1C"
	if(any(names(plot.param)=="lty")) lty <- plot.param$lty
	else lty <- 1
	if(any(names(plot.param)=="lwd")) lwd <- plot.param$lwd
	else lwd <- 1.2
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="cex.lab")) cex.lab <- plot.param$cex.lab
	else cex.lab <- cex
	if(any(names(plot.param)=="cex.axis")) cex.axis <- plot.param$cex.axis
	else cex.axis <- cex
	if(any(names(plot.param)=="cex.leg")) cex.leg <- plot.param$cex.leg
	else cex.leg <- cex-0.2
	if(any(names(plot.param)=="x.intersp")) x.intersp <- plot.param$x.intersp
	else x.intersp <- 0.4
	if(any(names(plot.param)=="y.intersp")) y.intersp <- plot.param$y.intersp
	else y.intersp <- 0.8
	if(any(names(plot.param)=="bty.leg")) bty.leg <- plot.param$bty.leg
	else bty.leg <- "n"
	if(any(names(plot.param)=="pos.leg")) pos.leg <- plot.param$pos.leg
	else pos.leg <- "topright"
	if(any(names(plot.param)=="breaks")) breaks <- plot.param$breaks
	else breaks <- seq(0, ceiling(max(mast$sets[[v.set]]$data$v.avg[start:end], na.rm=TRUE)), 1)
	if(any(names(plot.param)=="xlab")) xlab <- plot.param$xlab
	else xlab <- paste0("Wind speed [", unit, "]")
	if(any(names(plot.param)=="ylab")) ylab <- plot.param$ylab
	else ylab <- "Frequency [%]"
	if(any(names(plot.param)=="xlim")) xlim <- plot.param$xlim
	else xlim <- range(breaks)
	if(any(names(plot.param)=="ylim")) ylim <- plot.param$ylim/100
	else ylim <- NULL
	if(any(names(plot.param)=="mar")) mar <- plot.param$mar
	else mar <- c(4.5,4.5,1,1)
	if(any(names(plot.param)=="mgp")) mgp <- plot.param$mgp
	else mgp <- c(2.2,0.7,0)
	if(any(names(plot.param)=="las")) las <- plot.param$las
	else las <- 1
	if(any(names(plot.param)=="bty")) bty <- plot.param$bty
	else bty <- "o"
	if(any(names(plot.param)=="legend")) legend <- plot.param$legend
	else legend <- TRUE
	if(any(names(plot.param)=="leg.text")) leg.text <- plot.param$leg.text
	else leg.text <- c("measured", "Weibull")
	
	density <- hist(mast$sets[[v.set]]$data$v.avg[start:end], breaks=breaks, plot=FALSE)$density
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
	
	# plot
	par(mar=mar, mgp=mgp, las=las, bty="n")
	plot.new()
	hist(mast$sets[[v.set]]$data$v.avg[start:end], breaks=breaks, axes=FALSE, freq=FALSE, col=col, border=border, main=NULL, xlab=xlab, ylab=ylab, cex.lab=cex.lab, col.lab=col.lab, xlim=xlim, ylim=ylim)
	box(bty=bty, col=col.box)
	axis(1, col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
	if(!is.null(ylim)) axis(2, at=seq(ylim[1], ylim[2], 0.02), labels=seq(ylim[1]*100, ylim[2]*100, 2), col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
	else axis(2, at=seq(0, floor(max(density)*100)/100, 0.02), labels=seq(0, floor(max(density)*100), 2), col=col.ticks, col.axis=col.axis, cex.axis=cex.axis)
	
	x <- NULL # just to satisfy R CMD check
	curve(dweibull(x, shape=tail(x$k, 1), scale=tail(x$A, 1)), col=line, lty=lty, lwd=lwd, add=TRUE)
	
	if(legend) {
		if(show.ak) leg.text <- c(leg.text[1], paste0(leg.text[2], " (A:", round(tail(x$A, 1), digits=1), ", k:", round(tail(x$k, 1), digits=1), ")"))
		legend(pos.leg, legend=leg.text, col=c(border, line), lty=c(NA, lty), lwd=c(NA, lwd), pch=c(22, NA), pt.bg=c(col, NA), bty=bty.leg, cex=cex.leg, text.col=col.leg, x.intersp=x.intersp, y.intersp=y.intersp)
	}
}
