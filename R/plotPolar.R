plotPolar <-
function(mast, v.set=1, dir.set=1, subset, ...) {
### plotting wind speed vs. wind direction in polar plot
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object\n"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object\n"))
	num.sets <- length(mast$sets)
	
	if(!is.numeric(v.set)) v.set <- match(v.set, names(mast$sets))
	if(is.na(v.set)) stop("'v.set' not found\n")
	if(!is.numeric(dir.set)) dir.set <- match(dir.set, names(mast$sets))
	if(is.na(dir.set)) stop("'dir.set' not found\n")
	
	if(v.set<=0 || v.set>num.sets) stop("Specified 'v.set' could not be found\n")
	if(is.null(mast$sets[[v.set]]$data$v.avg)) stop("Specified 'v.set' does not contain average wind speed data\n")
	if(dir.set<=0 || dir.set>num.sets) stop("Specified 'dir.set' could not be found\n")
	if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("Specified 'dir.set' does not contain average wind direction data\n")
	
	# subset
	num.samples <- length(mast$time.stamp)
	if(missing(subset)) subset <- c(NA, NA)
	if((!any(is.character(subset)) && !any(is.na(subset))) || length(subset)!=2) stop("Please specify 'subset' as vector of start and end time stamp\n")
	if(is.na(subset[1])) subset[1] <- as.character(mast$time.stamp[1])
	if(is.na(subset[2])) subset[2] <- as.character(mast$time.stamp[num.samples])
	if(nchar(subset[1])==10) subset[1] <- paste(subset[1], "00:00:00")
	if(nchar(subset[2])==10) subset[2] <- paste(subset[2], "00:00:00")
	start <- strptime(subset[1], "%Y-%m-%d %H:%M:%S")
	end <- strptime(subset[2], "%Y-%m-%d %H:%M:%S")
	if(is.na(start)) stop("Specified start time stamp in 'subset' not correctly formated\n")
	if(is.na(end)) stop("Specified end time stamp in 'subset' not correctly formated\n")
	if(start<mast$time.stamp[1] || start>mast$time.stamp[num.samples]) stop("Specified 'start' not in period\n")
	match.date <- difftime(mast$time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(start, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	start <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	if(end<mast$time.stamp[1] || end>mast$time.stamp[num.samples]) stop("Specified 'end' not in period\n")
	match.date <- difftime(mast$time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(end, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	end <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	
	ws <- mast$sets[[v.set]]$data$v.avg[!is.na(mast$sets[[v.set]]$data$v.avg[start:end]) & !is.na(mast$sets[[dir.set]]$data$dir.avg[start:end])]
	wd <- mast$sets[[dir.set]]$data$dir.avg[!is.na(mast$sets[[v.set]]$data$v.avg[start:end]) & !is.na(mast$sets[[dir.set]]$data$dir.avg[start:end])]*-pi/180+pi/2
	v.max <- max(mast$sets[[v.set]]$data$v.avg[start:end], na.rm=TRUE)
	unit <- attr(mast$sets[[v.set]]$data$v.avg, "unit")
	
	# prepare plot
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	
	plot.param <- list(...)
	if(any(names(plot.param)=="col")) col <- plot.param$col
	else col <- "#3182BD"
	if(any(names(plot.param)=="pch")) pch <- plot.param$pch
	else pch <- "."
	if(any(names(plot.param)=="cex")) cex <- plot.param$cex
	else cex <- 1
	if(any(names(plot.param)=="cex.pts")) cex.pts <- plot.param$cex.pts
	else cex.pts <- cex
	if(any(names(plot.param)=="cex.axis")) cex.axis <- plot.param$cex.axis
	else cex.axis <- cex-0.2
	if(any(names(plot.param)=="cex.lab")) cex.lab <- plot.param$cex.lab
	else cex.lab <- cex
	if(any(names(plot.param)=="circles")) circles <- seq(plot.param$circles[1], plot.param$circles[2], by=plot.param$circles[3])
	else circles <- seq(5, 5*(trunc(ceiling(v.max)/5)+1), by=5)
	if(any(names(plot.param)=="fg")) fg <- plot.param$fg
	else fg <- FALSE
	if(any(names(plot.param)=="col.circle")) col.circle <- plot.param$col.circle
	else col.circle <- "gray45"
	if(any(names(plot.param)=="col.cross")) col.cross <- plot.param$col.cross
	else col.cross <- "gray45"
	if(any(names(plot.param)=="col.axis")) col.axis <- plot.param$col.axis
	else col.axis <- "gray45"
	if(any(names(plot.param)=="col.lab")) col.lab <- plot.param$col.lab
	else col.lab <- "black"
	if(any(names(plot.param)=="lwd.circle")) lwd.circle <- plot.param$lwd.circle
	else lwd.circle <- 0.7
	if(any(names(plot.param)=="lwd.cross")) lwd.cross <- plot.param$lwd.cross
	else lwd.cross <- 0.7
	if(any(names(plot.param)=="lty.circle")) lty.circle <- plot.param$lty.circle
	else lty.circle <- 2
	if(any(names(plot.param)=="lty.cross")) lty.cross <- plot.param$lty.cross
	else lty.cross <- 1
	if(any(names(plot.param)=="pos.axis")) pos.axis <- plot.param$pos.axis
	else pos.axis <- 60
		
	# plot
	par(mar=c(0,0,0,0), las=1)
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1] > pin[2]) xlim <- (pin[1]/pin[2]) * xlim
	else ylim <- (pin[2]/pin[1]) * ylim
	plot.window(xlim, ylim, "", asp=1)
	
	if(!fg) {
		xlist <- 0.9 * ws/tail(circles, 1) * cos(wd)
		ylist <- 0.9 * ws/tail(circles, 1) * sin(wd)
		points(xlist, ylist, pch=pch, col=col, cex=cex.pts)
	}
	
	circle.pts <- seq(0, 2*pi, length.out=360)
	pos.axis <- pi/2 - pos.axis*pi/180
	for(i in 1:length(circles)) {
		rad <- 0.9 * circles[i]/tail(circles, 1)
		circle.x <- cos(circle.pts)*rad
		circle.y <- sin(circle.pts)*rad
		lines(circle.x, circle.y, lty=lty.circle, lwd=lwd.circle, col=col.circle)	
		text(cos(pos.axis)*rad, sin(pos.axis)*rad, circles[i], cex=cex.axis, col=col.axis)
	}
	
	lines(c(-0.92, 0.92), c(0, 0), lty=lty.cross, lwd=lwd.cross, col=col.cross)
	lines(c(0, 0), c(0.92, -0.92), lty=lty.cross, lwd=lwd.cross, col=col.cross)
	text(0, -0.9, "S", pos=1, cex=cex.lab, col=col.lab)
	text(-0.9, 0, "W", pos=2, cex=cex.lab, col=col.lab)
	text(0, 0.9, "N", pos=3, cex=cex.lab, col=col.lab)
	text(0.9, 0, "E", pos=4, cex=cex.lab, col=col.lab)
	text(1, -1, unit, pos=2, cex=cex.axis, col=col.axis)
	
	if(fg) {
		xlist <- 0.9 * ws/tail(circles, 1) * cos(wd)
		ylist <- 0.9 * ws/tail(circles, 1) * sin(wd)
		points(xlist, ylist, pch=pch, col=col, cex=cex.pts)
	}
}
