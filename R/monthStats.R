monthStats <-
function(mast, set, signal="v.avg", fun=c("mean", "median", "min", "max", "sd"), subset, digits=3, print=TRUE) {
### calculating monthly statistics

	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object\n"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object\n"))
	if(missing(set)) set <- "all"
	if(is.null(signal)) stop("Please choose signal\n")
	if(length(signal)>1) stop("Please choose only one signal\n")
	if(missing(fun) || length(fun)!=1) fun <- "mean"
	
	# subset
	num.samples <- length(mast$time.stamp)
	if(missing(subset)) subset <- c(NA, NA)
	if((!any(is.character(subset)) && !any(is.na(subset))) || length(subset)!=2) stop("Please specify 'subset' as vector of start and end time stamp\n")
	if(is.na(subset[1])) subset[1] <- as.character(mast$time.stamp[1])
	if(is.na(subset[2])) subset[2] <- as.character(mast$time.stamp[num.samples])
	start <- strptime(subset[1], "%Y-%m-%d %H:%M:%S")
	end <- strptime(subset[2], "%Y-%m-%d %H:%M:%S")
	if(is.na(start)) start <- strptime(subset[1], "%Y-%m-%d %H:%M")
	if(is.na(end)) end <- strptime(subset[2], "%Y-%m-%d %H:%M")
	if(is.na(start)) start <- strptime(subset[1], "%Y-%m-%d %H")
	if(is.na(end)) end <- strptime(subset[2], "%Y-%m-%d %H")
	if(is.na(start)) stop("Specified start time stamp in 'subset' not correctly formated\n")
	if(is.na(end)) stop("Specified end time stamp in 'subset' not correctly formated\n")
	if(start<mast$time.stamp[1] || start>mast$time.stamp[num.samples]) stop("Specified 'start' not in period\n")
	match.date <- difftime(mast$time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(start, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	start <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	if(end<mast$time.stamp[1] || end>mast$time.stamp[num.samples]) stop("Specified 'end' not in period\n")
	match.date <- difftime(mast$time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(end, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	end <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	
	m.stats.l <- NULL
	years <- unique(mast$time.stamp[start:end]$year+1900)
	num.sets <- length(mast$sets)
	unit <- NULL
	
	if(set!="all") { # one set
		if(!is.numeric(set)) set <- match(set, names(mast$sets))
		if(is.na(set)) stop("Set not found\n") 
		if(set<0 | set>num.sets) stop("Set not found\n")
		if(!any(names(mast$sets[[set]]$data)==signal)) stop("Specified set does not contain the choosen signal\n")
		dat <- mast$sets[[set]]$data[,which(names(mast$sets[[set]]$data)==signal)][start:end]
		m.stats.l <- list(monthStatsInt(dat, fun, mast$time.stamp[start:end], years, digits))
		names(m.stats.l) <- names(mast$sets)[set]
		unit <- attr(mast$sets[[set]]$data[,which(names(mast$sets[[set]]$data)==signal)], "unit")
	} else { # all sets
		set.index <- NULL
		for(s in 1:num.sets) if(any(names(mast$sets[[s]]$data)==signal)) set.index <- append(set.index, s)
		if(is.null(set.index)) stop("Signal not found in any set\n")
		
		m.stats.l <- list(monthStatsInt(mast$sets[[set.index[1]]]$data[,which(names(mast$sets[[set.index[1]]]$data)==signal)][start:end], fun, mast$time.stamp[start:end], years, digits))
		unit <- attr(mast$sets[[set.index[1]]]$data[,which(names(mast$sets[[set.index[1]]]$data)==signal)], "unit")
		
		if(length(set.index) > 1) {
			for(s in 2:length(set.index)) {
				m.stats.df <- monthStatsInt(mast$sets[[set.index[s]]]$data[,which(names(mast$sets[[set.index[s]]]$data)==signal)][start:end], fun, mast$time.stamp[start:end], years, digits)
				m.stats.l[[length(m.stats.l)+1]] <- m.stats.df
			}
		}
		names(m.stats.l) <- names(mast$sets)[set.index]
	}

	attr(m.stats.l, "unit") <- unit
	attr(m.stats.l, "call") <- list(func="monthStats", mast=deparse(substitute(mast)), set=set, signal=signal, fun=fun, subset=subset, digits=digits, print=print)
	
	if(print) printObject(m.stats.l)
	invisible(m.stats.l)
}
