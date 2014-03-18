availability <- 
function(mast, v.set, dir.set, digits=1, print=TRUE) {
### check availability for pairs of windspeed and direction - effective data period
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object\n"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object\n"))
	num.sets <- length(mast$sets)
	if(missing(v.set) && missing(dir.set)) v.set <- "all"
	if(!missing(v.set) && missing(dir.set)) dir.set <- v.set
	if(missing(v.set) && !missing(dir.set)) v.set <- dir.set
	
	if(!is.numeric(v.set)) if(!(length(v.set)==1 && any(v.set=="all"))) v.set <- match(v.set, names(mast$sets))
	if(any(is.na(v.set))) stop("'v.set' not found\n")
	if(!is.numeric(dir.set)) if(!(length(dir.set)==1 && any(dir.set=="all"))) dir.set <- match(dir.set, names(mast$sets))
	if(any(is.na(dir.set))) stop("'dir.set' not found\n")
	
	num.samples <- length(mast$time.stamp)
	start.year <- mast$time.stamp$year[1]+1900
	end.year <- mast$time.stamp$year[num.samples]+1900
	start.month <- mast$time.stamp$mon[1]+1
	end.month <- mast$time.stamp$mon[num.samples]+1
	start.day <- mast$time.stamp$mday[1]
	end.day <- mast$time.stamp$mday[num.samples]
	if(start.year==end.year) num.months <- end.month-start.month+1
	if(start.year!=end.year) num.months <- 13-start.month+end.month + 12*(end.year-start.year-1)
	period.days <- as.numeric(mast$time.stamp[num.samples]-mast$time.stamp[1])
	
	if(all(v.set!="all")) {
		if(length(v.set)==1 && length(dir.set)==1) { # one set
			if(v.set<0 || v.set>num.sets) stop("'v.set' not found\n")
			if(dir.set<0 || dir.set>num.sets) stop("'dir.set' not found\n")
			if(is.null(mast$sets[[v.set]]$data$v.avg)) stop("'v.set' does not contain average wind speed data\n")
			if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("'dir.set' does not contain average wind direction data\n")
			if(any(attr(mast$sets[[v.set]]$data, "clean")=="v.avg") && any(attr(mast$sets[[dir.set]]$data, "clean")=="dir.avg")) cat("Set(s) not cleaned - cleaning of wind speed v.avg and wind direction dir.avg using 'clean' is recommended to avoid overestimated availability\n")
			avail <- list(availabilityInt(mast$sets[[v.set]]$data$v.avg, mast$sets[[dir.set]]$data$dir.avg, mast$time.stamp, start.year, start.month, num.months, period.days, digits))
			if(v.set==dir.set) names(avail) <- names(mast$sets)[v.set]
			else names(avail) <- paste(names(mast$sets)[v.set], "_", names(mast$sets)[dir.set], sep="")
		} else { # list of sets
			if(length(v.set)==length(dir.set)) {
				avail <- total <- NULL
				uncleaned <- 0
				
				for(s in 1:length(v.set)) { # x/x
					if(v.set[s]<0 || v.set[s]>num.sets) stop("'v.set' not found\n")
					if(dir.set[s]<0 || dir.set[s]>num.sets) stop("'dir.set' not found\n")
					if(is.null(mast$sets[[v.set[s]]]$data$v.avg)) stop("'v.set' does not contain average wind speed data\n")
					if(is.null(mast$sets[[dir.set[s]]]$data$dir.avg)) stop("'dir.set' does not contain average wind direction data\n")
					if(any(attr(mast$sets[[v.set[s]]]$data, "clean")=="v.avg") && any(attr(mast$sets[[dir.set[s]]]$data, "clean")=="dir.avg")) cat("Set(s) not cleaned - cleaning of wind speed v.avg and wind direction dir.avg using 'clean' is recommended to avoid overestimated availability\n")
					
					avail.s <- availabilityInt(mast$sets[[v.set[s]]]$data$v.avg, mast$sets[[dir.set[s]]]$data$dir.avg, mast$time.stamp, start.year, start.month, num.months, period.days, digits)
					if(!is.null(avail)) avail[[length(avail)+1]] <- avail.s
					if(is.null(avail)) avail <- list(avail.s)
					if(any(attr(mast$sets[[v.set[s]]]$data, "clean")=="v.avg") && any(attr(mast$sets[[v.set[s]]]$data, "clean")=="dir.avg")) uncleaned <- uncleaned+1
					if(v.set[s]==dir.set[s]) names(avail)[s] <- names(mast$sets)[v.set[s]]
					else names(avail)[s] <- paste(names(mast$sets)[v.set[s]], "_", names(mast$sets)[dir.set[s]], sep="")
				}
			} else if(length(v.set)!=length(dir.set) && (length(v.set)==1 || length(dir.set)==1)) { # x/1 or 1/x
				avail <- total <- NULL
				uncleaned <- 0
				
				if(length(v.set)==1) {
					for(s in 1:length(dir.set)) {
						if(v.set<0 || v.set>num.sets) stop("'v.set' not found\n")
						if(dir.set[s]<0 || dir.set[s]>num.sets) stop("'dir.set' not found\n")
						if(is.null(mast$sets[[v.set]]$data$v.avg)) stop("'v.set' does not contain average wind speed data\n")
						if(is.null(mast$sets[[dir.set[s]]]$data$dir.avg)) stop("'dir.set' does not contain average wind direction data\n")
						if(any(attr(mast$sets[[v.set]]$data, "clean")=="v.avg") && any(attr(mast$sets[[dir.set[s]]]$data, "clean")=="dir.avg")) cat("Set(s) not cleaned - cleaning of wind speed v.avg and wind direction dir.avg using 'clean' is recommended to avoid overestimated availability\n")
						
						avail.s <- availabilityInt(mast$sets[[v.set]]$data$v.avg, mast$sets[[dir.set[s]]]$data$dir.avg, mast$time.stamp, start.year, start.month, num.months, period.days, digits)
						if(!is.null(avail)) avail[[length(avail)+1]] <- avail.s
						if(is.null(avail)) avail <- list(avail.s)
						if(any(attr(mast$sets[[v.set]]$data, "clean")=="v.avg") && any(attr(mast$sets[[v.set]]$data, "clean")=="dir.avg")) uncleaned <- uncleaned+1
						if(v.set==dir.set[s]) names(avail)[s] <- names(mast$sets)[v.set]
						else names(avail)[s] <- paste(names(mast$sets)[v.set], "_", names(mast$sets)[dir.set[s]], sep="")
					}
				} else {
					for(s in 1:length(v.set)) {
						if(v.set[s]<0 || v.set[s]>num.sets) stop("'v.set' not found\n")
						if(dir.set<0 || dir.set>num.sets) stop("'dir.set' not found\n")
						if(is.null(mast$sets[[v.set[s]]]$data$v.avg)) stop("'v.set' does not contain average wind speed data\n")
						if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("'dir.set' does not contain average wind direction data\n")
						if(any(attr(mast$sets[[v.set[s]]]$data, "clean")=="v.avg") && any(attr(mast$sets[[dir.set]]$data, "clean")=="dir.avg")) cat("Set(s) not cleaned - cleaning of wind speed v.avg and wind direction dir.avg using 'clean' is recommended to avoid overestimated availability\n")
						
						avail.s <- availabilityInt(mast$sets[[v.set[s]]]$data$v.avg, mast$sets[[dir.set]]$data$dir.avg, mast$time.stamp, start.year, start.month, num.months, period.days, digits)
						if(!is.null(avail)) avail[[length(avail)+1]] <- avail.s
						if(is.null(avail)) avail <- list(avail.s)
						if(any(attr(mast$sets[[v.set[s]]]$data, "clean")=="v.avg") && any(attr(mast$sets[[v.set[s]]]$data, "clean")=="dir.avg")) uncleaned <- uncleaned+1
						if(v.set[s]==dir.set) names(avail)[s] <- names(mast$sets)[v.set[s]]
						else names(avail)[s] <- paste(names(mast$sets)[v.set[s]], "_", names(mast$sets)[dir.set], sep="")
					}
				}
			}
		}
	} else { # all sets
		set.index <- NULL
		for(s in 1:num.sets) if(!is.null(mast$sets[[s]]$data$v.avg) && !is.null(mast$sets[[s]]$data$dir.avg)) set.index <- append(set.index, s)
		if(is.null(set.index)) stop("No pairs of wind speed and wind direction data found")
		avail <- total <- NULL
		uncleaned <- 0
		
		for(s in 1:length(set.index)) {
			avail.s <- availabilityInt(mast$sets[[set.index[s]]]$data$v.avg, mast$sets[[set.index[s]]]$data$dir.avg, mast$time.stamp, start.year, start.month, num.months, period.days, digits)
			if(!is.null(avail)) avail[[length(avail)+1]] <- avail.s
			if(is.null(avail)) avail <- list(avail.s)
			if(any(attr(mast$sets[[set.index[s]]]$data, "clean")=="v.avg") && any(attr(mast$sets[[set.index[s]]]$data, "clean")=="dir.avg")) uncleaned <- uncleaned+1
		}
		names(avail) <- names(mast$sets)[set.index]
		if(uncleaned>0) cat(paste(uncleaned, "of", length(set.index), "sets were not cleaned - cleaning of wind speed v.avg and wind direction dir.avg using 'clean' is recommended to avoid overestimated availability\n"))
	}
	
	attr(avail, "call") <- list(func="availability", mast=deparse(substitute(mast)), v.set=v.set, dir.set=dir.set, digits=3, print=print)
	
	if(print) printObject(avail)
	invisible(avail)
}
