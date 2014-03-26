profile <-
function(mast, v.set, dir.set, num.sectors=12, method=c("hellman", "loglm", "fixed"), alpha=NULL, subset, digits=3, print=TRUE) {
###	computing profile from mast data
	
	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object\n"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object\n"))
	num.sets <- length(mast$sets)
	if(missing(v.set)) stop("Please choose one or two sets in 'v.set'\n")
	if(!is.numeric(v.set)) if(!(length(v.set)==1 && any(v.set=="all"))) v.set <- match(v.set, names(mast$sets))
	if(any(is.na(v.set))) stop("'v.set' not found\n")
	if(any(v.set<=0) || any(v.set>num.sets)) stop("'v.set' not found\n")
	for(i in 1:length(v.set)) if(is.null(mast$sets[[v.set[i]]]$data$v.avg)) stop(paste("Set", v.set[i], "does not contain average wind speed data\n"))
	if(!is.numeric(dir.set)) dir.set <- match(dir.set, names(mast$sets))
	if(is.na(dir.set)) stop("'dir.set' not found\n")
	if(dir.set<=0 || dir.set>num.sets) stop("'dir.set' not found\n")
	if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop(paste("'dir.set' does not contain average wind direction data\n"))
	if(!is.numeric(num.sectors)) stop("'num.sectors' must be numeric\n")
	if(num.sectors<=1) stop("There must be at least 2 sectors\n")
	if(missing(method)) {
		if(length(v.set)==1) method <- "fixed"
		else if(length(v.set)==2) method <- "hellman"
		else method <- "loglm"
	}
	
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
	
	sector.width <- 360/num.sectors
	sectors <- seq(0, 360-sector.width, by=sector.width)
	sector.edges <- c(sectors-sector.width/2, tail(sectors, n=1)+sector.width/2)%%360
	r.names <- c(paste("s", 1:num.sectors, sep=""),"all")
	if(num.sectors==4) r.names <- c("n","e","s","w","all")
	if(num.sectors==8) r.names <- c("n","ne","e","se","s","sw","w","nw","all")
	if(num.sectors==12) r.names <- c("n","nne","ene","e","ese","sse","s","ssw","wsw","w","wnw","nnw","all")
	if(num.sectors==16) r.names <- c("n","nne","ne","ene","e","ese","se","sse","s","ssw","sw","wsw","w","wnw","nw","nnw","all")
	
	profile <- NULL
	v1 <- mast$sets[[v.set[1]]]$data$v.avg[start:end]
	h1 <- mast$sets[[v.set[1]]]$height
	dir <- mast$sets[[dir.set]]$data$dir.avg[start:end]
	
	if(method=="fixed") {	# fixed alpha
		idx.val <- !is.na(v1) & !is.na(dir) & v1>0
		if(is.null(alpha)) alpha <- 0.2
		profile <- data.frame(matrix(NA, nrow=num.sectors+1, ncol=2))
		
		for(i in 1:num.sectors) {
			low <- sector.edges[i]
			high <- sector.edges[i+1]			
			if(low<high) sector.idx <- dir>=low & dir<high
			else sector.idx <- dir>=low | dir<high
			
			profile[i,1] <- alpha
			profile[i,2] <- mean(v1[idx.val & sector.idx], na.rm=TRUE)
		}
		profile[num.sectors+1,1] <- alpha
		profile[num.sectors+1,2] <- mean(v1[idx.val], na.rm=TRUE)	# idx.val???
		names(profile) <- c("alpha", "v.ref")
		row.names(profile) <- r.names
		attr(profile, "units") <- c("-", attr(mast$sets[[v.set[1]]]$data$v.avg, "unit"))
	} else if(method=="hellman") {	# hellmann
		if(length(v.set)<2) stop("Hellman method requires two datasets")
		if(length(v.set)>2) {
			cat("Hellman method requires two datasets - only the first two sets from 'v.set' are used\n")
			v.set <- v.set[1:2]
		}
		
		v2 <- mast$sets[[v.set[2]]]$data$v.avg[start:end]
		h2 <- mast$sets[[v.set[2]]]$height
		
		if(h1==h2) stop("Sets have the same height - no extrapolation possible\n")
		
		idx.val1 <- !is.na(v1) & !is.na(dir) & v1>0
		idx.val2 <- !is.na(v2) & !is.na(dir) & v2>0
		profile <- data.frame(matrix(NA, ncol=num.sectors+1, nrow=length(v1)))
		v.ref <- NULL
		for(i in 1:num.sectors) {
			low <- sector.edges[i]
			high <- sector.edges[i+1]
			if(low<high) sector.idx <- dir>=low & dir<high
			else sector.idx <- dir>=low | dir<high
		
			profile[idx.val1 & idx.val2 & sector.idx,i] <- log(v2[idx.val1 & idx.val2 & sector.idx] / v1[idx.val1 & idx.val2 & sector.idx]) / log(h2 / h1)
			v.ref <- append(v.ref, mean(v1[idx.val1 & sector.idx], na.rm=TRUE))
		}
		
		profile <- data.frame(cbind(colMeans(profile, na.rm=TRUE), c(v.ref, NA)))
		profile[num.sectors+1,1] <- mean(log(v2[idx.val1 & idx.val2] / v1[idx.val1 & idx.val2]) / log(h2 / h1), na.rm=TRUE)
		profile[num.sectors+1,2] <- mean(v1[idx.val1], na.rm=TRUE)	# idx.val1???
		names(profile) <- c("alpha", "v.ref")
		row.names(profile) <- r.names
		attr(profile, "units") <- c("-", attr(mast$sets[[v.set[1]]]$data$v.avg, "unit"))
	} else {	# logreg
		if(length(v.set)<2) stop("Loglm method requires at least two datasets")
				
		v <- v1
		h <- rep(h1, length(v))
		for(i in 2:length(v.set)) {
			v <- append(v, mast$sets[[v.set[i]]]$data$v.avg[start:end])
			h <- append(h, rep(mast$sets[[v.set[i]]]$height, length(mast$sets[[v.set[i]]]$data$v.avg[start:end])))
		}
		
		if(length(unique(h))!=length(v.set)) stop("Sets have the same height - no extrapolation possible\n")
		
		profile <- data.frame(matrix(NA, ncol=2, nrow=num.sectors+1))
		v.ref <- NULL
		idx.val <- !is.na(v1) & !is.na(dir) & v1>0
		d <- rep(dir, length(v.set))
		
		for(i in 1:num.sectors) {			
			low <- sector.edges[i]
			high <- sector.edges[i+1]
			if(low<high) sector.idx <- dir>=low & dir<high
			else sector.idx <- dir>=low | dir<high
			sector.idx.1 <- sector.idx
			sector.idx <- rep(sector.idx, length(v.set))
			
			idx <- !is.na(v) & !is.na(d) & v>0 & sector.idx
			loglm <- lm(log(v[idx]) ~ log(h[idx]))
			slp <- loglm$coefficients[2]
			if(slp<0 && length(v.set)==2) {	# prevent some negative alphas
				h.1 <- mast$sets[[v.set[1]]]$height
				h.2 <- mast$sets[[v.set[2]]]$height
				v.1 <- mean(mast$sets[[v.set[1]]]$data$v.avg[start:end][sector.idx.1], na.rm=TRUE)
				v.2 <- mean(mast$sets[[v.set[2]]]$data$v.avg[start:end][sector.idx.1], na.rm=TRUE)
				if((h.1>h.2 && v.1>v.2) || (h.1<h.2 && v.1<v.2)) slp <- abs(slp)
			}
			if(round(slp, digits)==0 && digits>=2) slp <- 10^-digits
			profile[i,1] <- slp
			profile[i,2] <- mean(v1[idx.val & sector.idx.1], na.rm=TRUE)
		}
		
		idx <- !is.na(v) & !is.na(d) & v>0
		loglm <- lm(log(v[idx]) ~ log(h[idx]))
		profile[num.sectors+1,1] <- loglm$coefficients[2]
		profile[num.sectors+1,2] <- mean(v1[idx.val], na.rm=TRUE)	# idx.val???
		names(profile) <- c("alpha", "v.ref")
		row.names(profile) <- r.names
		attr(profile, "units") <- c("-", attr(mast$sets[[v.set[1]]]$data$v.avg, "unit"))
	}
	
	profile <- list(profile=round(profile, digits), h.ref=h1)
	attr(profile, "call") <- list(func="profile", mast=deparse(substitute(mast)), v.set=v.set, dir.set=dir.set, num.sectors=num.sectors, method=method, alpha=alpha, subset=subset, digits=digits, print=print)
	
	if(print) printObject(profile)
	invisible(profile)
}
