turbulence <-
function(mast, turb.set, dir.set, num.sectors=12, bins=c(5,10,15,20), subset, digits=3, print=TRUE) {
### calculating mean wind speed and turbulence intensity of sectors

	if(is.null(attr(mast, "call"))) stop(paste(substitute(mast), "is no mast object\n"))
	if(attr(mast, "call")$func!="createMast") stop(paste(substitute(mast), "is no mast object\n"))
	num.sets <- length(mast$sets)
	if(!missing(turb.set) && missing(dir.set)) dir.set <- turb.set
	if(missing(turb.set) && !missing(dir.set)) turb.set <- dir.set
	
	if(!is.numeric(num.sectors)) stop("'num.sectors' must be numeric\n")
	if(num.sectors<=1) stop("There must be at least 2 sectors\n")
	if(!is.numeric(turb.set)) turb.set <- match(turb.set, names(mast$sets))
	if(is.na(turb.set)) stop("'turb.set' not found\n")
	if(turb.set<=0 || turb.set>num.sets) stop("'turb.set' not found\n")
	if(!is.numeric(dir.set)) dir.set <- match(dir.set, names(mast$sets))
	if(is.na(dir.set)) stop("'dir.set' not found\n")
	if(dir.set<=0 || dir.set>num.sets) stop("'dir.set' not found\n")
	if(is.null(mast$sets[[turb.set]]$data$turb.int)) stop("Specified set does not contain turbulence intensity data\n")
	if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("Specified set does not contain wind direction data\n")
	if(any(bins<0)) stop("'bins' must be NULL or a vector of positives\n")
	
	sector.width <- 360/num.sectors
	sectors <- seq(0, 360-sector.width, by=sector.width)
	sector.edges <- c(sectors-sector.width/2, tail(sectors, n=1)+sector.width/2)%%360
	if(!is.null(bins)) if(head(bins, 1)!=0) bins <- c(0, bins)
	num.classes <- length(bins)
	v.max <- max(mast$sets[[turb.set]]$data$v.avg, na.rm=TRUE)
	if(num.classes>2) {
		for(i in (num.classes-1):2) {
			if(bins[i+1]>=v.max & bins[i]>=v.max) {
				bins <- head(bins, -1)
				num.classes <- length(bins)
			}
		}
	}
	if(!is.null(bins)) if(num.classes==2 && bins[num.classes]>=v.max) stop("Only one wind class found\n")
	
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
	
	turb.tbl <- matrix(NA, nrow=num.sectors+1, ncol=num.classes+1)
	# indices for valid data
	idx.val <- !is.na(mast$sets[[turb.set]]$data$turb.int[start:end]) & !is.na(mast$sets[[dir.set]]$data$dir.avg[start:end])
	idx.v <- !is.na(mast$sets[[turb.set]]$data$v.avg[start:end])
	
	for(s in 1:num.sectors) {
		# index for direction
		low <- sector.edges[s]
		high <- sector.edges[s+1]
		if(low<high) idx.dir <- mast$sets[[dir.set]]$data$dir.avg[start:end]>=low & mast$sets[[dir.set]]$data$dir.avg[start:end]<high
		else idx.dir <- mast$sets[[dir.set]]$data$dir.avg[start:end]>=low | mast$sets[[dir.set]]$data$dir.avg[start:end]<high
		
		if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.dir])<3) turb.tbl[s,1] <- NA
		else turb.tbl[s,1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.dir])
		if(!is.null(bins)) {
			for(c in 1:(num.classes-1)) {
				# index for wind class
				idx.class <- mast$sets[[turb.set]]$data$v.avg[start:end]>=bins[c] & mast$sets[[turb.set]]$data$v.avg[start:end]<bins[c+1]
				if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & idx.class])<3) turb.tbl[s,c+1] <- NA
				else turb.tbl[s,c+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & idx.class])
			}
			if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & mast$sets[[turb.set]]$data$v.avg[start:end]>=bins[num.classes]])<3) turb.tbl[s,num.classes+1] <- NA
			else turb.tbl[s,num.classes+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & mast$sets[[turb.set]]$data$v.avg[start:end]>=bins[num.classes]])
		}
	}
	if(length(mast$sets[[turb.set]]$data$turb.int[start:end])<3) turb.tbl[num.sectors+1,1] <- NA
	else turb.tbl[num.sectors+1,1] <- mean(mast$sets[[turb.set]]$data$turb.int[start:end], na.rm=TRUE)
	
	if(!is.null(bins)) {
		for(i in 1:(num.classes-1)) {
			# index for wind class
			idx.class <- mast$sets[[turb.set]]$data$v.avg[start:end]>=bins[i] & mast$sets[[turb.set]]$data$v.avg[start:end]<bins[i+1]
			if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.class])<3) turb.tbl[num.sectors+1,i+1] <- NA
			else turb.tbl[num.sectors+1,i+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.class], na.rm=TRUE)
		}
		if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & mast$sets[[turb.set]]$data$v.avg[start:end]>=bins[num.classes]])<3) turb.tbl[num.sectors+1,num.classes+1] <- NA
		else turb.tbl[num.sectors+1,num.classes+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & mast$sets[[turb.set]]$data$v.avg[start:end]>=bins[num.classes]])
	}
	
	r.names <- c(paste("s", 1:num.sectors, sep=""),"all")
	if(num.sectors==4) r.names <- c("n","e","s","w","all")
	if(num.sectors==8) r.names <- c("n","ne","e","se","s","sw","w","nw","all")
	if(num.sectors==12) r.names <- c("n","nne","ene","e","ese","sse","s","ssw","wsw","w","wnw","nnw","all")
	if(num.sectors==16) r.names <- c("n","nne","ne","ene","e","ese","se","sse","s","ssw","sw","wsw","w","wnw","nw","nnw","all")
	turb.tbl <- data.frame(turb.tbl, row.names=r.names)
	c.names <- c("total")
	if(!is.null(bins)) {
		for(i in 1:(num.classes-1)) c.names <- append(c.names, paste(bins[i], bins[i+1], sep="-"))
		c.names <- append(c.names, paste(">", bins[num.classes], sep=""))
	}
	names(turb.tbl) <- c.names
	
	for(i in 1:length(turb.tbl)) turb.tbl[,i][is.nan(turb.tbl[,i]) | turb.tbl[,i]==0] <- NA
	if(sum(turb.tbl[,length(turb.tbl)], na.rm=TRUE)==0) turb.tbl[,length(turb.tbl)] <- NULL
	
	attr(turb.tbl, "call") <- list(func="turbulence", mast=deparse(substitute(mast)), turb.set=turb.set, dir.set=dir.set, num.sectors=num.sectors, bins=bins, subset=subset, digits=digits, print=print)
	
	turb.tbl <- round(turb.tbl, digits)
	if(print) printObject(turb.tbl)
	invisible(turb.tbl)
}
