frequency <-
function(mast, v.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE) {
### calculating mean wind speed and frequency of sectors

	if(class(mast)!="mast") stop(paste(substitute(mast), "is no mast object"))
	num.sets <- length(mast$sets)
	if(!missing(v.set) && missing(dir.set)) dir.set <- v.set
	if(missing(v.set) && !missing(dir.set)) v.set <- dir.set
	
	if(!is.numeric(v.set)) v.set <- match(v.set, names(mast$sets))
	if(is.na(v.set)) stop("'v.set' not found\n")
	if(!is.numeric(dir.set)) dir.set <- match(dir.set, names(mast$sets))
	if(is.na(dir.set)) stop("'dir.set' not found\n")
	
	if(!is.numeric(num.sectors)) stop("'num.sectors' must be numeric\n")
	if(num.sectors<=1) stop("There must be at least 2 sectors\n")
	if(v.set<=0 || v.set>num.sets) stop("'v.set' not found\n")
	if(dir.set<=0 || dir.set>num.sets) stop("'dir.set' not found\n")
	if(is.null(mast$sets[[v.set]]$data$v.avg)) stop("Specified set does not contain average wind speed data\n")
	if(is.null(mast$sets[[dir.set]]$data$dir.avg)) stop("Specified set does not contain wind direction data\n")
	if(any(bins<0)) stop("'bins' must be NULL or a vector of positives\n")
	
	sector.width <- 360/num.sectors
	sectors <- seq(0, 360-sector.width, by=sector.width)
	sector.edges <- c(sectors-sector.width/2, tail(sectors, n=1)+sector.width/2)%%360
	if(!is.null(bins)) if(head(bins, 1)!=0) bins <- c(0, bins)
	num.classes <- length(bins)
	v.max <- max(mast$sets[[v.set]]$data$v.avg, na.rm=TRUE)
	if(num.classes>2) {
		for(i in (num.classes-1):2) {
			if(bins[i+1]>=v.max & bins[i]>=v.max) {
				bins <- head(bins, -1)
				num.classes <- length(bins)
			}
		}
	}
	if(!is.null(bins)) if(num.classes==2 && bins[num.classes]>=v.max) stop("Only one wind class found\n")
	
	freq.tbl <- matrix(NA, nrow=num.sectors+1, ncol=num.classes+2)
	# index for valid data
	idx.val <- !is.na(mast$sets[[v.set]]$data$v.avg) & !is.na(mast$sets[[dir.set]]$data$dir.avg) & mast$sets[[v.set]]$data$v.avg >= 0
	
	for(s in 1:num.sectors) {
		# index for direction
		low <- sector.edges[s]
		high <- sector.edges[s+1]
		if(low<high) idx.dir <- mast$sets[[dir.set]]$data$dir.avg>=low & mast$sets[[dir.set]]$data$dir.avg<high
		else idx.dir <- mast$sets[[dir.set]]$data$dir.avg>=low | mast$sets[[dir.set]]$data$dir.avg<high
		
		freq.tbl[s,1] <- mean(mast$sets[[v.set]]$data$v.avg[idx.val & idx.dir])
		freq.tbl[s,2] <- length(mast$sets[[v.set]]$data$v.avg[idx.val & idx.dir]) * 100 / length(mast$sets[[dir.set]]$data$dir.avg[idx.val])
		if(!is.null(bins)) {
			for(c in 1:(num.classes-1)) {
				# index for wind class
				idx.class <- mast$sets[[v.set]]$data$v.avg>=bins[c] & mast$sets[[v.set]]$data$v.avg<bins[c+1]
				freq.tbl[s,c+2] <- length(mast$sets[[v.set]]$data$v.avg[idx.val & idx.dir & idx.class]) * 100 / length(mast$sets[[dir.set]]$data$dir.avg[idx.val])
			}
		}
		if(!is.null(bins)) {
			freq.tbl[s,num.classes+2] <- length(mast$sets[[v.set]]$data$v.avg[idx.val & idx.dir & mast$sets[[v.set]]$data$v.avg>=bins[num.classes]]) * 100 / length(mast$sets[[dir.set]]$data$dir.avg[idx.val])
		}
	}
	freq.tbl[num.sectors+1,1] <- mean(mast$sets[[v.set]]$data$v.avg, na.rm=TRUE)
	freq.tbl[num.sectors+1,2] <- sum(freq.tbl[1:num.sectors,2], na.rm=TRUE)
	
	if(!is.null(bins)) for(i in 3:(num.classes+2)) freq.tbl[num.sectors+1,i] <- sum(freq.tbl[1:num.sectors,i], na.rm=TRUE)
	
	r.names <- c(paste("s", 1:num.sectors, sep=""),"all")
	if(num.sectors==4) r.names <- c("n","e","s","w","all")
	if(num.sectors==8) r.names <- c("n","ne","e","se","s","sw","w","nw","all")
	if(num.sectors==12) r.names <- c("n","nne","ene","e","ese","sse","s","ssw","wsw","w","wnw","nnw","all")
	if(num.sectors==16) r.names <- c("n","nne","ne","ene","e","ese","se","sse","s","ssw","sw","wsw","w","wnw","nw","nnw","all")
	freq.tbl <- data.frame(freq.tbl, row.names=r.names)
	c.names <- c("wind.speed","total")
	if(!is.null(bins)) {
		for(i in 1:(num.classes-1)) c.names <- append(c.names, paste(bins[i], bins[i+1], sep="-"))
		c.names <- append(c.names, paste(">", bins[num.classes], sep=""))
	}
	names(freq.tbl) <- c.names
	
	for(i in 1:length(freq.tbl)) freq.tbl[,i][is.nan(freq.tbl[,i]) | freq.tbl[,i]==0] <- NA
	if(sum(freq.tbl[,length(freq.tbl)], na.rm=TRUE)==0) freq.tbl[,length(freq.tbl)] <- NULL
	
	unit <-
	attr(freq.tbl, "units") <- c(attr(mast$sets[[v.set]]$data$v.avg, "unit"), "%")
	attr(freq.tbl, "call") <- list(func="frequency", mast=deparse(substitute(mast)), v.set=v.set, dir.set=dir.set, num.sectors=num.sectors, bins=bins, digits=digits, print=print)
	class(freq.tbl) <- "frequency"
	
	freq.tbl <- round(freq.tbl, digits)
	if(print) printObject(freq.tbl)
	invisible(freq.tbl)
}
