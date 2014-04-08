energy <-
function(wb, rho=1.225, bins=c(5,10,15,20), digits=0, print=TRUE) {
###	calculating wind energy per sector
	
	if(is.null(attr(wb, "call"))) stop(paste(substitute(wb), "is no weibull object\n"))
	if(attr(wb, "call")$func!="weibull") stop(paste(substitute(wb), "is no weibull object\n"))
	if(any(bins<0)) stop("'bins' must be NULL or a vector of positives\n")

	if(is.null(attr(wb, "call")$mast)) stop(paste("Source mast object of", substitute(wb), "could not be found\n"))
	mast <- get(attr(wb, "call")$mast)
	v.set <- attr(wb, "call")$v.set
	dir.set <- attr(wb, "call")$dir.set
	num.sectors <- attr(wb, "call")$num.sectors
	subset <- attr(wb, "call")$subset
	
	# subset
	num.samples <- length(mast$time.stamp)
	start <- strptime(subset[1], "%Y-%m-%d %H:%M:%S")
	end <- strptime(subset[2], "%Y-%m-%d %H:%M:%S")
	match.date <- difftime(mast$time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(start, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	start <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	match.date <- difftime(mast$time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(end, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	end <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))
	
	lim <- c(0, 5*(trunc(ceiling(max(mast$sets[[v.set]]$data$v.avg[start:end], na.rm=TRUE))/5)+1))
	
	if(!is.null(bins)) if(head(bins, 1)!=0) bins <- c(0, bins)
	num.classes <- length(bins)
	v.max <- max(mast$sets[[v.set]]$data$v.avg[start:end], na.rm=TRUE)
	if(num.classes>2) {
		for(i in (num.classes-1):2) {
			if(bins[i+1]>=v.max && bins[i]>=v.max) {
				bins <- head(bins, -1)
				num.classes <- length(bins)
			}
		}
	}
	if(!is.null(bins)) if(num.classes==2 && bins[num.classes]>=v.max) stop("Only one wind class found\n")

	energy.tbl <- data.frame(matrix(NA, nrow=num.sectors+1, ncol=num.classes+1))
	r.names <- c(paste0("s", 1:num.sectors),"all")
	if(num.sectors==4) r.names <- c("n","e","s","w","all")
	if(num.sectors==8) r.names <- c("n","ne","e","se","s","sw","w","nw","all")
	if(num.sectors==12) r.names <- c("n","nne","ene","e","ese","sse","s","ssw","wsw","w","wnw","nnw","all")
	if(num.sectors==16) r.names <- c("n","nne","ne","ene","e","ese","se","sse","s","ssw","sw","wsw","w","wnw","nw","nnw","all")
	row.names(energy.tbl) <- r.names
	c.names <- c("total")
	if(!is.null(bins)) {
		for(i in 1:(num.classes-1)) c.names <- append(c.names, paste(bins[i], bins[i+1], sep="-"))
		c.names <- append(c.names, paste0(">", bins[num.classes]))
	}
	names(energy.tbl) <- c.names
	
	freq <- frequency(mast, v.set, dir.set, num.sectors, bins, subset, print=FALSE)[,-1]
	
	for(i in 1:num.sectors) {
		if(!is.null(bins)) {
			for(j in 2:dim(freq)[2]) energy.tbl[i,j] <- round(energyInt(lim, wb$k[i], wb$A[i], rho)*freq[i,j]/100, digits)
			energy.tbl[i,1] <- round(energyInt(lim, wb$k[i], wb$A[i], rho)*freq[i,1]/100, digits)
		} else {
			energy.tbl[i,1] <- round(energyInt(lim, wb$k[i], wb$A[i], rho)*freq[i]/100, digits)
		}
	}
	for(i in 1:(num.classes+1)) energy.tbl[num.sectors+1,i] <- sum(energy.tbl[1:num.sectors,i], na.rm=TRUE)
	for(i in 1:length(energy.tbl)) energy.tbl[,i][is.nan(energy.tbl[,i]) | energy.tbl[,i]==0] <- NA
	
	if(!is.null(bins)) if(tail(bins,1)>=v.max) energy.tbl[,length(energy.tbl)] <- NULL
	if(sum(energy.tbl[,length(energy.tbl)], na.rm=TRUE)==0) energy.tbl[,length(energy.tbl)] <- NULL
	
	attr(energy.tbl, "unit") <- "kWh/m^2/a"	
	attr(energy.tbl, "call") <- list(func="energy", wb=deparse(substitute(wb)), rho=rho, bins=bins, digits=digits, print=print)
	
	energy.tbl <- round(energy.tbl, digits)
	if(print) printObject(energy.tbl)
	invisible(energy.tbl)
}
