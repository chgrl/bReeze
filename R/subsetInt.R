subsetInt <-
function(time.stamp, subset) {
###	internal function for data subsets
	
	num.samples <- length(time.stamp)
	if((!any(is.character(subset)) && !any(is.na(subset))) || length(subset)!=2) stop("Please specify 'subset' as vector of start and end time stamp")
	if(is.na(subset[1])) subset[1] <- as.character(time.stamp[1])
	if(is.na(subset[2])) subset[2] <- as.character(time.stamp[num.samples])
	if(nchar(subset[1])==10) subset[1] <- paste(subset[1], "00:00:00")
	if(nchar(subset[2])==10) subset[2] <- paste(subset[2], "00:00:00")
	start <- strptime(subset[1], "%Y-%m-%d %H:%M:%S")
	end <- strptime(subset[2], "%Y-%m-%d %H:%M:%S")
	if(is.na(start)) stop("'start' time stamp in 'subset' not correctly formated")
	if(is.na(end)) stop("'end' time stamp in 'subset' not correctly formated")
	if(start<time.stamp[1] || start>time.stamp[num.samples]) stop("'start' time stamp in 'subset' not in period")
	if(end<time.stamp[1] || end>time.stamp[num.samples]) stop("'end' time stamp in 'subset' not in period")
	
	match.date <- difftime(time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(start, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	start <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))	
	match.date <- difftime(time.stamp, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days") - difftime(end, ISOdatetime(1,1,1,0,0,0), tz="GMT", units="days")
	end <- which(abs(as.numeric(match.date)) == min(abs(as.numeric(match.date))))	
		
	return(cbind(start, end))
}
