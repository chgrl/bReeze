formatTS <-
function(time.stamp, pattern) {
### formatting time stamp (lookup or with given pattern)
	
	if(anyDuplicated(time.stamp)) if(any(duplicated(time.stamp)==TRUE)) stop("'time.stamp' contains duplicates\n") # sometimes anyDuplicated() founds duplicates although there are no duplicates
	ts <- nts <- NULL
		
	if(missing(pattern)) { # search for pattern
		pattern.list <- read.table(system.file(package="bReeze", "ts_patterns", "patterns.txt"), sep=",")
		pattern <- as.vector(unlist(pattern.list))
				
		for(i in 1:length(pattern)) {
			nts <- strptime(time.stamp[1], pattern[i])
			if(is.na(nts)) {
				if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="S") nts <- strptime(paste(time.stamp[1], "00:00:00"), pattern[i])
				if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="M") nts <- strptime(paste(time.stamp[1], "00:00"), pattern[i])
			}
			if(!is.na(nts) && substr(nts,1,2)!="00") {
				nts <- strptime(time.stamp, pattern[i])
				if(any(is.na(nts)==TRUE)) {
					if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="S") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00:00"), pattern[i])
					if(substr(pattern[i], nchar(pattern[i]), nchar(pattern[i]))=="M") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00"), pattern[i])
				}
				if(!any(is.na(nts)==TRUE)) {
					cat(paste("Pattern found:", pattern[i], "\n"))
					break
				}
			}
		}
				
		if(length(nts)==1) stop("No pattern found\n")
	} else { # pattern specified
		nts <- strptime(time.stamp, pattern)
		if(any(is.na(nts)==TRUE)) {
			if(substr(pattern, nchar(pattern), nchar(pattern))=="S") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00:00"), pattern)
			if(substr(pattern, nchar(pattern), nchar(pattern))=="M") nts[which(is.na(nts)==TRUE)] <- strptime(paste(time.stamp[which(is.na(nts)==TRUE)], "00:00"), pattern)
		}
		if(any(is.na(nts)==TRUE) || substr(nts[1],1,2)=="00") stop("Pattern does not match\n")
	}
	
	return(nts)
}
