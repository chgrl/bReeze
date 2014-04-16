createMast <-
function(time.stamp, ..., loc=NULL, desc=NULL) {
### creating met mast from several datasets

	if(missing(time.stamp)) stop("'time.stamp' is mandatory")
	if(!is.null(loc)) {
		if(!is.vector(loc)) stop("'location' must be a vector of latitude and longitude")
		if(length(loc)!=2) stop("'location' must be a numeric vector of latitude and longitude")
		if(!is.numeric(loc)) stop("'location' must be a numeric vector of decimal degrees")
		if(loc[1]>90 || loc[1]<(-90) || loc[2]>180 || loc[2]<(-180)) stop("Coordinates in 'location' out of range - please use decimal degrees")
	}
	
	l <- list(...)
	
	# check sets and time stamp
	num.sets <- length(l)
	if(num.sets<1) stop("No data - please add at least one set created by createSet")
	for(i in 1:num.sets) {
		if(is.null(attr(l[[i]], "call"))) stop(names(l)[i], " is no set object - please use createSet")
		if(attr(l[[i]], "call")$func!="createSet") stop(names(l)[i], " is no set object - please use createSet")
		attr(l[[i]], "call") <- NULL
	}
	if(any(class(time.stamp)=="POSIXlt")==FALSE) stop("'time.stamp' must be given in POSIXlt format - for reformating use formatTS")
	if(length(time.stamp)!=length(l[[1]]$data[,1])) stop("Different length of time.stamp and sets")
	
	# check units
	var.names <- NULL
	for(i in 1:num.sets) var.names <- append(var.names, names(l[[i]]$data))
	var.names <- unique(var.names)
	units <- data.frame(matrix(NA, nrow=num.sets, ncol=length(var.names)+1))
	names(units) <- c("height", var.names)
	
	for(i in 1:num.sets) {
		units[i,1] <- attr(l[[i]]$height, "unit")
		for(j in 2:dim(units)[2]) {
			if(!is.null(l[[i]]$data[1,names(units)[j]])) if(!is.null(attr(l[[i]]$data[,names(units)[j]], "unit"))) units[i,j] <- attr(l[[i]]$data[,names(units)[j]], "unit")
		}
	}
	
	for(i in 1:dim(units)[2]) {
		if(length(unique(units[,i])[!is.na(unique(units[,i]))])>1) stop("Different units in ", names(units)[i], " - ", paste(unique(units[,i])[!is.na(unique(units[,i]))], collapse=", "))
	}
	
	# sort by heights
	if(length(l)>1) {
		heights <- c()
		for(i in 1:length(l)) heights <- append(heights, l[[i]]$height)
		height.idx <- cbind(heights, seq(1:length(heights)))
		l <- l[height.idx[order(height.idx[,1], decreasing=TRUE),][,2]]
	}
	
	# name sets
	if(is.null(names(l))) for(i in 1:length(l)) names(l)[i] <- paste0("set", i)
	else for(i in 1:length(l)) if(names(l)[i]=="") names(l)[i] <- paste0("set", i)
	
	if(is.null(loc)) {
		if(is.null(desc)) r <- list(time.stamp=time.stamp, sets=l)
		else r <- list(time.stamp=time.stamp, description=desc, sets=l)
	} else {
		if(is.null(desc)) r <- list(time.stamp=time.stamp, location=loc, sets=l)
		else r <- list(time.stamp=time.stamp, location=loc, description=desc, sets=l)
	}
	attr(r, "call") <- list(func="createMast")
	
	return(r)
}
