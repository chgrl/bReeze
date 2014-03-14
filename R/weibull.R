weibull <-
function(mast, v.set, dir.set, num.sectors=12, digits=3, print=TRUE) {
### calculating weibull parameters for sectors
	
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
	
	sector.width <- 360/num.sectors
	sectors <- seq(0, 360-sector.width, by=sector.width)
	sector.edges <- c(sectors-sector.width/2, tail(sectors, n=1)+sector.width/2)%%360
		
	weibull.tbl <- matrix(NA, nrow=num.sectors+1, ncol=4)
	for(s in 1:num.sectors) {
		low <- sector.edges[s]
		high <- sector.edges[s+1]
		if(low<high) sector.idx <- mast$sets[[dir.set]]$data$dir.avg>=low & mast$sets[[dir.set]]$data$dir.avg<high
		else sector.idx <- mast$sets[[dir.set]]$data$dir.avg>=low | mast$sets[[dir.set]]$data$dir.avg<high
		
		weibull.param <- weibullInt(mast$sets[[v.set]]$data$v.avg[sector.idx], FALSE)
		weibull.tbl[s,1] <- weibull.param$A
		weibull.tbl[s,2] <- weibull.param$k
	}
	weibull.param <- weibullInt(mast$sets[[v.set]]$data$v.avg, TRUE)
	weibull.tbl[num.sectors+1,1] <- weibull.param$A
	weibull.tbl[num.sectors+1,2] <- weibull.param$k
	
	freq <- frequency(mast, v.set, dir.set, num.sectors, bins=NULL, digits=digits, print=FALSE)
	weibull.tbl[,3] <- freq$wind.speed
	weibull.tbl[,4] <- freq$total

	r.names <- c(paste("s", 1:num.sectors, sep=""),"all")
	if(num.sectors==4) r.names <- c("n","e","s","w","all")
	if(num.sectors==8) r.names <- c("n","ne","e","se","s","sw","w","nw","all")
	if(num.sectors==12) r.names <- c("n","nne","ene","e","ese","sse","s","ssw","wsw","w","wnw","nnw","all")
	if(num.sectors==16) r.names <- c("n","nne","ne","ene","e","ese","se","sse","s","ssw","sw","wsw","w","wnw","nw","nnw","all")
	weibull.tbl <- data.frame(weibull.tbl, row.names=r.names)
	names(weibull.tbl) <- c("A", "k", "wind.speed", "frequency")
	
	attr(weibull.tbl, "units") <- c("m/s", "-", attr(mast$sets[[v.set]]$data$v.avg, "unit"), "%")
	attr(weibull.tbl, "call") <- list(func="weibull", mast=deparse(substitute(mast)), v.set=v.set, dir.set=dir.set, num.sectors=num.sectors, digits=digits, print=print)
	class(weibull.tbl) <- "weibull"
	
	weibull.tbl <- round(weibull.tbl, digits)
	if(print) printObject(weibull.tbl)
	invisible(weibull.tbl)
}
