turbulence <-
function(mast, turb.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE) {
### calculating mean wind speed and turbulence intensity of sectors

	if(class(mast)!="mast") stop(paste(substitute(mast), "is no mast object"))
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
	
	turb.tbl <- matrix(NA, nrow=num.sectors+1, ncol=num.classes+1)
	# indices for valid data
	idx.val <- !is.na(mast$sets[[turb.set]]$data$turb.int) & !is.na(mast$sets[[dir.set]]$data$dir.avg)
	idx.v <- !is.na(mast$sets[[turb.set]]$data$v.avg)
	
	for(s in 1:num.sectors) {
		# index for direction
		low <- sector.edges[s]
		high <- sector.edges[s+1]
		if(low<high) idx.dir <- mast$sets[[dir.set]]$data$dir.avg>=low & mast$sets[[dir.set]]$data$dir.avg<high
		else idx.dir <- mast$sets[[dir.set]]$data$dir.avg>=low | mast$sets[[dir.set]]$data$dir.avg<high
		
		if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.dir])<3) turb.tbl[s,1] <- NA
		else turb.tbl[s,1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.dir])
		if(!is.null(bins)) {
			for(c in 1:(num.classes-1)) {
				# index for wind class
				idx.class <- mast$sets[[turb.set]]$data$v.avg>=bins[c] & mast$sets[[turb.set]]$data$v.avg<bins[c+1]
				if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & idx.class])<3) turb.tbl[s,c+1] <- NA
				else turb.tbl[s,c+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & idx.class])
			}
			if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & mast$sets[[turb.set]]$data$v.avg>=bins[num.classes]])<3) turb.tbl[s,num.classes+1] <- NA
			else turb.tbl[s,num.classes+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.dir & mast$sets[[turb.set]]$data$v.avg>=bins[num.classes]])
		}
	}
	if(length(mast$sets[[turb.set]]$data$turb.int)<3) turb.tbl[num.sectors+1,1] <- NA
	else turb.tbl[num.sectors+1,1] <- mean(mast$sets[[turb.set]]$data$turb.int, na.rm=TRUE)
	
	if(!is.null(bins)) {
		for(i in 1:(num.classes)) {
			# index for wind class
			idx.class <- mast$sets[[turb.set]]$data$v.avg>=bins[i] & mast$sets[[turb.set]]$data$v.avg<bins[i+1]
			if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.class])<3) turb.tbl[num.sectors+1,i+1] <- NA
			else turb.tbl[num.sectors+1,i+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & idx.class])
		}
		if(length(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & mast$sets[[turb.set]]$data$v.avg>=bins[num.classes]])<3) turb.tbl[num.sectors+1,num.classes+1] <- NA
		else turb.tbl[num.sectors+1,num.classes+1] <- mean(mast$sets[[turb.set]]$data$turb.int[idx.val & idx.v & mast$sets[[turb.set]]$data$v.avg>=bins[num.classes]])
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
	
	attr(turb.tbl, "call") <- list(func="turbulence", mast=deparse(substitute(mast)), turb.set=turb.set, dir.set=dir.set, num.sectors=num.sectors, bins=bins, digits=digits, print=print)
	class(turb.tbl) <- "turbulence"
	
	turb.tbl <- round(turb.tbl, digits)
	if(print) printObject(turb.tbl)
	invisible(turb.tbl)
}
