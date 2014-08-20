printObject <- function(object) {
### summarising object information
	
	if(is.null(attr(object, "call"))) stop(substitute(object), " seems not to be a bReeze object")
	
	if(attr(object, "call")$func=="createMast") { # mast object
		if(!is.null(object$location)) loc <- object$location
		if(!is.null(object$description)) desc <- object$description
		num.sets <- length(object$sets)
		num.samples <- nrow(object$sets[[1]]$data)
		heights <- object$sets[[1]]$height
		h.unit <- attr(object$sets[[1]]$height, "unit")
		interval <- object$timestamp[2]-object$timestamp[1]
		if(attr(interval, "units")=="days" && interval>1) warning("Availability cannot be calculated - time interval longer than 1 day", call.=FALSE)
		if(attr(interval, "units")=="days") daily.samples <- interval
		if(attr(interval, "units")=="hours") daily.samples <- 24/as.numeric(interval)
		if(attr(interval, "units")=="mins") daily.samples <- 24*60/as.numeric(interval)
		if(attr(interval, "units")=="secs") daily.samples <- 24*60*60/as.numeric(interval)
		period.start <- as.character(object$timestamp[1])
		period.end <- as.character(object$timestamp[num.samples])
		if(nchar(period.start)==10) period.start <- paste(period.start, "00:00:00")
		if(nchar(period.end)==10) period.end <- paste(period.end, "00:00:00")
		period.start <- strptime(period.start, format="%Y-%m-%d %H:%M:%S")
		period.end <- strptime(period.end, format="%Y-%m-%d %H:%M:%S")
		period.days <- as.numeric(period.end-period.start)
		signals <- names(object$sets[[1]]$data)
		if(is.null(object$sets[[1]]$data$v.avg)) wind.speed <- 0
		else wind.speed <- mean(object$sets[[1]]$data$v.avg, na.rm=TRUE); v.unit <- attr(object$sets[[1]]$data$v.avg, "unit")
		if(is.null(object$sets[[1]]$data$v.avg) || is.null(object$sets[[1]]$data$dir.avg)) avail <- 0 
		else avail <- sum(!is.na(object$sets[[1]]$data$v.avg) & !is.na(object$sets[[1]]$data$dir.avg)) * 100 / (daily.samples*period.days)
		if(is.null(attr(object$sets[[1]]$data, "clean"))) clean <- "not cleaned"
		else clean <- unlist(attr(object$sets[[1]]$data, "clean"))
			
		if(num.sets>1) {
			signals <- list(signals)
			clean <- list(clean)
			for(i in 2:num.sets) {
				heights <- append(heights, object$sets[[i]]$height)
				signals[[i]] <- names(object$sets[[i]]$data)
				if(is.null(object$sets[[i]]$data$v.avg)) wind.speed <- 0 
				else wind.speed <- append(wind.speed, mean(object$sets[[i]]$data$v.avg, na.rm=TRUE)); v.unit <- attr(object$sets[[i]]$data$v.avg, "unit")
				if(is.null(object$sets[[i]]$data$v.avg) || is.null(object$sets[[i]]$data$dir.avg)) avail <- append(avail, 0)
				else avail <- append(avail, sum(!is.na(object$sets[[i]]$data$v.avg) & !is.na(object$sets[[i]]$data$dir.avg)) * 100 / (daily.samples*period.days))
				if(is.null(attr(object$sets[[i]]$data, "clean"))) clean[[i]] <- "not cleaned"
				else clean[[i]] <- unlist(attr(object$sets[[i]]$data, "clean"))
			}
		}
		
		attr(heights, "unit") <- h.unit
		attr(avail, "unit") <- "%"
		attr(wind.speed, "unit") <- v.unit
		
		if(is.null(object$location)) {
			if(is.null(object$description)) r <- list(num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, clean=clean)
			else r <- list(description=desc, num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, clean=clean)
		} else {
			if(is.null(object$description)) r <- list(location=loc, num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, clean=clean)
			else r <- list(location=loc, description=desc, num.sets=num.sets, heights=heights, signals=signals, period=list(period.start=period.start, period.end=period.end, period.days=period.days), num.samples=num.samples, wind.speed=wind.speed, availability=avail, clean=clean)
		}
		
		cat(paste("\n\tMet mast", substitute(object), "\n\n"))
		if(!is.null(object$location)) {
			if(object$location[1]<0) ns <- " degree South, " else ns <- " degree North, "
			if(object$location[2]<0) we <- " degree West" else we <- " degree East"
			cat("location: ", abs(object$location[1]), ns, abs(object$location[2]), we, "\n", sep="")
		}
		if(!is.null(object$description)) cat("description:", object$description, "\n\n")
		cat(paste0("measuring period: from ", period.start, " to ", period.end, " (", round(period.days, 1), " days)\n"))
		cat("samples:", num.samples, "\n\n")
		cat("datasets (", num.sets, "):\n", sep="")
		det <- data.frame(cbind(heights, round(wind.speed, 2), round(avail, 1)))
		tbl.units <- data.frame(t(names(det)))
		tbl.units[,1] <- paste0("[", h.unit, "]")
		tbl.units[,2] <- paste0("[", v.unit, "]")
		tbl.units[,3] <- "[%]"
		det[is.na(det)] <- ""
		det <- as.data.frame(lapply(det, as.character))
		names(det) <- names(tbl.units) <- c("height", "wind speed", "availability")
		row.names(tbl.units) <- " "
		row.names(det) <- names(object$sets)
		print(rbind(tbl.units, det), quote=FALSE)
		sig <- unique(unlist(signals))
		cat("\nsignals (", length(sig), "):\n", sep="")
		sig.tbl <- data.frame(matrix(NA, nrow=length(sig), ncol=num.sets))
		row.names(sig.tbl) <- sig
		names(sig.tbl) <- c(names(object$sets))
		if(num.sets==1) {
			signals <- list(signals)
			clean <- list(clean)
		}
		for(i in 1:length(sig)) for(j in 1:num.sets) if(any(signals[[j]]==row.names(sig.tbl)[i])) sig.tbl[i,j] <- "o"
		for(j in 1:num.sets) {
			if(length(clean[[j]])!=1 && clean[[j]][1]!="not cleaned") {
				if(any(names(clean[[j]])=="v.avg.min" || names(clean[[j]])=="v.avg.max")) if(any(signals[[j]]=="v.avg")) sig.tbl[which(row.names(sig.tbl)=="v.avg"),j] <- "c"
				if(any(names(clean[[j]])=="dir.clean")) if(clean[[j]][names(clean[[j]])=="dir.clean"]) if(any(signals[[j]]=="dir.avg")) sig.tbl[which(row.names(sig.tbl)=="dir.avg"),j] <- "c"
				if(any(names(clean[[j]])=="icing")) if(clean[[j]][names(clean[[j]])=="icing"]) if(any(signals[[j]]=="dir.avg")) sig.tbl[which(row.names(sig.tbl)=="dir.avg"),j] <- "c"
				if(any(names(clean[[j]])=="turb.clean")) if(any(signals[[j]]=="turb.int")) sig.tbl[which(row.names(sig.tbl)=="turb.int"),j] <- "c"
			}
		}
		sig.tbl[is.na(sig.tbl)] <- ""
		print(sig.tbl, quote=FALSE)
		cat("\t(o=original data, c=cleaned data)\n")
		
		invisible(r)
	} else stop(substitute(object), " seems not to be a bReeze object")
}
