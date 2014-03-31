printObject <- function(object) {
### summarising object information
	
	if(is.null(attr(object, "call"))) stop(paste(substitute(object), "seems not to be a bReeze object\n"))
	
	if(attr(object, "call")$func=="createMast") { # mast object
		if(!is.null(object$location)) loc <- object$location
		if(!is.null(object$description)) desc <- object$description
		num.sets <- length(object$sets)
		num.samples <- nrow(object$sets[[1]]$data)
		heights <- object$sets[[1]]$height
		h.unit <- attr(object$sets[[1]]$height, "unit")
		interval <- object$time.stamp[2]-object$time.stamp[1]
		if(attr(interval, "units")=="days" && interval>1) cat("Availability cannot be calculated - time interval longer than 1 day\n")
		if(attr(interval, "units")=="days") daily.samples <- interval
		if(attr(interval, "units")=="hours") daily.samples <- 24/as.numeric(interval)
		if(attr(interval, "units")=="mins") daily.samples <- 24*60/as.numeric(interval)
		if(attr(interval, "units")=="secs") daily.samples <- 24*60*60/as.numeric(interval)
		period.start <- object$time.stamp[1]
		period.end <- object$time.stamp[num.samples]
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
	} else if(attr(object, "call")$func=="createSet") { # set object
		cat("\n\tDataset", substitute(object), "\n\n")
		if(!is.null(object$description)) cat("description:", object$description, "\n")
		cat("samples:", nrow(object$data), "\n")
		cat("height:", object$height, attr(object$height, "unit"), "\n")
		signals <- names(object$data)
		if(!is.null(object$data$v.avg) && !is.null(object$data$dir.avg)) cat(paste0("availability: ", round(sum(!is.na(object$data$v.avg) & !is.na(object$data$dir.avg)) * 100 / nrow(object$data)), "%\n")) else cat("availability: 0%\n")
		cat("\nsignals:")
		if(is.null(attr(object$data, "clean"))) clean <- "not cleaned"
		else clean <- unlist(attr(object$data, "clean"))
		sig.tbl <- data.frame(matrix(NA, nrow=length(signals), ncol=1))
		names(sig.tbl) <- " "
		row.names(sig.tbl) <- signals
		for(i in 1:length(signals)) if(any(signals==row.names(sig.tbl)[i])) sig.tbl[i,1] <- "(original)"
		if(length(clean)!=1 && clean[1]!="not cleaned") {
			if(any(names(clean)=="v.avg.min" || names(clean)=="v.avg.max")) if(any(signals=="v.avg")) sig.tbl[which(row.names(sig.tbl)=="v.avg"),1] <- "(cleaned) "
			if(any(names(clean)=="dir.clean")) if(clean[names(clean)=="dir.clean"]) if(any(signals=="dir.avg")) sig.tbl[which(row.names(sig.tbl)=="dir.avg"),1] <- "(cleaned) "
			if(any(names(clean)=="icing")) if(clean[names(clean)=="icing"]) if(any(signals=="dir.avg")) sig.tbl[which(row.names(sig.tbl)=="dir.avg"),2] <- "(cleaned) "
			if(any(names(clean)=="turb.clean")) if(any(signals=="turb.int")) sig.tbl[which(row.names(sig.tbl)=="turb.int"),1] <- "(cleaned) "
		}
		sig.tbl[is.na(sig.tbl)] <- ""
		print(sig.tbl, quote=FALSE)
		cat("\n")
	} else if(attr(object, "call")$func=="createPC" || attr(object, "call")$func=="readPC") { # power curve object
		cat("\n\tPower curve", substitute(object), "\n\n")
		if(!is.null(attr(object, "description"))) cat("description:", attr(object, "description"), "\n")
		cat("rated power:", attr(object, "rated.power"), "\n")
		cat("air pressure:", attr(object, "rho"), "\n\n")
		tbl.units <- data.frame(t(names(object)))
		tbl.units[,] <- "[-]"
		tbl.units[,1] <- paste0("[", attr(object, "units")[1], "]")
		tbl.units[,2] <- paste0("[", attr(object, "units")[2], "]")
		object[is.na(object)] <- ""
		obj <- as.data.frame(lapply(object, as.character))
		names(object)[1] <- "wind speed"
		names(object)[2] <- "power"
		names(object)[names(object)=="cp"] <- "power coefficient"
		names(object)[names(object)=="ct"] <- "thrust coefficient"
		names(tbl.units) <- names(obj) <- names(object)
		row.names(tbl.units) <- " "
		row.names(obj) <- as.character(1:nrow(obj))
		print(rbind(tbl.units, obj), quote=FALSE)
		cat("\n")
	} else if(attr(object, "call")$func=="availability") { # availability object
		cat("\n\tAvailability for pairs of wind speed and direction\n\n")
		tot <- object[[1]]$total
		if(length(object)>1) for(i in 2:length(object)) tot <- rbind(tot, object[[i]]$total)
		tbl.units <- data.frame(t(names(tot)))
		tbl.units[,] <- "[d]"
		tbl.units[,1] <- "[%]"
		names(tot)[2:3] <- c("effective period", "total period")
		names(tbl.units) <- names(tot)
		row.names(tbl.units) <- " "
		row.names(tot) <- names(object)
		print(rbind(tbl.units, tot), quote=FALSE)
		cat("\nnumber of daily samples:\n")
		cat(names(object)[1], "\n")
		object[[1]]$daily[is.na(object[[1]]$daily)] <- ""
		names(object[[1]]$daily)[1] <- "%"
		print(object[[1]]$daily, quote=FALSE)
		cat("\n")
		if(length(object)>1) {
			for(i in 2:length(object)) {
				cat(names(object)[i], "\n")
				object[[i]]$daily[is.na(object[[i]]$daily)] <- ""
				names(object[[i]]$daily)[1] <- "%"
				print(object[[i]]$daily, quote=FALSE)
				cat("\n")
			}
		}
		if(attr(object, "call")$v.set[1]=="all") attr(object, "call")$v.set[1] <- "\"all\""
		if(attr(object, "call")$dir.set[1]=="all") attr(object, "call")$dir.set[1] <- "\"all\""
		if(length(attr(object, "call")$v.set)==1) vset <- paste0(", v.set=", attr(object, "call")$v.set)
		else vset <- paste0(", v.set=c(", paste0(attr(object, "call")$v.set, collapse=", "), ")")
		if(length(attr(object, "call")$dir.set)==1) dirset <- paste0(", dir.set=", attr(object, "call")$dir.set)
		else dirset <- paste0(", dir.set=c(", paste0(attr(object, "call")$dir.set, collapse=", "), ")")
		cat("call: availability(mast=", attr(object, "call")$mast, vset, dirset, ", subset=c(\"", paste(attr(object, "call")$subset, collapse="\", \""), "\"), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="monthStats") { # month stats object
		cat("\n\tMonthly statistics\n\n")
		cat(names(object)[1], "\n")
		object[[1]][is.na(object[[1]])] <- ""
		if(length(row.names(object[[1]]))==14) names(object[[1]])[length(names(object[[1]]))] <- row.names(object[[1]])[14] <- gsub("\\.", " ", row.names(object[[1]])[14])
		row.names(object[[1]])[1:12] <- c(toupper(row.names(object[[1]])[1:12]))
		print(object[[1]], quote=FALSE)
		cat("\n")
		if(length(object)>1) {
			for(i in 2:length(object)) {
				cat(names(object)[i], "\n")
				object[[i]][is.na(object[[i]])] <- ""
				if(length(row.names(object[[1]]))==14) names(object[[i]])[length(names(object[[i]]))] <- row.names(object[[i]])[14] <- gsub("\\.", " ", row.names(object[[i]])[14])
				row.names(object[[i]])[1:12] <- c(toupper(row.names(object[[i]])[1:12]))
				print(object[[i]], quote=FALSE)
				cat("\n")
			}
		}
		if(attr(object, "call")$set=="all") attr(object, "call")$set <- "\"all\""
		cat("call: monthStats(mast=", attr(object, "call")$mast, ", set=", attr(object, "call")$set, ", signal=\"", attr(object, "call")$signal, "\", fun=\"", attr(object, "call")$fun, "\", subset=c(\"", paste(attr(object, "call")$subset, collapse="\", \""), "\"), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="frequency") { # frequency object
		cat("\n\tFrequency\n\n")
		object <- as.data.frame(object)
		tbl.units <- data.frame(t(names(object)))
		tbl.units[,] <- paste0("[", attr(object, "unit")[2], "]")
		tbl.units[,1] <- paste0("[", attr(object, "unit")[1], "]")
		object[is.na(object)] <- ""
		obj <- as.data.frame(lapply(object, as.character))
		names(object)[1] <- "wind speed"
		names(tbl.units) <- names(obj) <- names(object)
		row.names(tbl.units) <- " "
		row.names(obj) <- c(toupper(head(row.names(object), -1)), tail(row.names(object), 1))
		print(rbind(tbl.units, obj), quote=FALSE)
		cat("\ncall: frequency(mast=", attr(object, "call")$mast, ", v.set=", attr(object, "call")$v.set, ", dir.set=", attr(object, "call")$dir.set, ", num.sectors=", attr(object, "call")$num.sectors, ", bins=c(", paste(attr(object, "call")$bins, collapse=", "), "), subset=c(\"", paste(attr(object, "call")$subset, collapse="\", \""), "\"), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="turbulence") { # turbulence object
		cat("\n\tTurbulence intensity\n\n")
		row.names(object) <- c(toupper(head(row.names(object), -1)), tail(row.names(object), 1))
		object[is.na(object)] <- ""
		print(object, quote=FALSE)
		cat("\ncall: turbulence(mast=", attr(object, "call")$mast, ", turb.set=", attr(object, "call")$turb.set, ", dir.set=", attr(object, "call")$dir.set, ", num.sectors=", attr(object, "call")$num.sectors, ", bins=c(", paste(attr(object, "call")$bins, collapse=", "), "), subset=c(\"", paste(attr(object, "call")$subset, collapse="\", \""), "\"), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="weibull") { # weibull object
		cat("\n\tWeibull parameters\n\n")
		tbl.units <- data.frame(t(names(object)))
		tbl.units[,1] <- paste0("[", attr(object, "unit")[1], "]")
		tbl.units[,2] <- paste0("[", attr(object, "unit")[2], "]")
		tbl.units[,3] <- paste0("[", attr(object, "unit")[3], "]")
		tbl.units[,4] <- paste0("[", attr(object, "unit")[4], "]")
		obj <- as.data.frame(lapply(object, as.character))
		names(object)[3] <- "wind speed"
		names(tbl.units) <- names(obj) <- names(object)
		row.names(tbl.units) <- " "
		row.names(obj) <- c(toupper(head(row.names(object), -1)), tail(row.names(object), 1))
		print(rbind(tbl.units, obj), quote=FALSE)
		cat("\ncall: weibull(mast=", attr(object, "call")$mast, ", v.set=", attr(object, "call")$v.set, ", dir.set=", attr(object, "call")$dir.set, ", num.sectors=", attr(object, "call")$num.sectors, ", subset=c(\"", paste(attr(object, "call")$subset, collapse="\", \""), "\"), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="energy") { # energy object
		cat("\n\tWind energy content\n\n")
		row.names(object) <- c(toupper(head(row.names(object), -1)), tail(row.names(object), 1))
		object[is.na(object)] <- ""
		print(object, quote=FALSE)
		cat("\t(all values in ", attr(object, "unit"), ")\n", sep="")
		cat("\ncall: energy(wb=", attr(object, "call")$wb, ", rho=", attr(object, "call")$rho, ", bins=c(", paste(attr(object, "call")$bins, collapse=", "), "), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="profile") { # profile object
		cat("\n\tWind profile\n\n")
		tbl.units <- data.frame(t(names(object$profile)))
		tbl.units[,1] <- paste0("[", attr(object$profile, "unit")[1], "]")
		tbl.units[,2] <- paste0("[", attr(object$profile, "unit")[2], "]")
		obj <- as.data.frame(lapply(object$profile, as.character))
		names(object$profile)[2] <- "wind speed"
		names(tbl.units) <- names(obj) <- names(object$profile)
		row.names(tbl.units) <- " "
		row.names(obj) <- c(toupper(head(row.names(object$profile), -1)), tail(row.names(object$profile), 1))
		print(rbind(tbl.units, obj), quote=FALSE)
		cat("\nreference height:", object$h.ref, attr(object$h.ref, "unit"), "\n")
		if(is.null(attr(object, "call")$alpha)) alph <- ", alpha=NULL"
		else {
			if(length(attr(object, "call")$alpha)==1) alph <- paste0(", alpha=", attr(object, "call")$alpha)
			else  alph <- paste0(", alpha=c(", paste0(attr(object, "call")$alpha, collapse=", "), ")")
		}
		if(length(attr(object, "call")$v.set)==1) vset <- paste0(", v.set=", attr(object, "call")$v.set)
		else vset <- paste0(", v.set=c(", paste0(attr(object, "call")$v.set, collapse=", "), ")")
		cat("\ncall: profile(mast=", attr(object, "call")$mast, vset, ", dir.set=", attr(object, "call")$dir.set, ", num.sectors=", attr(object, "call")$num.sectors, ", method=\"", attr(object, "call")$method, "\"", alph, ", subset=c(\"", paste(attr(object, "call")$subset, collapse="\", \""), "\"), digits=", attr(object, "call")$digits, ", print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="aep") { # aep object
		cat("\n\tAnnual energy production\n\n")
		tbl.units <- data.frame(t(names(object$aep)))
		tbl.units[,] <- paste0("[", attr(object$aep[,3], "unit"), "]")
		tbl.units[,1] <- paste0("[", attr(object$aep[,1], "unit"), "]")
		tbl.units[,2] <- paste0("[", attr(object$aep[,2], "unit"), "]")
		object$aep[is.na(object$aep)] <- ""
		obj <- as.data.frame(lapply(object$aep, as.character))
		names(object$aep)[1] <- "wind speed"
		names(tbl.units) <- names(obj) <- names(object$aep)
		row.names(tbl.units) <- " "
		row.names(obj) <- c(toupper(head(row.names(object$aep), -1)), tail(row.names(object$aep), 1))
		print(rbind(tbl.units, obj), quote=FALSE)
		cat("\ncapacity factor:", object$capacity, "\n")
		cat("\ncall: aep(profile=", attr(object, "call")$profile, ", pc=", attr(object, "call")$pc, ", hub.h=", attr(object, "call")$hub.h, ", rho=", attr(object, "call")$rho, ", avail=", attr(object, "call")$avail, ", bins=c(", paste(attr(object, "call")$bins, collapse=", "), "), sectoral=", attr(object, "call")$sectoral, ", digits=c(", paste(attr(object, "call")$digits, collapse=", "), "), print=", attr(object, "call")$print, ")\n\n", sep="")
	} else if(attr(object, "call")$func=="uncertainty") { # uncertainty object
		cat("\n\tUncertainty\n\n")
		ucm <- object$uncertainty.meth
		cat("Uncertainties of applied methods:\n")
		ucm.units <- data.frame(t(names(ucm)))
		ucm.units[,] <- "[%]"
		obj <- as.data.frame(lapply(ucm, as.character))
		names(ucm) <- "uncertainty"
		names(ucm.units) <- names(obj) <- names(ucm)
		row.names(ucm.units) <- " "
		print(rbind(ucm.units, ucm), quote=FALSE)
		pe <- object$prob.exceedance
		cat("\nProbability of exceedance:\n")
		pe.units <- data.frame(t(names(pe)))
		pe.units[,1] <- paste0("[", attr(pe$probability, "unit"), "]")
		pe.units[,2] <- paste0("[", attr(pe$aep, "unit"), "]")
		obj <- as.data.frame(lapply(pe, as.character))
		names(pe)[2] <- "AEP"
		names(pe.units) <- names(obj) <- names(pe)
		row.names(pe.units) <- " "
		row.names(pe) <- row.names(pe)
		print(rbind(pe.units, pe), quote=FALSE)
		cat("\ncall: uncertainty(aep=", attr(object, "call")$aep, ", uc.values=c(", paste(attr(object, "call")$uc.values, collapse=", "), "), uc.names=c(\"", paste(attr(object, "call")$uc.names, collapse="\", \""), "\"), prob=c(", paste(attr(object, "call")$prob, collapse=", "), "), digits=c(", paste(attr(uc, "call")$digits, collapse=", "), "), print=", attr(object, "call")$print, ")\n\n", sep="")
	} else stop(paste(substitute(object), "seems not to be a bReeze object"))
}
