pc <- function(x, ...) {
	# this is not really the way the S3 system should work, is it?
	if(any(is.numeric(x))) r <- pc.default(x, ...)
	else if(any(is.character(x))) r <- pc.read(x, ...)
	else stop(substitute(x) , "must be a numeric vector of wind speeds, the name of, or the path to a 'wgt' or 'pow' file containing power curve data")
	return(r)
}


pc.default <-
function(x, p, cp, ct, rho=1.225, rated.p, desc, ...) {
###	creating power curve object

	if(missing(x)) stop("Wind speed 'v' is mandatory")
	if(!is.vector(x)) stop("'v' requires numeric vector")
	if(missing(p)) stop("Power 'p' is mandatory")
	if(!is.vector(p)) stop("'p' requires numeric vector")
	if(length(x)!=length(p)) stop("Different vector length of 'v' and 'p'")
	if(missing(cp)) cp <- NULL
	if(!is.null(cp)) if(!is.vector(cp)) stop("'cp' requires numeric vector")
	if(!is.null(cp)) if(length(x)!=length(cp)) stop("Different vector length of 'v' and 'p'")
	if(missing(ct)) ct <- NULL
	if(!is.null(ct)) if(!is.vector(ct)) stop("'ct' requires numeric vector")
	if(!is.null(ct)) if(length(x)!=length(ct)) stop("Different vector length of 'v' and 'cp'")
	if(missing(rho)) rho <- 1.225
	if(missing(rated.p)) rated.p <- max(p, na.rm=TRUE)
	if(missing(desc)) desc <- NULL
	
	pc <- data.frame(cbind(x, p, cp, ct))
	names <- c("v", "P")
	attr(pc, "units") <- c("m/s", "kW")
	if(!is.null(cp)) {
		names <- append(names, "cp")
		attr(pc, "units") <- append(attr(pc, "units"), "-")
	}
	if(!is.null(ct)) {
		names <- append(names, "ct")
		attr(pc, "units") <- append(attr(pc, "units"), "-")
	}
	names(pc) <- names
	attr(pc, "rho") <- rho
	attr(pc, "rated.power") <- rated.p
	if(!is.null(desc)) attr(pc, "description") <- desc
	attr(pc, "call") <- list(func="createPC")
	
	class(pc) <- "pc"
	return(pc)
}


pc.read <-
function(x, ex=FALSE, ...) {
### importing power curve from WAsP .wgt file or WindPower program .pow file
	
	#if(!ex) {
	#	if(system.file(package="bReeze", "powercurves", x)=="") stop("Power curve not found in package collection. To read external power curve files set 'ex' to TRUE")
	#	x <- system.file(package="bReeze", "powercurves", x)
	#}
	
	if(system.file(package="bReeze", "powercurves", x)!="") x <- system.file(package="bReeze", "powercurves", x)
	if(!file.exists(x)) stop("File not found")
	
	type <- substr(x, nchar(x)-3, nchar(x))
	if(!any(c(".pow", ".wtg")==type)) stop("Cannot handle file - only WAsP .wtg files and WindPower program .pow files are supported")
	
	r <- NULL
	if(type==".pow") {
		pow <- read.table(x, as.is=TRUE)
		cut.out <- as.numeric(pow[4,1])
		if(is.na(cut.out) || is.null(cut.out)) stop("Cannot handle file")
		v <- seq(1, cut.out, 1)
		p <- tail(pow, -1)
		suppressWarnings(if(is.na(as.numeric(tail(p, 1)))) p <- head(p, -1))
		p <- as.numeric(p[5:(cut.out+4),1])
		desc <- pow[1,1]
		r <- pc.default(x=v, p=p, rho=1.225, desc=desc)
		attr(r, "call") <- list(func="readPC", x=x)
	} else if(type==".wtg") {
		stopifnot(require(XML))
		wtg <- xmlTreeParse(x, asTree=TRUE)
		if(is.null(wtg$doc$children$WindTurbineGenerator)) stop("Cannot handle file")
		n <- length(wtg$doc$children$WindTurbineGenerator)
		idx <- 3
		if(n>4) {
			rho <- NULL
			for(i in 3:(n-1)) rho <- append(rho, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[i]])[["AirDensity"]]))
			idx <- which.min(abs(rho-1.225))+2
		}
		rho <- as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]])[["AirDensity"]])
		n <- length(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]])
		v <- p <- ct <- NULL
		for(i in 1:n) {
			v <- append(v, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]][[i]])[["WindSpeed"]]))
			p <- append(p, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]][[i]])[["PowerOutput"]])/1000)
			ct <- append(ct, as.numeric(xmlAttrs(wtg$doc$children$WindTurbineGenerator[[idx]][["DataTable"]][[i]])[["ThrustCoEfficient"]]))
		}
		desc <- xmlAttrs(xmlRoot(wtg))[["Description"]]
		r <- pc.default(x=v, p=p, ct=ct, rho=rho, desc=desc)
		attr(r, "call") <- list(func="readPC", x=x, ex=ex)
	}
	
	class(r) <- "pc"	
	return(r)
}
