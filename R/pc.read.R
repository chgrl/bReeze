pc.read <-
function(x, ex=FALSE, ...) {
### importing power curve from WAsP .wgt file or WindPower program .pow file
	
	if(!ex) {
		if(system.file(package="bReeze", "powercurves", x)=="") stop("Power curve not found in package collection. To read external power curve files set 'ex' to TRUE")
		x <- system.file(package="bReeze", "powercurves", x)
	}
	
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
		r <- pc(x=v, p=p, rho=1.225, desc=desc)
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
		r <- pc(x=v, p=p, ct=ct, rho=rho, desc=desc)
		attr(r, "call") <- list(func="readPC", x=x, ex=ex)
	}
	
	class(r) <- "pc"	
	return(r)
}
