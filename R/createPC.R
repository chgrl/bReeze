createPC <-
function(v, p, cp, ct, rho=1.225, rated.p, desc) {
###	creating power curve object

	if(missing(v)) stop("Wind speed 'v' is mandatory\n")
	if(!is.vector(v)) stop("'v' requires numeric vector\n")
	if(missing(p)) stop("Power 'p' is mandatory\n")
	if(!is.vector(p)) stop("'p' requires numeric vector\n")
	if(length(v)!=length(p)) stop("Different vector length of 'v' and 'p'\n")
	if(missing(cp)) cp <- NULL
	if(!is.null(cp)) if(!is.vector(cp)) stop("'cp' requires numeric vector\n")
	if(!is.null(cp)) if(length(v)!=length(cp)) stop("Different vector length of 'v' and 'p'\n")
	if(missing(ct)) ct <- NULL
	if(!is.null(ct)) if(!is.vector(ct)) stop("'ct' requires numeric vector\n")
	if(!is.null(ct)) if(length(v)!=length(ct)) stop("Different vector length of 'v' and 'cp'\n")
	if(missing(rated.p)) rated.p <- max(p, na.rm=TRUE)
	if(missing(desc)) desc <- NULL
	
	pc <- data.frame(cbind(v, p, cp, ct))
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
