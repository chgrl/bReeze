pc <- function(x, ...) {
	# this is not really the way the S3 system should work, is it?
	if(any(class(x)=="integer") || any(class(x)=="numeric")) r <- pc.default(x, ...)
	else if(any(class(x)=="character")) r <- pc.read(x, ...)
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
