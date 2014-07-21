print.energy <- function(x, ...) {
### summarising energy object information
	
	cat("\n\tWind energy content\n\n")
	row.names(x) <- c(toupper(head(attr(x, "row.names"), -1)), tail(attr(x, "row.names"), 1))
	x[x==0] <- ""
	print(x, quote=FALSE)
	cat("\t(all values in ", attr(x, "unit"), ")\n", sep="")
	cat("\ncall: energy(wb=", attr(x, "call")$wb, ", rho=", attr(x, "call")$rho, ", bins=c(", paste(attr(x, "call")$bins, collapse=", "), "), digits=", attr(x, "call")$digits, ", print=", attr(x, "call")$print, ")\n\n", sep="")
}
