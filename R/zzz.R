.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage("********************")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type changes(\"bReeze\") to see changes/bug fixes, help(bReeze) for documentation")
    packageStartupMessage("or citation(\"bReeze\") for how to cite bReeze.")
    packageStartupMessage(" ")
    packageStartupMessage("********************")
    packageStartupMessage(" ")
}


changes <- 
function(pkg="bReeze") {
    if(pkg=="bReeze") file.show(file.path(system.file(package="bReeze"), "NEWS"))
}


### short name wrapper functions

avail <- function(mast, v.set, dir.set, digits=1, print=TRUE) {
	availability(mast, v.set, dir.set, digits, print)
}

cln <- function(mast, set, v.avg.min=0.4, v.avg.max=50, dir.clean=TRUE, turb.clean=4, icing=FALSE, rep=NULL, n.rep=5) {
	clean(mast, set, v.avg.min, v.avg.max, dir.clean, turb.clean, icing, rep, n.rep)
}

mast <- function(time.stamp, ..., loc=NULL, desc=NULL) {
	createMast(time.stamp, ..., loc, desc)
}

pc <- function(v, p, cp, ct, rho=1.225, rated.p, desc) {
	createPC(v, p, cp, ct, rho, rated.p, desc)
}
	
set <- function(height, desc, v.avg, v.max, v.min, v.std, dir.avg, dir.std, tmp, ...) {
	createSet(height, desc, v.avg, v.max, v.min, v.std, dir.avg, dir.std, tmp, ...)
}

en <- function(wb, rho=1.225, bins=c(5,10,15,20), digits=0, print=TRUE) {
	energy(wb, rho, bins, digits, print)
}

forts <- function(time.stamp, pattern) {
	formatTS(time.stamp, pattern)
}

freq <- function(mast, v.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE) {
	frequency(mast, v.set, dir.set, num.sectors, bins, digits, print)
}

map <- function(mast, type=c("satellite", "terrain", "hybrid", "roadmap"), zoom, label, ...) {
	plotMap(mast, type, zoom, label, ...)
}

ms <- function(mast, set, signal="v.avg", fun=c("mean", "median", "min", "max", "sd"), digits=3, print=TRUE) {
	monthStats(mast, set, signal, fun, digits, print)
}

plaep <- function(aep, show.total=TRUE, ...) {
	plotAep(aep, show.total, ...)
}

plavail <- function(avail, set, ...) {
	plotAvailability(avail, set, ...)
}

plday <- function(mast, set, signal, ...) {
	plotDay(mast, set, signal, ...)
}

plen <- function(energy, show.total=TRUE, ...) {
	plotEnergy(energy, show.total, ...)
}

plfreq <- function(freq, ...) {
	plotFrequency(freq, ...)
}

plms <- function(stats, set, ...) {
	plotMonthStats(stats, set, ...)
}

plpc <- function(pc, cp=TRUE, ct=TRUE, ...) {
	plotPC(pc, cp, ct, ...)
}

plpol <- function(mast, v.set=1, dir.set=1, ...) {
	plotPolar(mast, v.set, dir.set, ...)
}

plpro <- function(profile, sector, measured=TRUE, ...) {
	plotProfile(profile, sector, measured, ...)
}

plts <- function(mast, set, signal=c("v.avg", "dir.avg", "turb.int"), start, end, ...) {
	plotTimeSeries(mast, set, signal, start, end, ...)
}

pliec <- function(mast, set, ...) {
	plotTurbIEC(mast, set, ...)
}

plturb <- function(turb, ...) {
	plotTurbulence(turb, ...)
}

pluc <- function(uncertainty, type=c("prob", "uncert"), p.values=c(50, 75, 90), ...) {
	plotUncertainty(uncertainty, type, p.values, ...)
}

plwbd <- function(wb, show.ak=FALSE, ...) {
	plotWbDir(wb, show.ak, ...)
}

plwb <- function(wb, show.ak=FALSE, ...) {
	plotWeibull(wb, show.ak, ...)
}

probj <- function(object) {
	printObject(object)
}

pro <- function(mast, v.set, dir.set, num.sectors=12, method=c("hellman", "loglm", "fixed"), alpha=NULL, digits=3, print=TRUE) {
	profile(mast, v.set, dir.set, num.sectors, method, alpha, digits, print)
}

rpc <- function(file, ex=FALSE) {
	readPC(file, ex)
}

turb <- function(mast, turb.set, dir.set, num.sectors=12, bins=c(5,10,15,20), digits=3, print=TRUE) {
	turbulence(mast, turb.set, dir.set, num.sectors, bins, digits, print)
}

uc <- function(aep, uc.values, uc.names, prob=seq(5,95,5), digits=c(0,0), print=TRUE) {
	uncertainty(aep, uc.values, uc.names, prob, digits, print)
}

wb <- function(mast, v.set, dir.set, num.sectors=12, digits=3, print=TRUE) {
	weibull(mast, v.set, dir.set, num.sectors, digits, print)
}