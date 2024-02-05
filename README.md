<img src="bReeze_logo.png" alt="bReeze" />

An R package collecting functions for wind resource assessment

======

![downloads](http://cranlogs.r-pkg.org/badges/grand-total/bReeze)

bReeze is a collection of widely used methods to analyse, visualise and interpret wind data. Wind resource analyses can subsequently be combined with characteristics of wind turbines to estimate the potential energy production.

bReeze is developed by Christian Graul, considerably based on the work of Carsten Poppinga.

Official release on CRAN: http://cran.r-project.org/package=bReeze


#### Install from GitHub
```
devtools::install_github("chgrl/bReeze")
```

#### Example
```
# load example data
data(winddata)

# create two datasets
set40 <- set(height=40, v.avg=winddata[,2], v.std=winddata[,5], dir.avg=winddata[,14])
set30 <- set(height=30, v.avg=winddata[,6], v.std=winddata[,9], dir.avg=winddata[,16])

# format time stamp
ts <- timestamp(timestamp=winddata[,1])

# create met mast object
metmast <- mast(timestamp=ts, set40=set40, set30=set30)

# plot time series of met mast signals
plot(metmast)

# calculate frequency and mean wind speed per wind direction sector
freq <- frequency(mast=metmast, v.set=1)

# plot frequency
plot(freq)

# calculate availability of pairs of wind speed and direction
availability(mast=metmast)

# calculate monthly means of wind speed
month.stats(mast=metmast)

# calculate turbulence intensity
turbulence(mast=metmast, turb.set=1)

# calculate weibull parameters
wb <- weibull(mast=metmast, v.set=1)

# calculate total wind energy content
energy(wb=wb)

# calculate wind profile
pf <- windprofile(mast=metmast, v.set=c(1,2), dir.set=1)

# import power curve
pc <- pc("Enercon_E126_7.5MW.pow")

# calculate annual energy production
aep <- aep(profile=pf, pc=pc, hub.h=135)

# plot AEP
plot(aep)
```
