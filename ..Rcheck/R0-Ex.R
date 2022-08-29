pkgname <- "R0"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('R0')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("GT.chld.hsld")
### * GT.chld.hsld

flush(stderr()); flush(stdout())

### Name: GT.chld.hsld
### Title: 2009 A/H1N1 observed Generation Time distribution
### Aliases: GT.chld.hsld
### Keywords: datasets

### ** Examples

data(GT.chld.hsld)
## maybe str(GT.chld.hsld) ; plot(GT.chld.hsld) ...



cleanEx()
nameEx("Germany.1918")
### * Germany.1918

flush(stderr()); flush(stdout())

### Name: Germany.1918
### Title: Germany.1918 exemple dataset
### Aliases: Germany.1918
### Keywords: datasets

### ** Examples

data(Germany.1918)
## maybe str(Germany.1918) ; plot(Germany.1918) ...



cleanEx()
nameEx("H1N1.serial.interval")
### * H1N1.serial.interval

flush(stderr()); flush(stdout())

### Name: H1N1.serial.interval
### Title: H1N1 serial interval sample
### Aliases: H1N1.serial.interval
### Keywords: datasets

### ** Examples

data(H1N1.serial.interval)
## maybe str(H1N1.serial.interval) ; plot(H1N1.serial.interval) ...



cleanEx()
nameEx("check.incid")
### * check.incid

flush(stderr()); flush(stdout())

### Name: check.incid
### Title: Check incid in the input
### Aliases: check.incid

### ** Examples
#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany
data(Germany.1918)
Germany.1918

## check.incid will extract names from the vector and coerce them as dates
check.incid(Germany.1918)

## Had Germany.1918 not have names() set, output would have been with index dates
## To force such an output, we here impose t=1:126. 
## Erasing names(Germany.1918) would have produced the same
## If so, then the epid$t vector returned will be replacement values.
check.incid(Germany.1918, t=1:126)

## You can also choose not to provide a complete date vector, but to only
## indicated the first day of the observation, and the number of days between each
## observation. In this example we will assume a time step of 7 days.
check.incid(Germany.1918, date.first.obs="1918-01-01", time.step=7)

## Finally, if no names() are available for the dataset and date.first.obs is not provided,
## setting time.step to any integer value will generate a t vector starting 
## from 1 and incrementing by the time.step parameter.


cleanEx()
nameEx("est.GT")
### * est.GT

flush(stderr()); flush(stdout())

### Name: est.GT
### Title: Find the best-fitting GT distribution for a series of serial
###   interval
### Aliases: est.GT

### ** Examples
#Loading package
library(R0)

# Data taken from traced cases of H1N1 viruses.
data(H1N1.serial.interval)
est.GT(serial.interval=H1N1.serial.interval)

## Best fitting GT distribution is a gamma distribution with mean = 3.039437 and sd = 1.676551 .
## Discretized Generation Time distribution
## mean: 3.070303 , sd: 1.676531 
## [1] 0.0000000000 0.1621208802 0.2704857362 0.2358751176 0.1561845680 0.0888997193 0.0459909903 
## 0.0222778094 0.0102848887 0.0045773285 0.0019791984 0.0008360608 0.0003464431 0.0001412594


# The same result can be achieved with two vectors of dates of onset.
# Here we use the same data, but trick the function into thinking onset dates are all "0".
data(H1N1.serial.interval)
est.GT(infector.onset.dates=rep(0,length(H1N1.serial.interval)), 
       infectee.onset.dates=H1N1.serial.interval)


cleanEx()
nameEx("est.R0.AR")
### * est.R0.AR

flush(stderr()); flush(stdout())

### Name: est.R0.AR
### Title: Estimate R0 from attack rate of an epidemic
### Aliases: est.R0.AR

### ** Examples
#Loading package
library(R0)

## Woodall reported an attack rate of 0.31 in a population of 1732 during
## the 1957 H2N2 influenza pandemic ('Age and Asian Influenza, 1957', BMJ, 1958)

est.R0.AR(pop.size=1732, AR=0.31)
# Reproduction number estimate using Attack Rate method
# R :  1.19698[ 1.179606 , 1.215077 ]

est.R0.AR(AR=0.31)
# Reproduction number estimate using  Attack Rate  method.
# R :  1.19698

est.R0.AR(pop.size=1732, incid=31)
# Reproduction number estimate using Attack Rate method
# R :  1.009057[ 1.005873 , 1.012269 ]

est.R0.AR(pop.size=1732, incid=c(2,3,4,7,4,2,4,5))
# Reproduction number estimate using Attack Rate method
# R :  1.009057[ 1.005873 , 1.012269 ]

est.R0.AR(pop.size=1732, incid=c(2,3,0,7,4,2,0,5))
# Reproduction number estimate using Attack Rate method
# R :  1.006699[ 1.003965 , 1.009453 ]


cleanEx()
nameEx("est.R0.EG")
### * est.R0.EG

flush(stderr()); flush(stdout())

### Name: est.R0.EG
### Title: Estimate R from exponential growth rate
### Aliases: est.R0.EG

### ** Examples
#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)

data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))

est.R0.EG(Germany.1918, mGT, begin=1, end=27)
## Reproduction number estimate using  Exponential Growth 
## R :  1.525895[ 1.494984 , 1.557779 ]


cleanEx()
nameEx("est.R0.ML")
### * est.R0.ML

flush(stderr()); flush(stdout())

### Name: est.R0.ML
### Title: Estimate the reproduction number by maximum likelihood
### Aliases: est.R0.ML

### ** Examples
#Loading package
library(R0)

## Data is taken from paper by Nishiura for key transmission parameters of an institutional
## outbreak during the 1918 influenza pandemic in Germany)

data(Germany.1918)
mGT<-generation.time("gamma", c(2.45, 1.38))
est.R0.ML(Germany.1918, mGT, begin=1, end=27, range=c(0.01,50))
# Reproduction number estimate using  Maximum Likelihood  method.
# R :  1.307222[ 1.236913 , 1.380156 ]

res=est.R0.ML(Germany.1918, mGT, begin=1, end=27, range=c(0.01,50))
plot(res)

## no change in R with varying range 
## (dates here are the same index as before. Just to illustrate different use)
est.R0.ML(Germany.1918, mGT, begin="1918-09-29", end="1918-10-25", range=c(0.01,100))
# Reproduction number estimate using  Maximum Likelihood  method.
# R :  1.307249[ 1.236913 , 1.380185 ]




cleanEx()
nameEx("est.R0.SB")
### * est.R0.SB

flush(stderr()); flush(stdout())

### Name: est.R0.SB
### Title: Estimate the time dependent reproduction number using a Bayesian
###   approach
### Aliases: est.R0.SB

### ** Examples
#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)

data(Germany.1918)
mGT <- generation.time("gamma", c(3,1.5))
SB <- est.R0.SB(Germany.1918, mGT)

## Results will include "most likely R(t)" (ie. the R(t) value for which the computed probability 
## is the highest), along with 95% CI, in a data.frame object
SB
# Reproduction number estimate using  Real Time Bayesian  method.
# 0 0 2.02 0.71 1.17 1.7 1.36 1.53 1.28 1.43 ...

SB$Rt.quant
# Date R.t. CI.lower. CI.upper.
# 1  1918-09-29 0.00      0.01      1.44
# 2  1918-09-30 0.00      0.01      1.42
# 3  1918-10-01 2.02      0.97      2.88
# 4  1918-10-02 0.71      0.07      1.51
# 5  1918-10-03 1.17      0.40      1.84
# 6  1918-10-04 1.70      1.09      2.24
# 7  1918-10-05 1.36      0.84      1.83
# 8  1918-10-06 1.53      1.08      1.94
# 9  1918-10-07 1.28      0.88      1.66
# 10 1918-10-08 1.43      1.08      1.77
# ...

## "Plot" will provide the most-likely R value at each time unit, along with 95CI
plot(SB)
## "Plotfit" will show the complete distribution of R for 9 time unit throughout the outbreak
plotfit(SB)


cleanEx()
nameEx("est.R0.TD")
### * est.R0.TD

flush(stderr()); flush(stdout())

### Name: est.R0.TD
### Title: Estimate the time dependent reproduction number
### Aliases: est.R0.TD

### ** Examples
#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)

data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))
TD <- est.R0.TD(Germany.1918, mGT, begin=1, end=126, nsim=100)
# Warning messages:
# 1: In est.R0.TD(Germany.1918, mGT) : Simulations may take several minutes.
# 2: In est.R0.TD(Germany.1918, mGT) : Using initial incidence as initial number of cases.
TD
# Reproduction number estimate using  Time-Dependent  method.
# 2.322239 2.272013 1.998474 1.843703 2.019297 1.867488 1.644993 1.553265 1.553317 1.601317 ...

## An interesting way to look at these results is to agregate initial data by longest time unit,
## such as weekly incidence. This gives a global overview of the epidemic.
TD.weekly <- smooth.Rt(TD, 7)
TD.weekly
# Reproduction number estimate using  Time-Dependant  method.
# 1.878424 1.580976 1.356918 1.131633 0.9615463 0.8118902 0.8045254 0.8395747 0.8542518 0.8258094..
plot(TD.weekly)


cleanEx()
nameEx("estimate.R")
### * estimate.R

flush(stderr()); flush(stdout())

### Name: estimate.R
### Title: Estimate R0 for one incidence dataset using several methods
### Aliases: estimate.R

### ** Examples
#Loading package
library(R0)

## Outbreak during 1918 influenza pandemic in Germany)
data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))
estR0<-estimate.R(Germany.1918, mGT, begin=1, end=27, methods=c("EG", "ML", "TD", "AR", "SB"), 
                  pop.size=100000, nsim=100)

attributes(estR0)
## $names
## [1] "epid"      "GT"        "begin"     "end"       "estimates"
## 
## $class
## [1] "R0.sR"

## Estimates results are stored in the $estimates object
estR0
## Reproduction number estimate using  Exponential Growth  method.
## R :  1.525895[ 1.494984 , 1.557779 ]
## 
## Reproduction number estimate using  Maximum Likelihood  method.
## R :  1.383996[ 1.309545 , 1.461203 ]
## 
## Reproduction number estimate using  Attack Rate  method.
## R :  1.047392[ 1.046394 , 1.048393 ]
## 
## Reproduction number estimate using  Time-Dependent  method.
## 2.322239 2.272013 1.998474 1.843703 2.019297 1.867488 1.644993 1.553265 1.553317 1.601317 ...
## 
## Reproduction number estimate using  Sequential Bayesian  method.
## 0 0 2.22 0.66 1.2 1.84 1.43 1.63 1.34 1.52 ...


## If no date vector nor date of first observation are provided, results are the same
## except time values in $t are replaced by index


cleanEx()
nameEx("generation.time")
### * generation.time

flush(stderr()); flush(stdout())

### Name: generation.time
### Title: Generation Time distribution
### Aliases: generation.time

### ** Examples
#Loading package
library(R0)

# GT for children at house(from Cauchemez PNAS 2011)

GT.chld.hsld1<-generation.time("empirical", c(0,0.25,0.2,0.15,0.1,0.09,0.05,0.01))
plot(GT.chld.hsld1, col="green")
GT.chld.hsld1
# Discretized Generation Time distribution
# mean: 2.729412 , sd: 1.611636 
# [1] 0.00000000 0.29411765 0.23529412 0.17647059 0.11764706 0.10588235 0.05882353
# [8] 0.01176471

GT.chld.hsld2<-generation.time("gamma", c(2.45, 1.38))
GT.chld.hsld2
# Discretized Generation Time distribution
# mean: 2.504038 , sd: 1.372760
# [1] 0.0000000000 0.2553188589 0.3247178420 0.2199060781 0.1144367560
# [6] 0.0515687896 0.0212246257 0.0082077973 0.0030329325 0.0010825594
#[11] 0.0003760069 0.0001277537


# GT for school & community
GTs1<-generation.time("empirical", c(0,0.95,0.05))
plot(GTs1, col='blue')


plot(GT.chld.hsld1, ylim=c(0,0.5), col="red")
par(new=TRUE)
plot(GT.chld.hsld2, xlim=c(0,7), ylim=c(0,0.5), col="black")


graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("sensitivity.analysis")
### * sensitivity.analysis

flush(stderr()); flush(stdout())

### Name: sensitivity.analysis
### Title: Sensitivity analysis of basic reproduction ratio to begin/end
###   dates
### Aliases: sensitivity.analysis

### ** Examples
#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)
data(Germany.1918)

## For this exemple, we use the exact same call as for the internal sensitivity analysis function

## sa.type = "GT"

## Here we will test GT with means of 1 to 5, each time with SD constant (1)
## GT and SD can be either fixed value or vectors of values
## Actual value in simulations may differ, as they are adapted according to the distribution type
tmp<-sensitivity.analysis(sa.type="GT", incid=Germany.1918, GT.type="gamma", GT.mean=seq(1,5,1), 
                          GT.sd.range=1, begin=1, end=27, est.method="EG")

## Results are stored in a matrix, each line dedicated to a (mean,sd) couple
plot(x=tmp[,"GT.Mean"], xlab="mean GT (days)", y=tmp[,"R"], ylim=c(1.2, 2.1), ylab="R0 (95% CI)", 
     type="p", pch=19, col="black", main="Sensitivity of R0 to mean GT")
arrows(x0=as.numeric(tmp[,"GT.Mean"]), y0=as.numeric(tmp[,"CI.lower"]), 
       y1=as.numeric(tmp[,"CI.upper"]), angle=90, code=3, col="black", length=0.05)
## One could tweak this example to change sorting of values (per mean, or per standard deviation)
## eg: 'x=tmp[,c('GT.Mean')]' could become 'x=tmp[,c('GT.SD')]'


## sa.type="time"

mGT<-generation.time("gamma", c(2.6,1))
sen=sensitivity.analysis(sa.type="time", incid=Germany.1918, GT=mGT, begin=1:15, end=16:30, 
                         est.method="EG")
# ...
# Warning message:
# If 'begin' and 'end' overlap, cases where begin >= end are skipped.
# These cases often return Rsquared = 1 and are thus ignored.
## A list with different estimates of reproduction ratio, exponential growth rate and 95%CI 
## wtih different pairs of begin and end dates in form of data frame is returned.
## If method is "EG", results will include growth rate and deviance R-squared measure
## Else, if "ML" method is used, growth rate and R-squared will be set as NA

## Interesting results include the variation of R0 given specific begin/end dates.
## Such results can be plot as a colored matrix and display Rsquared=f(time period)
plot(sen, what=c("criterion","heatmap"))
## Returns complete data.frame of best R0 value for each time period 
## (allows for quick visualization)
## The "best.fit" is the time period over which the estimate is the more robust

# $best.fit
#    Time.period Begin.dates  End.dates       R Growth.rate  Rsquared CI.lower. CI.upper.
# 92          15  1970-01-08 1970-01-23 1.64098   0.1478316 0.9752564  1.574953  1.710209


cleanEx()
nameEx("sim.epid")
### * sim.epid

flush(stderr()); flush(stdout())

### Name: sim.epid
### Title: Epidemic outbreak simulation
### Aliases: sim.epid

### ** Examples
#Loading package
library(R0)

## In this example we simulate n=100 epidemic curves, with peak value at 150 incident cases, 
## and maximum epidemic length of 30 time units.
## Only the outbreak phase is computed. When the peak value is reached, the process is stopped 
## and another epidemic is generated.
sim.epid(epid.nb=100, GT=generation.time("gamma",c(3,1.5)), R0=1.5, 
         epid.length=30, family="poisson", peak.value=150)

# Here, a 30*100 matrix is returned. Each column is a single epidemic.


cleanEx()
nameEx("smooth.Rt")
### * smooth.Rt

flush(stderr()); flush(stdout())

### Name: smooth.Rt
### Title: Smooth real-time reproduction number over larger time period
### Aliases: smooth.Rt

### ** Examples
#Loading package
library(R0)

## This script allows for generating a new estimation for RTB and TD methods.
## Estimations used as input are agregated by a time period provided by user.
## Results can be plotted exactly the same was as input estimations,
## except they won't show any goodness of fit curve.
data(Germany.1918)
mGT <- generation.time("gamma", c(3,1.5))
TD <- estimate.R(Germany.1918, mGT, begin=1, end=126, methods="TD", nsim=100)
TD
# Reproduction number estimate using  Time-Dependant  method.
# 2.322239 2.272013 1.998474 1.843703 2.019297 1.867488 1.644993 1.553265 1.553317 1.601317 ...
TD$estimates$TD$Rt.quant
#     Date      R.t. CI.lower.  CI.upper.
# 1      1 2.3222391 1.2000000  2.4000000
# 2      2 2.2720131 2.7500000  6.2500000
# 3      3 1.9984738 2.7500000  6.5000000
# 4      4 1.8437031 0.7368421  1.5789474
# 5      5 2.0192967 3.1666667  6.1666667
# 6      6 1.8674878 1.6923077  3.2307692
# 7      7 1.6449928 0.8928571  1.6428571
# 8      8 1.5532654 1.3043478  2.2608696
# 9      9 1.5533172 1.0571429  1.7428571
# 10    10 1.6013169 1.6666667  2.6666667
# ...

TD.weekly <- smooth.Rt(TD$estimates$TD, 7)
TD.weekly
# Reproduction number estimate using  Time-Dependant  method.
# 1.878424 1.580976 1.356918 1.131633 0.9615463 0.8118902 0.8045254 0.8395747 0.8542518 0.8258094..

TD.weekly$Rt.quant
#    Date      R.t. CI.lower. CI.upper.
# 1     1 1.8784240 1.3571429 2.7380952
# 2     8 1.5809756 1.3311037 2.0100334
# 3    15 1.3569175 1.1700628 1.5308219
# 4    22 1.1316335 0.9961229 1.2445302
# 5    29 0.9615463 0.8365561 1.0453074
# 6    36 0.8118902 0.7132668 0.9365193
# 7    43 0.8045254 0.6596685 0.9325967
# 8    50 0.8395747 0.6776557 1.0402930
# 9    57 0.8542518 0.6490251 1.1086351
# 10   64 0.8258094 0.5836735 1.1142857
# 11   71 0.8543877 0.5224719 1.1460674
# 12   78 0.9776385 0.6228070 1.4912281
# 13   85 0.9517133 0.5304348 1.3652174
# 14   92 0.9272833 0.5045045 1.3423423
# 15   99 0.9635479 0.4875000 1.5125000
# 16  106 0.9508951 0.5000000 1.6670455
# 17  113 0.9827432 0.5281989 1.8122157
# 18  120 0.5843895 0.1103040 0.9490928


### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
