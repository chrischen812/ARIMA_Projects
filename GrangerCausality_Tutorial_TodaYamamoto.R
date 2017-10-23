#Data From
#https://datazen.info/toda-yamamoto-implementation-in-r/


library(tseries)
library(urca)
library(vars)
library(aod)
library(zoo)
library(tseries)

setwd("C:/Users/Christopher/Documents/Projects/GitHub R/ARIMA_Projects/")


#Load data
cof = read.csv("Coffee_Data.csv", header=T, sep=",")
names(cof)

#Adjust Date format
cof["Date"] =paste(sub("M","-",cof$Date),"-01",sep="")

#Visualize
plot(as.Date(cof$Date),cof$Arabica,type="l",col="black",lwd=2)
lines(as.Date(cof$Date),cof$Robusta,col="blue",lty=2,lwd=1)
legend("topleft",c("Arabica","Robusta"),col=c("black","blue"),lty=c(1,2),lwd=c(2,1),bty="n")

#Possible structural break in 1970s. Therefore only values from 1976:01 onwards are regarded
cof1 = cof[193:615,]

#Visualize
plot(as.Date(cof1$Date),cof1$Arabica,type="l",col="black",lwd=2,ylim=range(cof1$Robusta))
lines(as.Date(cof1$Date),cof1$Robusta,col="blue",lty=2,lwd=1)
legend("topright",c("Arabica","Robusta"),col=c("black","blue"),lty=c(1,2),lwd=c(2,1),bty="n")

#Test for unit roots
adf.test(cof$Arabica)
adf.test(cof$Robusta)
kpss.test(cof$Arabica)
kpss.test(cof$Arabica)

adf.test(diff(cof$Arabica,1))
adf.test(diff(cof$Robusta,1))
kpss.test(diff(cof$Arabica,1))
kpss.test(diff(cof$Robusta,1))

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).

#Set up VAR-Model
#select lag order // either 2 or 6
VARselect(cof1[,2:3],lag=20,type="both")

#VAR Model, lag=2
V.2 = VAR(cof1[,2:3],p=2,type="both")
serial.test(V.2)

#VAR-Model, lag=6
V.6 = VAR(cof1[,2:3],p=6,type="both")
serial.test(V.6)

#Stability analysis
1/roots(V.6)[[1]] # "&gt;1"
1/roots(V.6)[[2]] # "&gt;1"

#Alternative stability analyis
plot(stability(V.6)) ## looks fine

# Model with p=6 is less likely to be serially correlated. Thus model with p=6 is selected.

# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
V.7 = VAR(cof1[,2:3],p=7,type="both")
V.7$varresult
summary(V.7)

#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(V.7$varresult[[1]]), Sigma=vcov(V.7$varresult[[1]]), Terms=c(2,4,6,8,10,12))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(V.7$varresult[[2]]), Sigma=vcov(V.7$varresult[[2]]), Terms= c(1,3,5,7,9,11))
# Could be rejected at 10% (X2=12.3; p=0.056)

# It seems that Arabica Granger-causes Robusta prices, but not the other way around.
