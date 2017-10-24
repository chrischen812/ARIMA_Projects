#Data from
#https://www.r-bloggers.com/chicken-or-the-egg-granger-causality-for-the-masses/


#Model 1 is the unrestricted model that includes the Granger-causal terms.
#Model 2 is the restricted model where the Granger-causal terms are omitted.
#The test is a Wald test that assesses whether using the restricted Model 2 in place of Model 1 makes statistical sense.

#You interpret the results as follows:
  
#if Pr(>F)<α (where α is your desired level of significance), you reject the null hypothesis of no Granger causality. This indicates that Model 2 is too restrictive as compared with Model 1.
#If the inequality is reversed, you do not reject the null hypothesis as the richer Model 1 is preferred to the restricted Model 2.

#Note: A more precise statement would be we are checking to see if including xx is useful for predicting yy when yy's own history is already being used for prediction. That is, do not miss the fact the xx has to be useful beyond (or extra to) the own history of yy.
setwd("C:/Users/Christopher/Documents/Projects/GitHub R/ARIMA_Projects/")
#rm(list=ls())

library(forecast)
library(lmtest)

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


head(cof1)

attach(cof1)
# plot the time series
par(mfrow=c(2,1))
plot.ts(Arabica)
plot.ts(Robusta)

# test for unit root and number of differences required, you can also test for seasonality with nsdiffs

ndiffs(Arabica, alpha=0.05, test= c("kpss"))
ndiffs(Robusta, alpha=0.05, test= c("kpss"))

#differenced time series
dArabica <- diff(Arabica) #order = 1 by default
dRobusta <- diff(Robusta)
plot.ts(dArabica)
plot.ts(dRobusta)

#do Arabica granger cause Robusta?
grangertest(dRobusta ~ dArabica, order=4)


#do Robusta granger cause Arabica?
grangertest(dArabica ~ dRobusta, order=4)


# It seems that Arabica Granger-causes Robusta prices, but not the other way around.
