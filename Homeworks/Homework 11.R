# ===================== #
# R Code for Homework 11 #
# ===================== #

# ==================== #
# Problem 1 #
# ==================== #
warm <- read.csv("warming2014.txt",     # Reads in the warming data
                 header=T)

# Part (a)
# ========
plot(warm$year,warm$temp,xlab="Year",ylab=    # Plot of temperature vs.
       "Temperature Difference",pch=16,cex=1.5,    #   year
     main="Scatterplot of Temperature vs. Year",
     cex.main=1.6,cex.axis=1.5,cex.lab=1.6,
     mgp=c(2.7,1,0))
reg1 <- lm(temp~year,data=warm)               # Regression of temp on year
summary(reg1)                                 # Regression summary

# Part (b)
# ========
qqnorm(reg1$resid,xlab="Normal Quantiles",    # Normal quantile plot of
       ylab="Residuals",cex.lab=1.6,cex.axis=1.5,  #   model residuals
       cex=1.5,pch=16,cex.main=1.8,
       main="Normal Quantile Plot",mgp=c(2.7,1,0))

library(nortest)                              # Loads "nortest" library
sf.test(reg1$resid)                           # Shapiro-Francia test

# Part (c)
# ========
plot(warm$year,reg1$resid,xlab="Year",ylab= # Partial residual plot of
       "Residuals",pch=16,cex=1.5,cex.main=1.6,  #   residuals vs. year
     main="Scatterplot of Residuals vs. Year",
     cex.axis=1.5,cex.lab=1.6,mgp=c(2.7,1,0))
abline(h=0,lwd=2,lty=2)   # Plots horizontal 0-line
library(lmtest)                             # Loads "lmtest" library
dwtest(reg1,alternative="greater")          # 1-sided Durbin-Watson test


# ==================== #
#  Problem 2 #
# ==================== #
dat <- read.csv("mortality.txt",header=T)  # Reads in mortality data

# Part (a)
# ========
source("Scripts/pairs.panels.r")      # Loads the "pairs.panels" function
pairs.panels(dat[,2:11])              # Panel plot of all variables

# Part (b)
# ========
lognox <- log(dat$nox)                # Log NOx variable
logso2 <- log(dat$so2)                # Log SO2 variable
dat2 <- data.frame(dat,lognox,logso2) # New data frame with logged variables
pairs.panels(dat2[,c(2:9,12,13)])     # Panel plot of all variables

# Part (c)
# ========
reg1 <- lm(mortality~precip+jantemp+    # Regression of mortality on all 1st-
             house+educ+density+nonwhite+poor+     #   order terms and a quadratic temp
             lognox+logso2+I(jantemp^2),data=dat2) #   term
summary(reg1)                           # Regression summary
reg2 <- update(reg1,.~.-poor)           # Refits model without "poor" term
summary(reg2)                           # Regression summary
reg3 <- update(reg2,.~.-I(jantemp^2))   # Refits without "Jantemp^2" term
summary(reg3)                           # Regression summary
reg4 <- update(reg3,.~.-density)        # Refits without "density" term
summary(reg4)                           # Regression summary
reg5 <- update(reg4,.~.-logso2)         # Refits without "logso2" term
summary(reg5)                           # Regression summary
reg6 <- update(reg5,.~.-house)          # Refits without "house" term
summary(reg6)                           # Regression summary

# Part (d)
# ========
par(mfrow=c(2,2))         # 2x2 graphics window
qqnorm(reg6$resid,xlab="Normal Quantiles",ylab="Residuals",
       cex.axis=1.5,cex.lab=1.6,main="Normal Quantile Plot",
       cex.main=1.6,pch=16,cex=1.5,mgp=c(2.7,1,0))
plot(reg6$fitted,reg6$resid,xlab="Predicted Values",ylab="Residuals",
     cex.axis=1.5,cex.lab=1.6,main="Residual Plot",
     cex.main=1.6,pch=16,cex=1.5,mgp=c(2.7,1,0))
plot(dat2$jantemp,reg6$resid,xlab="Average January Temperature",
     ylab="Residuals",cex.axis=1.5,cex.lab=1.6,cex.main=1.6,main=
       "Partial Residual Plot",pch=16,cex=1.5,mgp=c(2.7,1,0))
plot(dat2$density,reg6$resid,xlab="Density",
     ylab="Residuals",cex.axis=1.5,cex.lab=1.6,cex.main=1.6,main=
       "Partial Residual Plot",pch=16,cex=1.5,mgp=c(2.7,1,0))

# Part (e)
# ========
round(cooks.distance(reg6),4)    # Cook's D values
round(dffits(reg6),4)            # DfFits values
round(dfbetas(reg6),4)           # DfBetas values