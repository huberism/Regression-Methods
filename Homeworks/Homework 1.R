##Problem 1##

gesell=read.csv("gesell.txt")	#Reads in gesell data

y=gesell$score			#Sets Gesell scores as the response variable
x=gesell$age			#Sets age as the expanatory variable

plot(x,y,xlab="Age",ylab="Gesell Scores")	#Plots scatterplot data

reg=lm(y~x)				#LS fit
summary(reg)			#Regression summary
abline(reg$coef,lwd=2)		#Plots regression line

reg.resid=resid(reg)		#Plot the residuals
plot(x,reg.resid,xlab="Age",ylab="Residuals")
abline(0,0)

##Removing Child 19##

gesell2=read.csv("gesell2.txt")	#Reads in gesell data

y2=gesell2$score			#Sets Gesell scores as the response variable
x2=gesell2$age			#Sets age as the expanatory variable

plot(x2,y2,xlab="Age",ylab="Gesell Scores")	#Plots scatterplot data

reg2=lm(y2~x2)			#LS fit
summary(reg2)			#Regression summary
abline(reg2$coef,lwd=2)		#Plots regression line

##Removing Child 18##

gesell3=read.csv("gesell3.txt")	#Reads in gesell data

y3=gesell3$score			#Sets Gesell scores as the response variable
x3=gesell3$age			#Sets age as the expanatory variable

plot(x3,y3,xlab="Age",ylab="Gesell Scores")	#Plots scatterplot data

reg3=lm(y3~x3)			#LS fit
summary(reg3)			#Regression summary
abline(reg3$coef,lwd=2)		#Plots regression line


plot(x,y,xlab="Age",ylab="Gesell Scores")	#Plots scatterplot data
reg3=lm(y3~x3)			#LS fit
summary(reg3)			#Regression summary
abline(reg3$coef,lwd=2)		#Plots regression line


##Problem 2##

recovery=read.csv("recovery.txt",header=T)

percentage=recovery$recovery
time=recovery$time

plot(time,percentage,xlab="Time",ylab="Percentage")

lnperc=log(percentage)

plot(time,lnperc,xlab="Time",ylab="Natural Log Percent")

recovreg=lm(lnperc~time)
summary(recovreg)
abline(recovreg$coef,lwd=2)

anova(recovreg)

percreg=lm(percentage~time)
summary(percreg)


