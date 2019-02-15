##Problem 1##

city=read.csv("citysize.txt")

y=city$expenditure
x=city$size

plot(x,y,main="Graph 1(a)",xlab="City Size",ylab="Expenditure")

cor(x,y)

out=lm(y~x)
resid=out$resid

summary(out)

plot(x,resid,main="Graph 1(d)",xlab="City Size",ylab="Residuals")
abline(0,0)

recovery=read.csv("recovery.txt")

percentage=recovery$recovery
time=recovery$time
lnperc=log(percentage)

recovreg=lm(lnperc~time)

recovreg

anova(recovreg)

confint(recovreg,level=.99)


