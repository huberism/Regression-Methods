##Problem 1##

horsepower=read.csv("horsepower.txt")

y=horsepower$price
x=horsepower$horsepower

plot(x,y,main="Graph 1(a)",xlab="Horsepower",ylab="Price")

out=lm(y~x)
resid=out$resid

abline(out)

xval=data.frame(x=280)
conf=predict(out,xval,interval="confidence")
pred=predict(out,xval,interval="prediction")

conf
pred

plot(x,resid,main="Graph 1(d)",xlab="Horsepower",ylab="Residuals")
abline(0,0)

##Problem 2##

bigbang=read.csv("bigbang.txt")

distance=bigbang$distance
velocity=bigbang$velocity

plot(velocity,distance,main="Graph 2(a)",xlab="Recession Velocity",ylab="Distance")

reg.out=lm(distance~velocity)
summary(reg.out)
abline(reg.out)

reg2.out=lm(distance~velocity-1)
summary(reg2.out)

confint(reg2.out,level=.95)

