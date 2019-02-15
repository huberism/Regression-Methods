##Problem 1##

maxspeed=read.csv("maxspeed.txt")

boxplot(speed~sex,data=maxspeed,cex.axis=1.5,ylab="Maximum Speed Driven (mph)", names=c("Female","Male"),main="Boxplot of Maximum Speed by Sex",cex.main=1.8,cex.lab=1.6,mgp=c(1.5,1,0))

logitmod=glm(sex~speed,data=maxspeed,family=binomial)
summary(logitmod)

ptest=as.numeric(logitmod$fitted>0.5)
table(maxspeed$sex,ptest)

##Problem 2##

shuttle=read.csv("shuttle.txt")

logitmod2=glm(failure~temp,data=shuttle,family=binomial)
summary(logitmod2)

1-pchisq(5.945,1)

library(faraway)

x0=c(1,31)
eta0=sum(x0*coef(logitmod2))
ilogit(eta0)

varmat=summary(logitmod2)$cov.unscaled
se=sqrt(t(x0)%*%varmat%*%x0)
ilogit(c(eta0-1.96*se,eta0+1.96*se))
