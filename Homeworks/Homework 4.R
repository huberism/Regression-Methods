##Problem 1##

drug=read.csv("drug.txt")

x=drug$dose
y=drug$response

plot(x,y,main="Dose vs. Response",xlab="Dose",ylab="Response")

reg=lm(y~x)
summary(reg)

resid=reg$resid

plot(x,resid,main="Dose Level Residuals",xlab="Dose",ylab="Residuals")
abline(0,0)

purelof <- function(x,y){
  dat <- data.frame(x[order(x)],y[order(x)])
  run1.reg <- lm(y ~ x)
  run2.aov <- aov(run1.reg$resid ~ ordered(x))
  sspe <- sum(run2.aov$resid**2)
  sslof <- sum(run1.reg$resid**2) - sspe
  df.sspe <- run2.aov$df.residual
  df.sslof <- run1.reg$df.residual - df.sspe
  fstat <- (sslof/df.sslof)/(sspe/df.sspe)
  pvalue <- 1-pf(fstat,df.sslof,df.sspe)
  digits <- 6-trunc(log10(sspe))
  cat(" Test for Pure Error/Lack of Fit","\n","-------------------------------","\n",
    "Source of Variance",rep("",digits-2),"df",rep("",digits),"SS",rep("",digits+3),
    "MS",rep("",digits+3),"F",rep("",digits+1),"P-value","\n")
  cat(" Lack of Fit            ",df.sslof,"  ",format(round(sslof,digits)),"  ",format(round(sslof/df.sslof,digits)),
    "  ",format(round(fstat,digits)),"  ",format(round(pvalue,digits)),"\n")
  cat(" Pure Error            ",df.sspe,"  ",format(round(sspe,digits)),"  ",format(round(sspe/df.sspe,digits)),
    "\n")
}

purelof(x,y)

parte=log(x)

plot(parte,y,main="log(Dose) vs. Response",xlab="log(Dose)",ylab="Response")

reg2=lm(y~parte)
summary(reg2)

resid2=reg2$resid

plot(parte,resid2,main="log(Dose) Level Residuals",xlab="log(Dose)",ylab="Residuals")
abline(0,0)

purelof(parte,y)


##Problem 2##

natal=read.csv("natal.txt")

x=natal$bodymass
y=natal$maxdist

plot(x,y,main="Body Weight vs. Dispersal Distance",xlab="Body Weight",ylab="Dispersal Distance")

logdist=log(y)
logbody=log(x)
type=natal$type

plot(logbody,logdist,cex.lab=1.6,pch=1,xlab="Log Body Weight",cex.axis=1.5,ylab="Log Dispersal Distance",cex=1.5,main="Log Dispersal Distance vs. Log Body Weight",cex.main=1.6,mgp=c(2.7,1,0))
points(logbody[type=="Carnivore"],logdist[type=="Carnivore"],pch=2,cex=1.5)
points(logbody[type=="Herbivore"],logdist[type=="Herbivore"],pch=16,cex=1.5)
legend(-5.6,6,c("Omnivore","Carnivore","Herbivore"),pch=c(1,2,16),cex=1.5)

reg.out=lm(logdist~logbody+type)
summary(reg.out)
studres=rstudent(reg.out)

plot(logbody,studres,main="Log Body Weight vs. Studentized Deleted Residuals",xlab="Log Body Weight",ylab="Residuals")
abline(0,0)

