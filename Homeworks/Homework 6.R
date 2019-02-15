##Problem 1##

library(faraway)
data(seatpos)
attach(seatpos)


install.packages('pls')
library(pls)

pcr.cv=pcr(hipcenter~.,data=seatpos,scale=TRUE,validation="LOO")

summary(pcr.cv)

validationplot(pcr.cv,val.type="MSEP")

m=2

pr.out=prcomp(seatpos[,1:8],scale=TRUE)
pcreg=lm(hipcenter~pr.out$x[,1:m])

summary(pcreg)


##Problem 2##

nordic=read.csv("nordic.txt")

attach(nordic)
scores=cbind(SkiJump,CrossCountry)
pr.out=prcomp(scores,scale=TRUE)

pr.out$x
sort(pr.out$x)

pr.out$rotation

pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)

plot(pve,xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type='b')




