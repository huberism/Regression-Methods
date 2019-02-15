##Problem 1##

library(faraway)
data(seatpos)
attach(seatpos)

reg.out=lm(hipcenter~Age+Weight+HtShoes+Ht+Seated+Arm+Thigh+Leg)
summary(reg.out)

cor(seatpos[,1:8])

vif(reg.out)

install.packages('glmnet')
library(glmnet)

x=data.matrix(seatpos[,1:8])
y=hipcenter

ridge.mod=cv.glmnet(x,y,nfold=length(y),alpha=0)

plot(log10(ridge.mod$lambda),ridge.mod$cvm,xlab="log10(Lambda)",ylab="CV Error")
abline(v=log10(ridge.mod$lambda.min),lty=3)

lambda=ridge.mod$lambda.min
[1] 0.2848036
predict(ridge.mod,s=lambda,type="coefficients")

min(ridge.mod$cvm)