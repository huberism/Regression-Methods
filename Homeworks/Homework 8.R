##Problem 1##

bats=read.csv("bats.txt")

x=bats$logmass
y=bats$logenergy
type=bats$type

reg.out=lm(y~x+type+x:type)

anova(reg.out)

plot(x,y,cex.lab=1.6,pch=1,xlab="Log Body Mass",cex.axis=1.5,ylab="Log Energy Expenditure",cex=1.5,main="Log Body Mass vs. Log Energy Expenditue",cex.main=1.6,mgp=c(2.7,1,0))
points(x[type=="Non-echolocating birds"],y[type=="Non-echolocating birds"],pch=2,cex=1.5)
points(x[type=="Echolocating bats"],y[type=="Echolocating bats"],pch=16,cex=1.5)
legend(0.9,1.5,c("Non-echolocating bats","Non-echolocating birds","Echolocating bats"),pch=c(1,2,16),cex=1.0)



##Problem 2##

sludge=read.csv("sludge.txt",header=T)

z=sludge$plantconc
w=sludge$soilconc
type=sludge$type

reg.out3=lm(z~w+type+w*type+I(w^2)+I(w^2):type)

anova(reg.out3)

plot(w,z,cex.lab=1.6,pch=1,xlab="Soil Mercury Content",cex.axis=1.5,ylab="Plant Mercury Content",cex=1.5,main="Soil Mercury Content vs. Plant Mercury Content",cex.main=1.6,mgp=c(2.7,1,0))
points(w[type=="Wheat"],z[type=="Wheat"],pch=2,cex=1.5)
points(w[type=="Barley"],z[type=="Barley"],pch=16,cex=1.5)
legend(0.9,150,c("Corn","Wheat","Barley"),pch=c(1,2,16),cex=1.0)





