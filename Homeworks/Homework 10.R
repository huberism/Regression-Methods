##Problem 1##

biomass=read.csv("biomass.txt",header=T)

library(MPV)

bio=data.frame(biomass[,1:6])

source("pairs.panels.r")
lab=c("Biomass","Salinity","pH","Potassium","Sodium","Zinc")
pairs.panels(bio,labels=lab)

n=dim(bio)[1]

y=biomass$biomass
ph=biomass$pH
potassium=biomass$Potassium
sodium=biomass$Sodium

bioreg=lm(y~ph+potassium+sodium)
summary(bioreg)
PRESS(bioreg)
step(bioreg,k=2)
step(bioreg,k=log(n))

bioreg2=lm(y~ph+potassium)
summary(bioreg2)
PRESS(bioreg2)
step(bioreg2,k=2)
step(bioreg2,k=log(n))

bioreg3=lm(y~ph+sodium)
summary(bioreg3)
PRESS(bioreg3)
step(bioreg3,k=2)
step(bioreg3,k=log(n))

bioreg4=lm(y~potassium+sodium)
summary(bioreg4)
PRESS(bioreg4)
step(bioreg4,k=2)
step(bioreg4,k=log(n))

bioreg5=lm(y~ph)
summary(bioreg5)
PRESS(bioreg5)
step(bioreg5,k=2)
step(bioreg5,k=log(n))

bioreg6=lm(y~potassium)
summary(bioreg6)
PRESS(bioreg6)
step(bioreg6,k=2)
step(bioreg6,k=log(n))

bioreg7=lm(y~sodium)
summary(bioreg7)
PRESS(bioreg7)
step(bioreg7,k=2)
step(bioreg7,k=log(n))



##Problem 2##

protein=read.csv("protein.txt",header=T)

y=protein$protein
copper=protein$copper
zinc=protein$zinc

reg.out=lm(y~zinc+copper+zinc:copper+I(zinc^2)+I(copper^2)+I(zinc^2*copper)+I(zinc*copper^2)+I(zinc^2*copper^2))
summary(reg.out)

reg.out2=lm(y~zinc+copper+zinc:copper+I(zinc^2)+I(copper^2)+I(zinc^2*copper)+I(zinc*copper^2))
summary(reg.out2)

reg.out3=lm(y~zinc+copper+zinc:copper+I(zinc^2)+I(copper^2)+I(zinc^2*copper))
summary(reg.out3)

reg.out4=lm(y~zinc+copper+zinc:copper+I(zinc^2)+I(copper^2))
summary(reg.out4)

reg.out5=lm(y~zinc+copper+zinc:copper+I(copper^2))
summary(reg.out5)

reg.out6=lm(y~zinc+copper+zinc:copper)
summary(reg.out6)
