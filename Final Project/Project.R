##########################################################
# Robert Kluge, Matthew Huber, James Fung, Erin McKevitt #
# Dr. Zhang                                              #
# Regression                                             #
# 29 March 2017                                          #
# Project Proposal                                       #
##########################################################


library(rJava)
library(xlsx)
library(MPV)

# 2016 Summer Olympics

# Reading in the data
Data2016<-as.data.frame(read.xlsx("Summer Olympics 2016.xlsx",1,header=T))
Data2016$Pop2015
Data2016$GDP2015
Data2016$GDPGrowth2015
Data2016$NumMedals
Data2016$NumAthletes
Data2016$NumEvents


# Defining a "keep" variable that is true for observations without missing data
for (i in 1:length(Data2016$Country)) {
  if (is.na(Data2016$Pop2015[i]) | is.na(Data2016$GDP2015[i]) | is.na(Data2016$GDPGrowth2015[i])) Data2016$Keep[i]<-F else {
    Data2016$Keep[i]<-T } }

# Defining a data set with no missing values
Data2016NoNA<-as.data.frame(Data2016[Data2016$Keep,])

# Examining variable distributions
par(mfrow=c(1,1))
hist(Data2016NoNA$NumMedals)
hist(Data2016NoNA$NumAthletes)
hist(Data2016NoNA$NumEvents)
hist(Data2016NoNA$Pop2015)
hist(Data2016NoNA$GDP2015)
hist(Data2016NoNA$GDPGrowth2015)

# Examning log transformation of above variables
hist(log(Data2016NoNA$NumAthletes))
hist(log(Data2016NoNA$NumEvents))
hist(log(Data2016NoNA$Pop2015))
hist(log(Data2016NoNA$GDP2015))
hist(Data2016NoNA$OlympicCount)

# Putting the log variables into the dataframe
Data2016NoNA$logNumMedals<-log(Data2016NoNA$NumMedals+2)
Data2016NoNA$logNumAthletes<-log(Data2016NoNA$NumAthletes)
Data2016NoNA$logNumEvents<-log(Data2016NoNA$NumEvents)
Data2016NoNA$logPop2015<-log(Data2016NoNA$Pop2015)
Data2016NoNA$logGDP2015<-log(Data2016NoNA$GDP2015)
Data2016NoNA$BCNumMedals<-(Data2016NoNA$NumMedals+1)

# Creating a multiple regression model with no interaction
reg2016_1<-lm(Data2016NoNA$NumMedals~Data2016NoNA$NumAthletes+Data2016NoNA$NumEvents+Data2016NoNA$Pop2015+Data2016NoNA$GDP2015
              +Data2016NoNA$ThirdWorld+Data2016NoNA$OlympicCount)
summary(reg2016_1)
par(mfrow=c(2,2))


# Creating a multiple regression model with no interaction & transformations
reg2016_2<-lm(log(Data2016NoNA$NumMedals+1)~log(Data2016NoNA$NumAthletes)+log(Data2016NoNA$NumEvents)+log(Data2016NoNA$Pop2015)+log(Data2016NoNA$GDP2015)+Data2016NoNA$ThirdWorld+log(Data2016NoNA$OlympicCount))

# Creating a multiple regression model with all two factor interactions and doing backward elimination alpha 0.05
reg2016_3<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumAthletes*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumEvents*Data2016NoNA$logPop2015+Data2016NoNA$logNumEvents*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumEvents*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2)+I(Data2016NoNA$logPop2015^2)+I(Data2016NoNA$GDPGrowth2015^2))

# Dropped logpop2015^2
reg2016_4<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumAthletes*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumEvents*Data2016NoNA$logPop2015+Data2016NoNA$logNumEvents*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumEvents*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2)+I(Data2016NoNA$GDPGrowth2015^2))

# Dropped gdpgrowth2015^2
reg2016_5<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumAthletes*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumEvents*Data2016NoNA$logPop2015+Data2016NoNA$logNumEvents*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumEvents*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumathletes * thirdworld
reg2016_6<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015
              +Data2016NoNA$logNumEvents*Data2016NoNA$logPop2015+Data2016NoNA$logNumEvents*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumEvents*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumevents * logpop2015
reg2016_7<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015
              +Data2016NoNA$logNumEvents*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logNumEvents*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumevents * gdpgrowth2015
reg2016_8<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015
              +Data2016NoNA$logNumEvents*Data2016NoNA$ThirdWorld
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumevents * thirdworld
reg2016_9<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
              +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015
              +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$logPop2015*Data2016NoNA$ThirdWorld+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
              +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped logpop2015 * thirdworld
reg2016_10<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015+Data2016NoNA$logNumAthletes*Data2016NoNA$GDPGrowth2015
               +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumathletes * gdpgrowth2015
reg2016_11<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015
               +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015+Data2016NoNA$GDPGrowth2015*Data2016NoNA$ThirdWorld
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped gdpgrowth2015 * thirdworld
reg2016_12<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015+Data2016NoNA$ThirdWorld
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015
               +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped thirdworld
reg2016_13<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents+Data2016NoNA$logNumAthletes*Data2016NoNA$logPop2015
               +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumathletes * logpop2015
reg2016_14<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents
               +Data2016NoNA$logPop2015*Data2016NoNA$GDPGrowth2015
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped logpop2015 * gdpgrowth2015
reg2016_15<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$logPop2015+Data2016NoNA$GDPGrowth2015
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped logpop2015
reg2016_16<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents+Data2016NoNA$GDPGrowth2015
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped gdpgrowth2015
reg2016_17<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents
               +I(Data2016NoNA$logNumAthletes^2)+I(Data2016NoNA$logNumEvents^2))

# Dropped lognumevents^2
reg2016_18<-lm(Data2016NoNA$logNumMedals~Data2016NoNA$logNumAthletes+Data2016NoNA$logNumEvents
               +Data2016NoNA$logNumAthletes*Data2016NoNA$logNumEvents
               +I(Data2016NoNA$logNumAthletes^2))

n=dim(Data2016NoNA)[1]

summary(reg2016_1)
PRESS(reg2016_1)
step(reg2016_1,k=2)
step(reg2016_1,k=log(n))

summary(reg2016_2)
PRESS(reg2016_2)
step(reg2016_2,k=2)
step(reg2016_2,k=log(n))

summary(reg2016_3)
PRESS(reg2016_3)
step(reg2016_3,k=2)
step(reg2016_3,k=log(n))

summary(reg2016_4)
PRESS(reg2016_4)
step(reg2016_4,k=2)
step(reg2016_4,k=log(n))

summary(reg2016_5)
PRESS(reg2016_5)
step(reg2016_5,k=2)
step(reg2016_5,k=log(n))

summary(reg2016_6)
PRESS(reg2016_6)
step(reg2016_6,k=2)
step(reg2016_6,k=log(n))

summary(reg2016_7)
PRESS(reg2016_7)
step(reg2016_7,k=2)
step(reg2016_7,k=log(n))

summary(reg2016_8)
PRESS(reg2016_8)
step(reg2016_8,k=2)
step(reg2016_8,k=log(n))

summary(reg2016_9)
PRESS(reg2016_9)
step(reg2016_9,k=2)
step(reg2016_9,k=log(n))

summary(reg2016_10)
PRESS(reg2016_10)
step(reg2016_10,k=2)
step(reg2016_10,k=log(n))

summary(reg2016_11)
PRESS(reg2016_11)
step(reg2016_11,k=2)
step(reg2016_11,k=log(n))

summary(reg2016_12)
PRESS(reg2016_12)
step(reg2016_12,k=2)
step(reg2016_12,k=log(n))

summary(reg2016_13)
PRESS(reg2016_13)
step(reg2016_13,k=2)
step(reg2016_13,k=log(n))

summary(reg2016_14)
PRESS(reg2016_14)
step(reg2016_14,k=2)
step(reg2016_14,k=log(n))

summary(reg2016_15)
PRESS(reg2016_15)
step(reg2016_15,k=2)
step(reg2016_15,k=log(n))

summary(reg2016_16)
PRESS(reg2016_16)
step(reg2016_16,k=2)
step(reg2016_16,k=log(n))

summary(reg2016_17)
PRESS(reg2016_17)
step(reg2016_17,k=2)
step(reg2016_17,k=log(n))

summary(reg2016_18)
PRESS(reg2016_18)
step(reg2016_18,k=2)
step(reg2016_18,k=log(n))
