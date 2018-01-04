library(shiny)
library(plotly)
library(leaflet)
library(lubridate)

ternsCOTE<-read.csv("FieldWork_COTE.csv")
ternsROST<-read.csv("FieldWork_ROST.csv")
BoatLocation<-read.csv("BoatLocation.csv")
BoatWaypoints<-read.csv("boatwaypoints.csv")
BoatPath<-read.csv("boatpath.csv")
GPSway<-read.csv("GPSway.csv")


ternsCOTE$tagDate<-as.POSIXct(x=ternsCOTE$tagDate, format="%m/%d/%Y")
ternsROST$tagDate<-as.POSIXct(x=ternsROST$tagDate, format="%m/%d/%Y")

ternsCOTE$tagid<-as.character(ternsCOTE$tagid)
ternsROST$tagid<-as.character(ternsROST$tagid)
BoatLocation$Location<-as.character(BoatLocation$Location)
BoatLocation$Tower.Transect<-as.character(BoatLocation$Tower.Transect)


a<-c(-71.63, -71.52, -71.502, -71.47, -71.507, -71.653)
b<-c(41.12478, 41.123617, 41.16, 41.18, 41.112, 41.114)

windturbinesloc<-data.frame(a,b)
windturbinesloc$a<-as.numeric(windturbinesloc$a)
windturbinesloc$b<-as.numeric(windturbinesloc$b)

z<-as.character(round(ternsCOTE$long, 4))
d<-as.character(round(ternsCOTE$lat, 4))

e<-as.character(round(ternsROST$long, 4))
f<-as.character(round(ternsROST$lat, 4))


new<-read.csv("new.csv")
newChick<-read.csv("new.csv")
newF<-read.csv("newF.csv")
newM<-read.csv("newM.csv")
newAll<-read.csv("newAll.csv")
newAllChick<-read.csv("newAll.csv")
newCOTE<-read.csv("newCote.csv")
newCOTEM<-read.csv("newCoteM.csv")
newCOTEMChick<-read.csv("newCoteM.csv")
newCOTEF<-read.csv("newCoteF.csv")
newCOTEFChick<-read.csv("newCoteF.csv")
newROST<-read.csv("newRost.csv")
newROSTM<-read.csv("newRostM.csv")
newROSTMChick<-read.csv("newRostM.csv")
newROSTF<-read.csv("newRostF.csv")
newROSTFChick<-read.csv("newRostF.csv")

newChick <- newChick[!is.na(newChick$ChkAgeMean),]
newAllChick <- newAllChick[!is.na(newAllChick$ChkAgeMean),]
newCOTEMChick <- newCOTEMChick[!is.na(newCOTEMChick$ChkAgeMean),]
newCOTEFChick <- newCOTEFChick[!is.na(newCOTEFChick$ChkAgeMean),]
newROSTMChick <- newROSTMChick[!is.na(newROSTMChick$ChkAgeMean),]
newROSTFChick <- newROSTFChick[!is.na(newROSTFChick$ChkAgeMean),]


newAll$DailyDate<-as.POSIXct(x=newAll$DailyDate, format="%m/%d/%Y")
new$DailyDate<-as.POSIXct(x=new$DailyDate, format="%Y-%m-%d")
newF$DailyDate<-as.POSIXct(x=newF$DailyDate, format="%m/%d/%Y")
newM$DailyDate<-as.POSIXct(x=newM$DailyDate, format="%m/%d/%Y")
newCOTE$DailyDate<-as.POSIXct(x=newCOTEM$DailyDate, format="%m/%d/%Y")
newCOTEM$DailyDate<-as.POSIXct(x=newCOTEM$DailyDate, format="%m/%d/%Y")
newCOTEF$DailyDate<-as.POSIXct(x=newCOTEF$DailyDate, format="%m/%d/%Y")
newROST$DailyDate<-as.POSIXct(x=newROST$DailyDate, format="%m/%d/%Y")
newROSTM$DailyDate<-as.POSIXct(x=newROSTM$DailyDate, format="%m/%d/%Y")
newROSTF$DailyDate<-as.POSIXct(x=newROSTF$DailyDate, format="%m/%d/%Y")


newAll$tsHr<-as.POSIXct(x=newAll$tsHr, format="%m/%d/%Y %H:%M")
new$tsHr<-as.POSIXct(x=new$tsHr, format="%Y-%m-%d %H:%M")
newF$tsHr<-as.POSIXct(x=newF$tsHr, format="%m/%d/%Y %H:%M")
newM$tsHr<-as.POSIXct(x=newM$tsHr, format="%m/%d/%Y %H:%M")
newCOTE$tsHr<-as.POSIXct(x=newCOTEM$tsHr, format="%m/%d/%Y %H:%M")
newCOTEM$tsHr<-as.POSIXct(x=newCOTEM$tsHr, format="%m/%d/%Y %H:%M")
newCOTEF$tsHr<-as.POSIXct(x=newCOTEF$tsHr, format="%m/%d/%Y %H:%M")
newROST$tsHr<-as.POSIXct(x=newROST$tsHr, format="%m/%d/%Y %H:%M")
newROSTM$tsHr<-as.POSIXct(x=newROSTM$tsHr, format="%m/%d/%Y %H:%M")
newROSTF$tsHr<-as.POSIXct(x=newROSTF$tsHr, format="%m/%d/%Y %H:%M")


## Create yday 
newAll$yday <- yday(newAll$DailyDate) # day of the year 
length(unique(newAll$yday))

yday("2016-06-26 00:00:00") #6/26/2016  is yday 178, 8/1/2016 is yday 214
yday("2016-08-01 00:00:00")

newF$yday <- yday(newF$DailyDate) # day of the year 
length(unique(newF$yday))

newM$yday <- yday(newM$DailyDate) # day of the year 
length(unique(newM$yday))

newROST$yday<-yday(newROST$DailyDate)
length(unique(newROST$yday))

newROSTM$yday<-yday(newROSTM$DailyDate)
length(unique(newROSTM$yday))

newROSTF$yday<-yday(newROSTF$DailyDate)
length(unique(newROSTF$yday))

newCOTE$yday<-yday(newCOTE$DailyDate)
length(unique(newCOTE$yday))

newCOTEF$yday<-yday(newCOTEF$DailyDate)
length(unique(newCOTEF$yday))

newCOTEM$yday<-yday(newCOTEM$DailyDate)
length(unique(newCOTEM$yday))

#To round and have two decimal places for each summary statistic.
#specify_decimal <- function(x, k) format(specify_decimal(x, k), nsmall=k)

###Summary Statistics Calculations

#All Birds Overtime Linear Model
M.lm1<- lm(newAll$Birds~newAll$yday)
my.p1 <-summary(M.lm1)$coefficients[2,4] 
my.p1<-c("<0.001")
r1<-(round(summary(M.lm1)$r.squared,3))*100

bf1<-paste(sep = "",
  format(round(coef(M.lm1)[1],1),nsmall=2), " + ", 
  format(round(coef(M.lm1)[2],2), nsmall=2), " x")

###VISIBILITY NON-LINEAR MODEL

Alm2 <- lm(newAll$VisM~ newAll$yday + I(newAll$yday^2))
M.lm2<-lm(newAll$VisM~newAll$yday)
my.p2 <-summary(Alm2)$coefficients[3,4] 
my.p2<-c("<0.001")
r2<-round(summary(Alm2)$r.squared,3)*100

#Use AIC to check whether linear or quadratic model is best-fit.
AIC(M.lm2, Alm2)

###CHECK BEST FIT LINE EQUATION

bf2<-paste(sep = "",
  format(round(coef(Alm2)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm2)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm2)[3],2), nsmall=2), "x^2 ")

###TEMPERATURE NON-LINEAR MODEL

Alm3 <- lm(newAll$TempF~ newAll$yday + I(newAll$yday^2))
M.lm3<-lm(newAll$TempF~newAll$yday)
my.p3 <-summary(Alm3)$coefficients[3,4] 
my.p3<-c("<0.001")
r3<-round(summary(Alm3)$r.squared,3)*100

AIC(M.lm3, Alm3)

###CHECK BEST FIT LINE EQUATION
bf3<-paste(sep = "",
  format(round(coef(Alm3)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm3)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm3)[3],2), nsmall=2), "x^2 ")

###DEW POINT NON-LINEAR MODEL
Alm4 <- lm(newAll$DewPtF~ newAll$yday + I(newAll$yday^2))
M.lm4<- lm(newAll$DewPtF~newAll$yday)
my.p4 <-summary(Alm4)$coefficients[3,4]
my.p4<-c("<0.001")
r4<-round(summary(Alm4)$r.squared,3)*100

AIC(M.lm4, Alm4)

###CHECK BEST FIT LINE EQUATION
bf4<-paste(sep = "",
  format(round(coef(Alm4)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm4)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm4)[3],2), nsmall=2), "x^2 ")

###HUMIDITY NON-LINEAR MODEL
Alm5 <- lm(newAll$Humid~ newAll$yday + I(newAll$yday^2))
my.p5 <-summary(Alm5)$coefficients[3,4] 
my.p5<-c("<0.001")
r5<-round(summary(Alm5)$r.squared,3)*100

M.lm5<- lm(newAll$Humid~ newAll$yday)
AIC(M.lm5, Alm5)

###CHECK BEST FIT LINE EQUATION
bf5<-paste(sep = "",
  format(round(coef(Alm5)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm5)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm5)[3],2), nsmall=2), "x^2 ")

###SEA LEVEL PRESSURES LINEAR MODEL
Alm6 <- lm(newAll$SeaLvlPrs~ newAll$yday + I(newAll$yday^2))
M.lm6<- lm(newAll$SeaLvlPrs~ newAll$yday)
my.p6 <-summary(M.lm6)$coefficients[2,4] 
my.p6 <- round(my.p6, 3)
my.p6<-c("<0.001")
r6<-round(summary(M.lm6)$r.squared,3)*100

AIC(M.lm6, Alm6)

###CHECK BEST FIT LINE EQUATION
bf6<-paste(sep = "",
  format(round(coef(M.lm6)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm6)[2],2), nsmall=2), " x")

#WIND SPEED LINEAR MODEL

M.lm7<-lm(newAll$WindSpd~newAll$yday)
Alm7<- lm(newAll$WindSpd~newAll$yday+I(newAll$yday^2))
my.p7 <-summary(M.lm7)$coefficients[2,4] 
my.p7 <- format(round(my.p7, 3), nsmall=3)
r7<-round(summary(M.lm7)$r.squared,3)*100

AIC(M.lm7, Alm7)

bf7<-paste(sep = "",
  format(round(coef(M.lm7)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm7)[2],2), nsmall=2), "x")

###FEMALE BIRDS OVERTIME NON-LINEAR MODEL
Alm7a <- lm(newF$Birds~ newF$yday + I(newF$yday^2))
lm7a<-lm(newF$Birds~ newF$yday )
my.p7a <- summary(Alm7a)$coefficients[3,4]
my.p7a <- round(my.p7a, 3)
r7a<-round(summary(Alm7a)$r.squared, 3)*100

AIC(lm7a, Alm7a)

bf7a<-paste(sep = "",
  format(round(coef(Alm7a)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm7a)[2],2), nsmall=2)," x + ",
  format(round(coef(Alm7a)[3],2), nsmall=2), "x^2")

###MALE BIRDS OVERTIME LINEAR MODEL
Alm7b <- lm(newM$Birds~ newM$yday + I(newM$yday^2))
M.lm7b<-lm(newM$Birds~ newM$yday)
my.p7b<-summary(M.lm7b)$coefficients[2,4]
my.p7b <- round(my.p7b, 3)
my.p7b<-c("<0.001")
r7b<-round(summary(M.lm7b)$r.squared,3)*100
r7b

AIC(M.lm7b, Alm7b)

bf7b<-paste(sep = "",
  format(round(coef(M.lm7b)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm7b)[2],2), nsmall=2), " x")

###Statistical Analysis
###ROST OVERTIME NON-LINEAR MODEL
Alm8 <- lm(newROST$Birds~ newROST$yday + I(newROST$yday^2))
M.lm8<- lm(newROST$Birds~ newROST$yday)
my.p8 <-summary(Alm8)$coefficients[3,4] 
my.p8 <- round(my.p8, 3)
r8<-round(summary(Alm8)$r.squared,3)*100

AIC(M.lm8, Alm8)

bf8<-paste(sep = "",
  format(round(coef(Alm8)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm8)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm8)[3],2), nsmall=2), "x^2 ")

###ROST Summary Statistics Calculations
###ROST VISIBILITY NON-LINEAR MODEL
Alm9 <- lm(newROST$VisM~ newROST$yday + I(newROST$yday^2))
M.lm9<- lm(newROST$VisM~newROST$yday)
my.p9 <-summary(Alm9)$coefficients[3,4]
my.p9<- c("<0.001")
r9<-round(summary(Alm9)$r.squared,3)*100

AIC(M.lm9, Alm9)

bf9<-paste(sep = "",
  format(round(coef(Alm9)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm9)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm9)[3],2), nsmall=2), "x^2 ")

###ROST TEMPERATURE NON-LINEAR MODEL
Alm10 <- lm(newROST$TempF~ newROST$yday + I(newROST$yday^2))
M.lm10<-lm(newROST$TempF~newROST$yday)
my.p10 <-summary(Alm10)$coefficients[3,4] 
my.p10<-c("<0.001")
r10<-round(summary(Alm10)$r.squared, 3)*100

AIC(M.lm10, Alm10)

bf10<-paste(sep = "",
  format(round(coef(Alm10)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm10)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm10)[3],2), nsmall=2), "x^2 ")

###ROST DEW POINT NON-LINEAR MODEL
Alm11 <- lm(newROST$DewPtF~ newROST$yday + I(newROST$yday^2))
M.lm11<-lm(newROST$DewPtF~newROST$yday)
my.p11 <-summary(Alm11)$coefficients[3,4] 
my.p11 <- c("<0.001")
r11<-round(summary(Alm11)$r.squared, 3)*100
AIC(M.lm11, Alm11)

bf11<-paste(sep = "",
  format(round(coef(Alm11)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm11)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm11)[3],2), nsmall=2), "x^2 ")

###ROST HUMIDITY NON-LINEAR MODEL
Alm12 <- lm(newROST$Humid~ newROST$yday + I(newROST$yday^2))
M.lm12<- lm(newROST$Humid~newROST$yday)
my.p12 <-summary(Alm12)$coefficients[3,4] 
my.p12 <- c("<0.001")
r12<-round(summary(Alm12)$r.squared, 3)*100
r12
AIC(M.lm12, Alm12)

bf12<-paste(sep = "",
  format(round(coef(Alm12)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm12)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm12)[3],2), nsmall=2), "x^2 ")

###NEGATIVE AIC, THE ONE CLOSER TO 0 IS BETTER FIT MODEL, HAVE ROST SEA LEVEL PRESSURE NON-LINEAR MODEL
lm13 <- lm(newROST$SeaLvlPrs~ newROST$yday)
Alm13<-lm(newROST$SeaLvlPrs~newROST$yday+I(newROST$yday^2))
AIC(lm13, Alm13)

my.p13 <-summary(Alm13)$coefficients[3,4] 
my.p13 <- c("<0.001")
r13<- round(summary(Alm13)$r.squared, 3)*100
r13<-format(r13, nsmall=2)

bf13<-paste(sep = "",
  format(round(coef(Alm13)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm13)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm13)[3],2), nsmall=2), "x^2 ")

###ROST WIND SPEED NON-LINEAR MODEL
Alm14 <- lm(newROST$WindSpd~ newROST$yday + I(newROST$yday^2))
M.lm14<- lm(newROST$WindSpd~ newROST$yday)
my.p14 <-summary(Alm14)$coefficients[3,4] 
my.p14 <- round(my.p14, 3)
r14<-round(summary(Alm14)$r.squared, 3)*100
r14

AIC(M.lm14, Alm14)

bf14<-paste(sep = "",
  format(round(coef(Alm14)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm14)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm14)[3],2), nsmall=2), "x^2 ")

###ROST FEMALES OVERTIME NON-LINEAR MODEL
Alm14a <- lm(newROSTF$Birds~ newROSTF$yday + I(newROSTF$yday^2))
M.lm14a<-lm(newROSTF$Birds~ newROSTF$yday)
my.p14a <- summary(Alm14a)$coefficients[3,4]
my.p14a <- c("<0.001")
r14a<-round(summary(Alm14a)$r.squared, 3)*100

AIC(M.lm14a, Alm14a)

bf14a<-paste(sep = "",
  format(round(coef(Alm14a)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm14a)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm14a)[3],2), nsmall=2), "x^2 ")

###ROST MALES OVERTIME NON-LINEAR MODEL
Alm14b <- lm(newROSTM$Birds~ newROSTM$yday + I(newROSTM$yday^2))
M.lm14b<- lm(newROSTM$Birds~ newROSTM$yday)
my.p14b<-summary(Alm14b)$coefficients[3,4]
my.p14b<- c("<0.001")
r14b <-round(summary(Alm14b)$r.squared, 3)*100

AIC(M.lm14b, Alm14b)

bf14b<-paste(sep = "",
  format(round(coef(Alm14b)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm14b)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm14b)[3],2), nsmall=2), "x^2 ")

###COTE Statistical Analysis

#All COTE
Alm15 <- lm(newCOTE$Birds~ newCOTE$yday + I(newCOTE$yday^2))
M.lm15<- lm(newCOTE$Birds~newCOTE$yday)
my.p15 <-summary(Alm15)$coefficients[3,4] 
my.p15 <- c("<0.001")
r15<-round(summary(Alm15)$r.squared, 3)*100

AIC(M.lm15, Alm15)

bf15<-paste(sep = "",
  format(round(coef(Alm15)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm15)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm15)[3],2), nsmall=2), "x^2 ")

###COTE VISIBILITY NON-LINEAR MODEL
Alm16 <- lm(newCOTE$VisM~ newCOTE$yday + I(newCOTE$yday^2))
M.lm16<-lm(newCOTE$VisM~ newCOTE$yday)
my.p16 <-summary(Alm16)$coefficients[3,4] 
my.p16 <- c("<0.001")
r16<-round(summary(Alm16)$r.squared, 3)*100

AIC(M.lm16, Alm16)

bf16<-paste(sep = "",
  format(round(coef(Alm16)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm16)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm16)[3],2), nsmall=2), "x^2 ")

###COTE TEMP NON-LINEAR MODEL
Alm17 <- lm(newCOTE$TempF~ newCOTE$yday + I(newCOTE$yday^2))
M.lm17<- lm(newCOTE$TempF~ newCOTE$yday)
my.p17 <-summary(Alm17)$coefficients[3,4] 
my.p17 <- c("<0.001")
r17<-round(summary(Alm17)$r.squared, 3)*100
r17

AIC(M.lm17, Alm17)

bf17<-paste(sep = "",
  format(round(coef(Alm17)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm17)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm17)[3],2), nsmall=2), "x^2 ")

#LINEAR MODEL 
M.lm18<- lm(newCOTE$DewPtF~newCOTE$yday)
Alm18 <- lm(newCOTE$DewPtF~ newCOTE$yday + I(newCOTE$yday^2))
my.p18 <-summary(M.lm18)$coefficients[2,4] 
my.p18 <- c("<0.001")
r18<-round(summary(M.lm18)$r.squared, 3)*100

AIC(M.lm18, Alm18)

bf18<-paste(sep = "",
  format(round(coef(M.lm18)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm18)[2],2), nsmall=2), " x")

###COTE HUMIDITY NON-LINEAR MODEL
Alm19 <- lm(newCOTE$Humid~ newCOTE$yday + I(newCOTE$yday^2))
M.lm19<- lm(newCOTE$Humid~ newCOTE$yday)
my.p19 <-summary(Alm19)$coefficients[3,4] 
my.p19 <- c("<0.001")
r19<-round(summary(Alm19)$r.squared, 3)*100

AIC(M.lm19, Alm19)

bf19<-paste(sep = "",
  format(round(coef(Alm19)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm19)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm19)[3],2), nsmall=2), "x^2 ")

###NEGATIVE AIC, CLOSER TO 0 IS BETTER FIT MODEL, COTE SEA LEVEL PRESSURES LINEAR MODEL
Alm20 <- lm(newCOTE$SeaLvlPrs~ newCOTE$yday + I(newCOTE$yday^2))
M.lm20<-lm(newCOTE$SeaLvlPrs~ newCOTE$yday)
my.p20 <-summary(M.lm20)$coefficients[2,4] 
my.p20 <- c("<0.001")
r20<-round(summary(M.lm20)$r.squared, 3)*100

AIC(M.lm20, Alm20)

bf20<-paste(sep = "",
  format(round(coef(M.lm20)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm20)[2],2), nsmall=2), " x")

###COTE WIND SPEED NON-LINEAR MODEL
Alm21 <- lm(newCOTE$WindSpd~ newCOTE$yday + I(newCOTE$yday^2))
M.lm21<- lm(newCOTE$WindSpd~ newCOTE$yday)
AIC(M.lm21, Alm21)
my.p21 <-summary(Alm21)$coefficients[3,4] 
my.p21<- c("<0.001")
r21<-round(summary(Alm21)$r.squared, 3)*100
r21<-format(r21, nsmall=2)

bf21<-paste(sep = "",
  format(round(coef(Alm21)[1],2), nsmall=2), " + ", 
  format(round(coef(Alm21)[2],2), nsmall=2), " x + ",
  format(round(coef(Alm21)[3],2), nsmall=2), "x^2 ")

##LINEAR MODEL
M.lm21a <- lm(newCOTEF$Birds~newCOTEF$yday)
Alm21a <- lm(newCOTEF$Birds~ newCOTEF$yday + I(newCOTEF$yday^2))
AIC(M.lm21a, Alm21a)
my.p21a <- summary(M.lm21a)$coefficients[2,4]
my.p21a <- c("<0.001")
r21a<-round(summary(M.lm21a)$r.squared, 3)*100
r21a

bf21a<-paste(sep = "",
  format(round(coef(M.lm21a)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm21a)[2],2), nsmall=2), " x")

##LINEAR MODEL
M.lm21b <- lm(newCOTEM$Birds~newCOTEM$yday)
Alm21b<-lm(newCOTEM$Birds~newCOTEM$yday+I(newCOTEM$yday^2))
AIC(M.lm21b,Alm21b)
my.p21b<-summary(M.lm21b)$coefficients[2,4]
my.p21b <- c("<0.001")
r21b<-round(summary(M.lm21b)$r.squared, 3)*100
r21b

bf21b<-paste(sep = "",
  format(round(coef(M.lm21b)[1],2), nsmall=2), " + ", 
  format(round(coef(M.lm21b)[2],2), nsmall=2), " x")









ui <- fluidPage(
  #Name graph title.
  titlePanel("Block Island Tern and Weather Data Analysis"), 
  navbarPage("",
             tabPanel("Welcome!", h6("Created by Tyler Sym and Rachel Tham"), br(),
                      fluidRow(
                        column(width = 4, h3("Welcome!"), br(), br(),
                          "The first offshore wind turbines in the USA have been in operation since early 2017, off the coast of Block Island, 
                          Rhode Island. The purpose of this app is to evaluate the activity patterns of seabirds around Block Island. 
                          We analyzed the activity of Common and Roseate Terns (the latter of which is federally endangered), relative to weather 
                          and time of day covariates. This information is important in assessments of potential collision risk.",
                          br(), br(),
                          "Please be patient for each tab to load properly!",
                          br(), br(),
                          img(src='Chick1.jpg', height = 400, width = 500, align = "left"), br(), "Photo by Tyler Sym", br(), br()),
                        column(width = 1
                               ),
                        column(width = 5,
                               img(src='Bird1.jpg', height = 400, width = 600, align = "left"), br(), "Photo by Tyler Sym", br(), br(),
                               "Our objective was to display a large amount of data collected about nanotagged terns and weather in the Block Island area 
                                and make it easily navigable for users. There are several tabs located at the top of this page that let you view the data 
                                in different ways. The first three 'Tern' tabs plot different variables over time. The data for these 
                                tabs are aggregated by day, meaning that the weather data are averaged across 24 hours and the tern detections are summed across 24 
                                hours to create each single plot value. This is helpful for viewing trends in the data over a long period of time. The next tab is 
                                similar, but aggregates the data by hour. This is helpful for zooming in on very specific portions of the graph, where the data are 
                                more detailed. It also allows you to plot a new hourly dependent variable, tidal height. The ‘Covariate Plots’ tab plots 
                                different variables against each other, eliminating the scale of time. Plotting variables against each other helps us explore correlations 
                                between them. The data used in the aforementioned tabs was collected with a stationary detection tower located on the southeast tip of Block Island.
                                Finally, the 'Field Work' tab shows abundance data that were collected in July 2017, after the construction of the wind 
                                farm was completed and the turbines became fully functional. The data in this tab were collected manually; the research team used binoculars to 
                                spot birds and determine their behavior and proximity to the boat, and also used a handheld yagi antenna to detect tagged birds around the boat.",
                               br(), br(),
                               img(src='Turbines1.jpg', height = 400, width = 550, align = "left"), br(), "Photo by Rachel Tham"
                               )
                               
                               )
                      
                      ),
             
             
             tabPanel("All Terns", br(),
                      "The data on this tab and the following 4 tabs were collected with a stationary detection tower on the southeast tip of Block Island. 
                      These data are aggregated by day. To isolate a variable, double click on the variable name in the legend. 
                      To add or remove variables, click once on the variable name in the legend. To zoom in on a particular part
                      of the graph, click and drag a box around it. To view a specific time range, click on the graph and drag to the right or left.", br(), br(),
                      plotlyOutput("plot"), br(),
                      "Summary Statistics for Variables (if best-fit-lines were drawn through the data):",
                      verbatimTextOutput("summary1"),
                      
                      h3("Data Analysis Examples"), br(), 
                      
                      "The screenshot of the graph below was taken from this 'All Terns' tab. It shows visibility on the right y-axis and tern detections on the left. 
                      Most bird detections occurred during clear skies (e.g. 10 miles). Some bird detections occurred at lower visibilities, but it was rare 
                      to detect any birds on days when the average visibility was below 5 miles. This conclusion is also supported by a plot that is
                      shown in the 'Data Analysis Examples' section of our 'Covariate Plots' tab.",
                      
                      br(), br(),
                      
                      img(src='AllTerns.jpg', height = 350, width = 1100, align = "left"),
                      
                      br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), 
                      br(), br(), br(), br(), br(), br(), br()
                      
                      ),
             
             tabPanel("Roseate Terns", br(),
                      "These data are aggregated by day. To isolate a variable, double click on the variable name in the legend. 
                      To add or remove variables, click once on the variable name in the legend. To zoom in on a particular part
                      of the graph, click and drag a box around it. To view a specific time range, click on the graph and drag to the right or left.", br(), br(),
                      plotlyOutput("plot1"), br(),
                      "Summary Statistics for Variables (if best-fit-lines were drawn through the data):",
                      verbatimTextOutput("summary2"),
                      
                      h3("Data Analysis Examples"), br(), 
                      
                      
                      "From the above graph, we extracted the plot below to show the relationship between the daily number of 
                       Roseate Tern detections and the average wind speed. Fewer Roseate Terns (n=16) were detected than Common 
                       Terns (n=30) and a maximum of 10 Roseate Terns was detected on July 3rd, 2016. The Roseate Tern detections 
                       consisted of 79.3% females and 20.7% males – these males were detected between June 12th and June 25th, 2016. 
                       Only one Roseate Tern was detected after July – it was a female, detected on August 5th. From June 19th to 
                       August 8th, 2016, as the wind speed increased (e.g. greater than 10 miles per hour), the number of tern detections
                       increased, especially from June 20th to August 5th.",
                      
                      br(), br(), br(), br(), br(), 
                      
                      img(src='Roseates.jpg', height = 350, width = 1100, align = "left"),
                      
                      br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), 
                      br(), br(), br(), br(), br(), br(), br()
                      
                      ),
             
             tabPanel("Common Terns", br(),
                      "These data are aggregated by day. To isolate a variable, double click on the variable name in the legend. 
                      To add or remove variables, click once on the variable name in the legend. To zoom in on a particular part
                      of the graph, click and drag a box around it. To view a specific time range, click on the graph and drag to the right or left.", br(), br(),
                      plotlyOutput("plot2"), br(),
                      "Summary Statistics for Variables (if best-fit-lines were drawn through the data):",
                      verbatimTextOutput("summary3"),
                      
                      h3("Data Analysis Examples"), br(), 
                      
                      
                      "From the above graph, we extracted the plot below to show the relationship between the daily number of 
                      Common Tern detections and the average wind speed. More Common Terns (n=30) were detected than Roseate Terns
                      (n=16) and a maximum of 18 Common Terns was detected on July 3rd, 2016. The Common Tern detections consisted of 
                      37.7% females and 62.3% males – only two female Common terns were detected after August 7th. From June 19th to 
                      August 8th, 2016, as the wind speed increased (e.g. greater than 10 miles per hour), the number of tern detections
                      increased, especially from June 20th to July 31st.",
                      
                      br(), br(), br(), br(), br(), 
                      
                      img(src='Commons.jpg', height = 350, width = 1100, align = "left"),
                      
                      br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), 
                      br(), br(), br(), br(), br(), br(), br()
                      
                      
                      ),
             tabPanel("Hourly Data", br(),
                      "These data are aggregated by hour. To isolate a variable, double click on the variable name in the legend. 
                      To add or remove variables, click once on the variable name in the legend. To zoom in on a particular part
                      of the graph, click and drag a box around it. To view a specific time range, click on the graph and drag to the right or left.", br(), br(),
                      plotlyOutput("plot3"), br(), br(), br(), br(),
                      
                      h3("Data Analysis Examples"), br(), 
                      
                      "Included below is a screenshot taken from this 'Hourly Data' tab. The graph is zoomed in between June 29th and July 4th 
                      (the time during which bird detections were highest) and the “All Terns” and 'Tidal Height' variables are plotted. 
                      The first thing to notice is that the bird detections appear to occur in specific aggregations each day. Most of the
                      birds were detected in the early morning, starting at 5:00AM. It is interesting to note how the timing of these detection 
                      clusters correlates with the rising tide. This suggests that the terns feed in the Block Island area as the tide increases. 
                      This is consistent with evidence in the literature that the positive effect of low tides on food availability increases foraging
                      activity in terns (Safina and Burger: 1988, 160). The rising waters increase the accessibility of forage fish to terns feeding over shoals.",
                      
                      br(), br(),
                      
                      img(src='TidalBirdCorrelation.jpg', height = 350, width = 1100, align = "left"),
                      
                      br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), 
                      br(), br(), br(), br(), br(), br(), br(),
                       

                      "The next screenshot shows data between June 27th and July 13th. Plotted on the left axis is 
                      all tern detections and on the right axis is inches of precipitation. Notice the negative correlation between these two variables. 
                      During periods of rainfall, tern detections are rare. In fact, across the entire season, only once did a tern detection fall within an 
                      hour when rainfall was recorded. That one case occurred on the afternoon of July 14th, when 1 male tern was detected during an hour in which 
                      0.02 inches of rainfall were recorded.",
                      
                      br(), br(), 
                      
                      img(src='PrecipBirdCorrelation.jpg', height = 350, width = 1100, align = "left"),
                      
                      br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), 
                      br(), br(), br(), br(), br(), br(), br()
                      
             ),
             
             tabPanel("Covariate Plots",
                      fluidRow(
                        column(width = 5,
                               selectInput("variable", "Choose an X-Variable:",
                                           list("Temperature" = "TempF", 
                                                "Visibility" = "VisM", 
                                                "Dew Point" = "DewPtF",
                                                "Humidity" = "Humid",
                                                "Barometric Pressure" = "SeaLvlPrs",
                                                "Wind Speed" = "WindSpd",
                                                "Wind Direction" = "WindDirCirc",
                                                "Chick Age" = "ChkAgeMean",
                                                "Precipitation" = "Precip"
                                           )
                               ),
                               br(), br(),
                               
                               conditionalPanel(
                                 condition = "input.variable == 'TempF'",
                                 sliderInput("tempvalue", "Zoom to a Specific Temperature Range (Degrees F)",
                                             min = 60, max = 80, value = c(60, 80))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'VisM'",
                                 sliderInput("visvalue", "Zoom to a Specific Visibility Range (Miles)",
                                             min = 4, max = 10, value = c(4, 10))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'DewPtF'",
                                 sliderInput("dewvalue", "Zoom to a Specific Dew Point Temperature Range (Degrees F)",
                                             min = 55, max = 80, value = c(55, 80))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'Humid'",
                                 sliderInput("humidvalue", "Zoom to a Specific Humidity Range (%)",
                                             min = 60, max = 100, value = c(60, 100))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'SeaLvlPrs'",
                                 sliderInput("pressurevalue", "Zoom to a Specific Barometric Pressure Range (Inches Hg)", step = 0.1,
                                             min = 29.7, max = 30.3, value = c(29.7, 30.3))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'WindSpd'",
                                 sliderInput("windspdvalue", "Zoom to a Specific Wind Speed Range (MPH)",
                                             min = 4, max = 17, value = c(4, 17))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'WindDirCirc'",
                                 sliderInput("winddirvalue", "Zoom to a Specific Wind Direction Range (Degrees)",
                                             min = 0, max = 360, value = c(0, 360))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'ChkAgeMean'",
                                 sliderInput("chickvalue", "Zoom to a Specific Chick Age Range (Days Old)",
                                             min = 0, max = 62, value = c(0, 62))
                               ),
                               conditionalPanel(
                                 condition = "input.variable == 'Precip'",
                                 sliderInput("precipvalue", "Zoom to a Specific Precipitation Range (Inches)",
                                             min = 0, max = 4, value = c(0, 4))
                               ),
                               
                               plotOutput("allternPlot"), br(),
                               "Summary statistics for generated best-fit-line. All birds included.", br(),
                               verbatimTextOutput("alllineinfo"), br(),
                               "Summary statistics for chosen x-variable.", br(),
                               verbatimTextOutput("allstats"), br(), br(), br()
                        ),
                        column(width = 1),
                        column(width = 5, br(), br(),
                               "Please specify the bird sex and species that you would like to view:", br(), br(), 
                               radioButtons("sexInput", "Sex",
                                            choices = c("M", "F"),
                                            selected = "M", inline = T),
                               radioButtons("speciesInput", "Species",
                                            choices = c("COTE", "ROST"),
                                            selected = "COTE", inline = T),
                               br(), 
                               plotOutput("ternPlot"), br(),
                               "Summary statistics for generated best-fit-line. Filtered for sex and species.", br(),
                               verbatimTextOutput("lineinfo"), br(), 
                               "Summary statistics for filtered sex and species.", br(),
                               verbatimTextOutput("stats"), br(), br(), br()
                        )
                      ),
                      
                      h3("Data Analysis Examples"), br(), 
                      fluidRow(
                        column(width = 5,
                            "The plot below was created in this 'Covariates Plot' tab. The x-axis shows the wind speed measured 
                            in miles per hour and the y-axis shows bird detections. It is important to note that these data are 
                            aggregated by day, meaning that the wind speed values are daily averages and the bird detection values are 
                            daily summations. In this plot, we can see from the best-fit-line (which explains almost 11% of the variance) 
                            that bird detections are related to wind speeds in a roughly parabolic way. The p-value of this line is <0.001, 
                            which indicates that the relationship between the two covariates is significant. The most birds were detected on days 
                            when the average wind speed was between 9 and 13 miles per hour.",
                      
                            br(), br(), 
                      
                            img(src='WindSpeedvsBirds.jpg', height = 550, width = 500, align = "left"),
                            
                            br(), br(), br(), br(), br(), br(), br(),
                            br(), br(), br(), br(), br(), br(), br(),
                            br(), br(), br(), br(), br(), br(), br(),
                            br(), br(), br(), br(), br(), br(), br()),
                        
                        column(width = 1),
                        column(width = 5,
                               "The following plot once again shows the correlation between between decreased visibility and decreased tern detections.
                               In 91 total hours when the visibility was less than 5 miles, 6 terns were detected. In 1,352 total hours when the visibility
                               was greater than 5 miles, those same 6 terns were recorded, in addition to 40 others. ",
                               
                               br(), br(), 
                               
                               img(src='VisCovariate.jpg', height = 550, width = 500, align = "left"),
                               
                               br(), br(), br(), br(), br(), br(), br(),
                               br(), br(), br(), br(), br(), br(), br()
                             
                             ))
                      
                      
             ),
                      
             
             
             tabPanel("July 2017 Field Work Map",
                      "Click on a data point to view tern detection or sighting information. 
                      To pan the map, click and drag mouse. To zoom in, use the left buttons or double click.",
                      br(), br(), br(),
                      "These data were collected manually while performing two independent boat surveys on July 5th and July 6th, 2017. The surveys were 
                      conducted after the construction of the Block Island wind farm was completed, with the intention of assessing how the tubines are impacting 
                      bird behavior. Researchers used binoculars to spot birds from the boat and used a handheld yagi antenna to detect tagged birds around the boat.
                      The sightings and detections are plotted on the map below.", br(), br(),
                      leafletOutput("mymap", width = "1100", height = "600"),
                      "Sources: Google Maps and Rhode Island Energy Gov: http://www.energy.ri.gov/Pictures/Figure%208.36%20RE%20Zone.JPG",
                      br(), br(), br(), br(), br()),
             
             tabPanel("Acknowledgements", br(),
                      "Shiny App coded by Rachel Tham and Tyler Sym", br(), br(), br(),
                      
                      "We would like to thank Dr. Holly Goyert, Dr. Pamela Loring, Mr. Kevin Rogers, Professor Curt Griffin, and Professor Paul Sievert for their guidance.", 
                      
                      br(), br(), br(),
                      
                      "Copyright 2017", 
                      
                      br(), 
                    
                      "Summer 2017 University of Massachusetts-Amherst Wind Energy Research Experience for Undergraduates (REU)",
                      
                      br(),
                      
                      "This study was funded in part by the U.S. Department of the Interior, Bureau of Ocean Energy Management through Interagency Agreement M13PG00012 
                      with the U.S. Department of the Interior, Fish and Wildlife Service. The findings and conclusions in this article are those of the authors and do not 
                      necessarily represent the views of the U.S. Fish and Wildlife Service."
             )
             
  ))



























server <- function(input, output) {
  
  BoatLocation$Location<-as.character(BoatLocation$Location)
  
  TowerLocations<-c("Plum Island, NY Tower", "Plum Island, NY Tower", "Great Gull Island, NY Tower", "Napatree, RI Tower", "Montauk, NY Tower",
                    "Trustom Pond, RI Tower", "Block Island, RI Tower")
  TurbineLocations<-c("Turbine 1", "Turbine 2", "Turbine 3", "Turbine 4", "Turbine 5")
  
  output$mymap <- renderLeaflet({
    leaflet(ternsCOTE) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(-71.75, 41.15, zoom = 10) %>% 
      addCircles(~ternsCOTE$long, ~ternsCOTE$lat,  
                 popup = ~paste("Common Tern:", br(), "(", z, ",", d,")", br(),"TagID:", ternsCOTE$tagid), 
                 weight = 4, radius=70, 
                 color="orange", stroke = TRUE, fillOpacity = 0.8) %>% 
      addCircles(~ternsROST$long, ~ternsROST$lat, popup=~paste("Roseate Tern:", br(), "(", e, ",", f, ")", br(),  
                                                               "Tag ID:", ternsROST$tagid), 
                 weight=4, radius=70, color="blue", stroke=TRUE, fillOpacity=0.8) %>%
      
      #Add Transects
      addPolylines(data = BoatPath, ~Longitude, ~Latitude,  
                 weight=2, color="white", stroke=TRUE, fillOpacity=1.0) %>%
      
      addPolylines(data = GPSway, ~GPS.Longitude, ~GPS.Latitude,  
                 weight=2, color="red", stroke=TRUE, fillOpacity=1.0) %>%
      
      addCircles(data = BoatWaypoints[BoatWaypoints$Type == "1",], ~Longitude, ~Latitude,  
                 weight=3, color="cyan", stroke=TRUE, fillOpacity=0.8, radius = 80,
                 popup = ~paste(BoatWaypoints$Count, BoatWaypoints$Bird.Type, br(), "(", round(BoatWaypoints$Longitude, 4), ",", round(BoatWaypoints$Latitude, 4),")", 
                                br(),"Distance from Boat (Meters):", BoatWaypoints$Distance, br(), "Behavior:", BoatWaypoints$Behavior.Direction)) %>%
      
      #Add Tower and Turbine Locations
      addCircles(data = BoatLocation[BoatLocation$Tower.Transect == "Tower",], ~Longitude, ~Latitude,  
                 popup=~paste(TowerLocations, br(), "(", round(BoatLocation$Longitude, 4), ",", round(BoatLocation$Latitude, 4), ")"), 
                 weight=3, radius=200,
                 color="green", stroke=TRUE, fillOpacity=1.0) %>%
      addCircles(data = BoatLocation[BoatLocation$Tower.Transect == "TurbineLoc",], ~Longitude, ~Latitude,  
                 popup = ~paste(TurbineLocations, br(), "(", round(BoatLocation$Longitude, 4), ",", round(BoatLocation$Latitude, 4), ")"), 
                 weight=3, radius=200, color="purple", stroke=TRUE, fillOpacity=1.0) %>%
      
      addLegend("bottomright", colors="white", labels="Day 1 Boat Path") %>%
      addLegend("bottomright", colors="red", labels="Day 2 Boat Path") %>%
      addLegend("bottomright", colors="green", labels="Detection Tower") %>%
      addLegend("bottomright", colors="purple", labels="Turbine Location") %>%
      addLegend("bottomright", colors= "orange", labels="Common Tern Detection") %>%
      addLegend("bottomright", colors="blue", labels="Roseate Tern Detection") %>%
      addLegend("bottomright", colors="cyan", labels="Bird Sighting") %>%
      addPopups(-71.5843, 41.1617, paste("Block Island")) %>%
      addPopups(-72.11829, 41.20246, paste("Great Gull Island")) %>%
      addPopups(-71.510591, 41.383044, paste("Point Judith Port")) 
    
    
  })
  
  formulaText <- reactive({
    paste("Birds ~", input$variable)
  })
  
  output$allternPlot <- renderPlot({
    
    termpsliderdata <- newAll[newAll$TempF >= input$tempvalue[1] &
                             newAll$TempF <= input$tempvalue[2],]
    vissliderdata <- newAll[newAll$VisM >= input$visvalue[1] &
                           newAll$VisM <= input$visvalue[2],]
    dewsliderdata <- newAll[newAll$DewPtF >= input$dewvalue[1] &
                           newAll$DewPtF <= input$dewvalue[2],]
    humidsliderdata <- newAll[newAll$Humid >= input$humidvalue[1] &
                             newAll$Humid <= input$humidvalue[2],]
    pressuresliderdata <- newAll[newAll$SeaLvlPrs >= input$pressurevalue[1] &
                                newAll$SeaLvlPrs <= input$pressurevalue[2],]
    windspdsliderdata <- newAll[newAll$WindSpd >= input$windspdvalue[1] &
                               newAll$WindSpd <= input$windspdvalue[2],]
    winddirsliderdata <- newAll[newAll$WindDirCirc >= input$winddirvalue[1] &
                               newAll$WindDirCirc <= input$winddirvalue[2],]
    chicksliderdata <- newAllChick[newAllChick$ChkAgeMean >= input$chickvalue[1] &
                                  newAllChick$ChkAgeMean <= input$chickvalue[2],]
    precipsliderdata <- newAll[newAll$Precip >= input$precipvalue[1] &
                                     newAll$Precip <= input$precipvalue[2],]
    
    
    
    if(input$variable == "TempF") {plot(as.formula(formulaText()),
                                        data = termpsliderdata, col = "blue",
                                        main = "All Birds Included", las = 1)}
    
    if(input$variable == "VisM") {plot(as.formula(formulaText()),
                                       data = vissliderdata, col = "blue",
                                       main = "All Birds Included", las = 1)}
    if(input$variable == "DewPtF") {plot(as.formula(formulaText()),
                                         data = dewsliderdata, col = "blue",
                                         main = "All Birds Included", las = 1)}
    if(input$variable == "Humid") {plot(as.formula(formulaText()),
                                        data = humidsliderdata, col = "blue",
                                        main = "All Birds Included", las = 1)}
    if(input$variable == "SeaLvlPrs") {plot(as.formula(formulaText()),
                                            data = pressuresliderdata, col = "blue",
                                            main = "All Birds Included", las = 1)}
    if(input$variable == "WindSpd") {plot(as.formula(formulaText()),
                                          data = windspdsliderdata, col = "blue",
                                          main = "All Birds Included", las = 1)}
    if(input$variable == "WindDirCirc") {plot(as.formula(formulaText()),
                                              data = winddirsliderdata, col = "blue",
                                              main = "All Birds Included", las = 1)}
    if(input$variable == "ChkAgeMean") {plot(as.formula(formulaText()),
                                              data = chicksliderdata, col = "blue",
                                              main = "All Birds Included", las = 1)}
    if(input$variable == "Precip") {plot(as.formula(formulaText()),
                                             data = precipsliderdata, col = "blue",
                                             main = "All Birds Included", las = 1)}
    
    
    lmAllBirdTempF <- lm(newAll$Birds ~ newAll$TempF + I(newAll$TempF^2))
    
    lmAllBirdVisM <- lm(newAll$Birds ~ newAll$VisM)
    
    lmAllBirdDewPtF <- lm(newAll$Birds ~ newAll$DewPtF + I(newAll$DewPtF^2))
    
    lmAllBirdHumid <- lm(newAll$Birds ~ newAll$Humid + I(newAll$Humid^2))
    
    lmAllBirdSeaLvlPrs <- lm(newAll$Birds ~ newAll$SeaLvlPrs + I(newAll$SeaLvlPrs^2))
    
    lmAllBirdWindSpd <- lm(newAll$Birds ~ newAll$WindSpd + I(newAll$WindSpd^2))
    
    lmAllBirdWindDirCirc <- lm(newAll$Birds ~ newAll$WindDirCirc + I(newAll$WindDirCirc^2))
    
    lmAllBirdChick <- lm(newAllChick$Birds ~ newAllChick$ChkAgeMean + I(newAllChick$ChkAgeMean^2))
    
    lmAllBirdPrecip <- lm(newAll$Birds ~ newAll$Precip)
    
    
    if(input$variable == "TempF") {lines(
      x=na.omit(newAll$TempF[order(na.omit(newAll$TempF))]), 
      y=fitted(lmAllBirdTempF)[order(na.omit(newAll$TempF))], col="red")}
    
    if(input$variable == "VisM") {abline(coef=coef(lmAllBirdVisM), col="red")}
    
    if(input$variable == "DewPtF") {lines(
      x=na.omit(newAll$DewPtF[order(na.omit(newAll$DewPtF))]), 
      y=fitted(lmAllBirdDewPtF)[order(na.omit(newAll$DewPtF))], col="red")}
    
    if(input$variable == "Humid") {lines(
      x=na.omit(newAll$Humid[order(na.omit(newAll$Humid))]), 
      y=fitted(lmAllBirdHumid)[order(na.omit(newAll$Humid))], col="red")}
    
    if(input$variable == "SeaLvlPrs") {lines(
      x=na.omit(newAll$SeaLvlPrs[order(na.omit(newAll$SeaLvlPrs))]), 
      y=fitted(lmAllBirdSeaLvlPrs)[order(na.omit(newAll$SeaLvlPrs))], col="red")}
    
    if(input$variable == "WindSpd") {lines(
      x=na.omit(newAll$WindSpd[order(na.omit(newAll$WindSpd))]), 
      y=fitted(lmAllBirdWindSpd)[order(na.omit(newAll$WindSpd))], col="red")}
    
    if(input$variable == "WindDirCirc") {lines(
      x=na.omit(newAll$WindDirCirc[order(na.omit(newAll$WindDirCirc))]), 
      y=fitted(lmAllBirdWindDirCirc)[order(na.omit(newAll$WindDirCirc))], col="red")}
    
    if(input$variable == "ChkAgeMean") {lines(
      x=na.omit(newAllChick$ChkAgeMean[order(na.omit(newAllChick$ChkAgeMean))]), 
      y=fitted(lmAllBirdChick)[order(na.omit(newAllChick$ChkAgeMean))], col="red")}
    
    if(input$variable == "Precip") {abline(coef=coef(lmAllBirdPrecip), col="red")}
    
    
    output$allstats <- renderPrint({
      summary(newAll[, input$variable])
    })
    
    output$alllineinfo <- renderPrint({
      if(input$variable == "TempF") {
        
        cat("The equation is: y = ",
          round(coef(lmAllBirdTempF)[3],2), "x^2 + ",
          round(coef(lmAllBirdTempF)[2],2), "x ",
          round(coef(lmAllBirdTempF)[1],2), sep = "", "
          
")
        cat("This model explains ",
            round(summary(lmAllBirdTempF)$r.squared*100,2),
            "% of the variance", "
          
")
        
        
        cat("The p-value is:", "<0.001")}
      
      
      
      
      
      if(input$variable == "VisM") {
        cat("The equation is: y = ", 
            round(coef(lmAllBirdVisM)[2],2), "x ",
            round(coef(lmAllBirdVisM)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmAllBirdVisM)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "<0.001")}    
      
      
      
      
      
      
      if(input$variable == "DewPtF") {
        
        cat("The equation is: y = ",
            round(coef(lmAllBirdDewPtF)[3],2), "x^2 + ",
            round(coef(lmAllBirdDewPtF)[2],2), "x ",
            round(coef(lmAllBirdDewPtF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmAllBirdDewPtF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      if(input$variable == "Humid") {
        
        cat("The equation is: y = ",
            round(coef(lmAllBirdHumid)[3],2), "x^2 + ",
            round(coef(lmAllBirdHumid)[2],2), "x ",
            round(coef(lmAllBirdHumid)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmAllBirdHumid)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      if(input$variable == "SeaLvlPrs") {
        
        cat("The equation is: y = ",
            round(coef(lmAllBirdSeaLvlPrs)[3],2), "x^2 + ",
            round(coef(lmAllBirdSeaLvlPrs)[2],2), "x ",
            round(coef(lmAllBirdSeaLvlPrs)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmAllBirdSeaLvlPrs)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      if(input$variable == "WindSpd") {
        
        cat("The equation is: y = ",
            round(coef(lmAllBirdWindSpd)[3],2), "x^2 + ",
            round(coef(lmAllBirdWindSpd)[2],2), "x ",
            round(coef(lmAllBirdWindSpd)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmAllBirdWindSpd)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      if(input$variable == "WindDirCirc") {
        
        cat("The equation is: y = ",
            round(coef(lmAllBirdWindDirCirc)[3],4), "x^2 + ",
            round(coef(lmAllBirdWindDirCirc)[2],2), "x ",
            round(coef(lmAllBirdWindDirCirc)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmAllBirdWindDirCirc)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      if(input$variable == "ChkAgeMean") {
        
        cat("The equation is: y = ",
            round(coef(lmAllBirdChick)[3],3), "x^2 + ",
            round(coef(lmAllBirdChick)[2],2), "x +",
            round(coef(lmAllBirdChick)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmAllBirdChick)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      
      if(input$variable == "Precip") {
        cat("The equation is: y = ", 
            round(coef(lmAllBirdPrecip)[2],2), "x + ",
            round(coef(lmAllBirdPrecip)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmAllBirdPrecip)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "0.255")}    
      
      
      
    })
    
      })
  
  
  
  
  
 
  
  
  
  
  
  
  output$ternPlot <- renderPlot({
    
    filteredData <- new[new$sex == input$sexInput &
                          new$spp == input$speciesInput,]
    
    plot(as.formula(formulaText()), 
         data = filteredData, col = "red", 
         main = "Filtered Data for Selected Sex and Species")
    
    #TempF
    
    lmBirdTempFCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$TempF + I(newCOTEM$TempF^2))
    lmBirdTempFCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$TempF + I(newCOTEF$TempF^2))
    lmBirdTempFROSTM <- lm(newROSTM$Birds ~ newROSTM$TempF + I(newROSTM$TempF^2))
    lmBirdTempFROSTF <- lm(newROSTF$Birds ~ newROSTF$TempF + I(newROSTF$TempF^2))
    
    if(input$variable == "TempF" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
        x=na.omit(newCOTEM$TempF[order(na.omit(newCOTEM$TempF))]), 
        y=fitted(lmBirdTempFCOTEM)[order(na.omit(newCOTEM$TempF))], col="blue")}
    if(input$variable == "TempF" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$TempF[order(na.omit(newCOTEF$TempF))]), 
      y=fitted(lmBirdTempFCOTEF)[order(na.omit(newCOTEF$TempF))], col="blue")}
    if(input$variable == "TempF" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$TempF[order(na.omit(newROSTM$TempF))]), 
      y=fitted(lmBirdTempFROSTM)[order(na.omit(newROSTM$TempF))], col="blue")}
    if(input$variable == "TempF" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$TempF[order(na.omit(newROSTF$TempF))]), 
      y=fitted(lmBirdTempFROSTF)[order(na.omit(newROSTF$TempF))], col="blue")}
    
    #DewPtF
    
    lmBirdDewPtFCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$DewPtF + I(newCOTEM$DewPtF^2))
    lmBirdDewPtFCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$DewPtF + I(newCOTEF$DewPtF^2))
    lmBirdDewPtFROSTM <- lm(newROSTM$Birds ~ newROSTM$DewPtF + I(newROSTM$DewPtF^2))
    lmBirdDewPtFROSTF <- lm(newROSTF$Birds ~ newROSTF$DewPtF + I(newROSTF$DewPtF^2))
    
    if(input$variable == "DewPtF" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$DewPtF[order(na.omit(newCOTEM$DewPtF))]), 
      y=fitted(lmBirdDewPtFCOTEM)[order(na.omit(newCOTEM$DewPtF))], col="blue")}
    if(input$variable == "DewPtF" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$DewPtF[order(na.omit(newCOTEF$DewPtF))]), 
      y=fitted(lmBirdDewPtFCOTEF)[order(na.omit(newCOTEF$DewPtF))], col="blue")}
    if(input$variable == "DewPtF" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$DewPtF[order(na.omit(newROSTM$DewPtF))]), 
      y=fitted(lmBirdDewPtFROSTM)[order(na.omit(newROSTM$DewPtF))], col="blue")}
    if(input$variable == "DewPtF" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$DewPtF[order(na.omit(newROSTF$DewPtF))]), 
      y=fitted(lmBirdDewPtFROSTF)[order(na.omit(newROSTF$DewPtF))], col="blue")}
    
    #SeaLvlPrs
    
    lmBirdSeaLvlPrsCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$SeaLvlPrs + I(newCOTEM$SeaLvlPrs^2))
    lmBirdSeaLvlPrsCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$SeaLvlPrs + I(newCOTEF$SeaLvlPrs^2))
    lmBirdSeaLvlPrsROSTM <- lm(newROSTM$Birds ~ newROSTM$SeaLvlPrs + I(newROSTM$SeaLvlPrs^2))
    lmBirdSeaLvlPrsROSTF <- lm(newROSTF$Birds ~ newROSTF$SeaLvlPrs + I(newROSTF$SeaLvlPrs^2))
    
    if(input$variable == "SeaLvlPrs" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$SeaLvlPrs[order(na.omit(newCOTEM$SeaLvlPrs))]), 
      y=fitted(lmBirdSeaLvlPrsCOTEM)[order(na.omit(newCOTEM$SeaLvlPrs))], col="blue")}
    if(input$variable == "SeaLvlPrs" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$SeaLvlPrs[order(na.omit(newCOTEF$SeaLvlPrs))]), 
      y=fitted(lmBirdSeaLvlPrsCOTEF)[order(na.omit(newCOTEF$SeaLvlPrs))], col="blue")}
    if(input$variable == "SeaLvlPrs" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$SeaLvlPrs[order(na.omit(newROSTM$SeaLvlPrs))]), 
      y=fitted(lmBirdSeaLvlPrsROSTM)[order(na.omit(newROSTM$SeaLvlPrs))], col="blue")}
    if(input$variable == "SeaLvlPrs" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$SeaLvlPrs[order(na.omit(newROSTF$SeaLvlPrs))]), 
      y=fitted(lmBirdSeaLvlPrsROSTF)[order(na.omit(newROSTF$SeaLvlPrs))], col="blue")}
    
    #WindSpd
    
    lmBirdWindSpdCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$WindSpd + I(newCOTEM$WindSpd^2))
    lmBirdWindSpdCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$WindSpd + I(newCOTEF$WindSpd^2))
    lmBirdWindSpdROSTM <- lm(newROSTM$Birds ~ newROSTM$WindSpd + I(newROSTM$WindSpd^2))
    lmBirdWindSpdROSTF <- lm(newROSTF$Birds ~ newROSTF$WindSpd + I(newROSTF$WindSpd^2))
    
    if(input$variable == "WindSpd" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$WindSpd[order(na.omit(newCOTEM$WindSpd))]), 
      y=fitted(lmBirdWindSpdCOTEM)[order(na.omit(newCOTEM$WindSpd))], col="blue")}
    if(input$variable == "WindSpd" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$WindSpd[order(na.omit(newCOTEF$WindSpd))]), 
      y=fitted(lmBirdWindSpdCOTEF)[order(na.omit(newCOTEF$WindSpd))], col="blue")}
    if(input$variable == "WindSpd" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$WindSpd[order(na.omit(newROSTM$WindSpd))]), 
      y=fitted(lmBirdWindSpdROSTM)[order(na.omit(newROSTM$WindSpd))], col="blue")}
    if(input$variable == "WindSpd" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$WindSpd[order(na.omit(newROSTF$WindSpd))]), 
      y=fitted(lmBirdWindSpdROSTF)[order(na.omit(newROSTF$WindSpd))], col="blue")}
    
    #VisM
    
    lmBirdVisMCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$VisM)
    lmBirdVisMCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$VisM)
    lmBirdVisMROSTM <- lm(newROSTM$Birds ~ newROSTM$VisM)
    lmBirdVisMROSTF <- lm(newROSTF$Birds ~ newROSTF$VisM + I(newROSTF$VisM^2))
    
    if(input$variable == "VisM" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$VisM[order(na.omit(newCOTEM$VisM))]), 
      y=fitted(lmBirdVisMCOTEM)[order(na.omit(newCOTEM$VisM))], col="blue")}
    if(input$variable == "VisM" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$VisM[order(na.omit(newCOTEF$VisM))]), 
      y=fitted(lmBirdVisMCOTEF)[order(na.omit(newCOTEF$VisM))], col="blue")}
    if(input$variable == "VisM" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$VisM[order(na.omit(newROSTM$VisM))]), 
      y=fitted(lmBirdVisMROSTM)[order(na.omit(newROSTM$VisM))], col="blue")}
    if(input$variable == "VisM" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$VisM[order(na.omit(newROSTF$VisM))]), 
      y=fitted(lmBirdVisMROSTF)[order(na.omit(newROSTF$VisM))], col="blue")}
    
    #Humid
    
    lmBirdHumidCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$Humid + I(newCOTEM$Humid^2))
    lmBirdHumidCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$Humid + I(newCOTEF$Humid^2))
    lmBirdHumidROSTM <- lm(newROSTM$Birds ~ newROSTM$Humid)
    lmBirdHumidROSTF <- lm(newROSTF$Birds ~ newROSTF$Humid + I(newROSTF$Humid^2))
    
    if(input$variable == "Humid" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$Humid[order(na.omit(newCOTEM$Humid))]), 
      y=fitted(lmBirdHumidCOTEM)[order(na.omit(newCOTEM$Humid))], col="blue")}
    if(input$variable == "Humid" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$Humid[order(na.omit(newCOTEF$Humid))]), 
      y=fitted(lmBirdHumidCOTEF)[order(na.omit(newCOTEF$Humid))], col="blue")}
    if(input$variable == "Humid" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$Humid[order(na.omit(newROSTM$Humid))]), 
      y=fitted(lmBirdHumidROSTM)[order(na.omit(newROSTM$Humid))], col="blue")}
    if(input$variable == "Humid" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$Humid[order(na.omit(newROSTF$Humid))]), 
      y=fitted(lmBirdHumidROSTF)[order(na.omit(newROSTF$Humid))], col="blue")}
    
    #WindDirCirc
    
    lmBirdWindDirCircCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$WindDirCirc + I(newCOTEM$WindDirCirc^2))
    lmBirdWindDirCircCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$WindDirCirc)
    lmBirdWindDirCircROSTM <- lm(newROSTM$Birds ~ newROSTM$WindDirCirc + I(newROSTM$WindDirCirc^2))
    lmBirdWindDirCircROSTF <- lm(newROSTF$Birds ~ newROSTF$WindDirCirc + I(newROSTF$WindDirCirc^2))
    
    if(input$variable == "WindDirCirc" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$WindDirCirc[order(na.omit(newCOTEM$WindDirCirc))]), 
      y=fitted(lmBirdWindDirCircCOTEM)[order(na.omit(newCOTEM$WindDirCirc))], col="blue")}
    if(input$variable == "WindDirCirc" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$WindDirCirc[order(na.omit(newCOTEF$WindDirCirc))]), 
      y=fitted(lmBirdWindDirCircCOTEF)[order(na.omit(newCOTEF$WindDirCirc))], col="blue")}
    if(input$variable == "WindDirCirc" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$WindDirCirc[order(na.omit(newROSTM$WindDirCirc))]), 
      y=fitted(lmBirdWindDirCircROSTM)[order(na.omit(newROSTM$WindDirCirc))], col="blue")}
    if(input$variable == "WindDirCirc" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$WindDirCirc[order(na.omit(newROSTF$WindDirCirc))]), 
      y=fitted(lmBirdWindDirCircROSTF)[order(na.omit(newROSTF$WindDirCirc))], col="blue")}
    
    #ChkAgeMean
    
    lmBirdChkAgeMeanCOTEM <- lm(newCOTEMChick$Birds ~ newCOTEMChick$ChkAgeMean + I(newCOTEMChick$ChkAgeMean^2))
    lmBirdChkAgeMeanCOTEF <- lm(newCOTEFChick$Birds ~ newCOTEFChick$ChkAgeMean + I(newCOTEFChick$ChkAgeMean^2))
    lmBirdChkAgeMeanROSTM <- lm(newROSTMChick$Birds ~ newROSTMChick$ChkAgeMean + I(newROSTMChick$ChkAgeMean^2))
    lmBirdChkAgeMeanROSTF <- lm(newROSTFChick$Birds ~ newROSTFChick$ChkAgeMean)
    
    if(input$variable == "ChkAgeMean" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEMChick$ChkAgeMean[order(na.omit(newCOTEMChick$ChkAgeMean))]), 
      y=fitted(lmBirdChkAgeMeanCOTEM)[order(na.omit(newCOTEMChick$ChkAgeMean))], col="blue")}
    if(input$variable == "ChkAgeMean" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEFChick$ChkAgeMean[order(na.omit(newCOTEFChick$ChkAgeMean))]), 
      y=fitted(lmBirdChkAgeMeanCOTEF)[order(na.omit(newCOTEFChick$ChkAgeMean))], col="blue")}
    if(input$variable == "ChkAgeMean" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTMChick$ChkAgeMean[order(na.omit(newROSTMChick$ChkAgeMean))]), 
      y=fitted(lmBirdChkAgeMeanROSTM)[order(na.omit(newROSTMChick$ChkAgeMean))], col="blue")}
    if(input$variable == "ChkAgeMean" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTFChick$ChkAgeMean[order(na.omit(newROSTFChick$ChkAgeMean))]), 
      y=fitted(lmBirdChkAgeMeanROSTF)[order(na.omit(newROSTFChick$ChkAgeMean))], col="blue")}
    
    #Precip
    
    lmBirdPrecipCOTEM <- lm(newCOTEM$Birds ~ newCOTEM$Precip)
    lmBirdPrecipCOTEF <- lm(newCOTEF$Birds ~ newCOTEF$Precip + I(newCOTEF$Precip^2))
    lmBirdPrecipROSTM <- lm(newROSTM$Birds ~ newROSTM$Precip + I(newROSTM$Precip^2))
    lmBirdPrecipROSTF <- lm(newROSTF$Birds ~ newROSTF$Precip + I(newROSTF$Precip^2))
    
    if(input$variable == "Precip" & input$sexInput == "M" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEM$Precip[order(na.omit(newCOTEM$Precip))]), 
      y=fitted(lmBirdPrecipCOTEM)[order(na.omit(newCOTEM$Precip))], col="blue")}
    if(input$variable == "Precip" & input$sexInput == "F" & input$speciesInput == "COTE")
    {lines(
      x=na.omit(newCOTEF$Precip[order(na.omit(newCOTEF$Precip))]), 
      y=fitted(lmBirdPrecipCOTEF)[order(na.omit(newCOTEF$Precip))], col="blue")}
    if(input$variable == "Precip" & input$sexInput == "M" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTM$Precip[order(na.omit(newROSTM$Precip))]), 
      y=fitted(lmBirdPrecipROSTM)[order(na.omit(newROSTM$Precip))], col="blue")}
    if(input$variable == "Precip" & input$sexInput == "F" & input$speciesInput == "ROST")
    {lines(
      x=na.omit(newROSTF$Precip[order(na.omit(newROSTF$Precip))]), 
      y=fitted(lmBirdPrecipROSTF)[order(na.omit(newROSTF$Precip))], col="blue")}
    
    
    
    
    
    output$stats <- renderPrint({
     
      filteredMean <- new$Birds[new$sex == input$sexInput & new$spp == input$speciesInput]
      
      if(input$sexInput == "M" & input$speciesInput == "COTE"){
      cat("An average of ", round(mean(filteredMean, na.rm=TRUE), 2), " male common terns were detected per day.", "

")
        cat("A maximum of  9  unique male common terns were detected on July 3rd.")}
      
      if(input$sexInput == "F" & input$speciesInput == "COTE"){
        cat("An average of ", round(mean(filteredMean, na.rm=TRUE), 2), " female common terns were detected per day.", "

")
        cat("A maximum of  9  unique female common terns were detected on July 3rd.")}
      
      if(input$sexInput == "M" & input$speciesInput == "ROST"){
        cat("An average of ", round(mean(filteredMean, na.rm=TRUE), 2), " male roseate terns were detected per day.", "

")
        cat("A maximum of  3  unique male roseate terns were detected on June 29th.")}
      
      if(input$sexInput == "F" & input$speciesInput == "ROST"){
        cat("An average of ", round(mean(filteredMean, na.rm=TRUE), 2), " female reseate terns were detected per day.", "

")
        cat("A maximum of  8  unique female roseate terns were detected on July 3rd.")
        }
    })
    
    
    
    
    
    
    output$lineinfo <- renderPrint({
      
      #TempF
      
      if(input$variable == "TempF" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdTempFCOTEM)[3],2), "x^2 + ",
            round(coef(lmBirdTempFCOTEM)[2],2), "x ",
            round(coef(lmBirdTempFCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdTempFCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "TempF" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdTempFCOTEF)[3],2), "x^2 + ",
            round(coef(lmBirdTempFCOTEF)[2],2), "x ",
            round(coef(lmBirdTempFCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdTempFCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "TempF" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdTempFROSTM)[3],2), "x^2 + ",
            round(coef(lmBirdTempFROSTM)[2],2), "x ",
            round(coef(lmBirdTempFROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdTempFROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "TempF" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdTempFROSTF)[3],2), "x^2 + ",
            round(coef(lmBirdTempFROSTF)[2],2), "x ",
            round(coef(lmBirdTempFROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdTempFROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      
      #VisM
      
      
      if(input$variable == "VisM" & input$sexInput == "M" & input$speciesInput == "COTE") {
        cat("The equation is: y = ", 
            round(coef(lmBirdVisMCOTEM)[2],2), "x ",
            round(coef(lmBirdVisMCOTEM)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdVisMCOTEM)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "<0.001")}   
      if(input$variable == "VisM" & input$sexInput == "F" & input$speciesInput == "COTE") {
        cat("The equation is: y = ", 
            round(coef(lmBirdVisMCOTEF)[2],2), "x ",
            round(coef(lmBirdVisMCOTEF)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdVisMCOTEF)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "<0.001")}   
      if(input$variable == "VisM" & input$sexInput == "M" & input$speciesInput == "ROST") {
        cat("The equation is: y = ", 
            round(coef(lmBirdVisMROSTM)[2],3), "x + ",
            round(coef(lmBirdVisMROSTM)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdVisMROSTM)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "0.669")}    
      if(input$variable == "VisM" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdVisMROSTF)[3],2), "x^2 + ",
            round(coef(lmBirdVisMROSTF)[2],2), "x ",
            round(coef(lmBirdVisMROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdVisMROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.00112")}
      
      #DewPtF
      
      if(input$variable == "DewPtF" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdDewPtFCOTEM)[3],2), "x^2 + ",
            round(coef(lmBirdDewPtFCOTEM)[2],2), "x ",
            round(coef(lmBirdDewPtFCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdDewPtFCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "DewPtF" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdDewPtFCOTEF)[3],2), "x^2 + ",
            round(coef(lmBirdDewPtFCOTEF)[2],2), "x ",
            round(coef(lmBirdDewPtFCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdDewPtFCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "DewPtF" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdDewPtFROSTM)[3],3), "x^2 + ",
            round(coef(lmBirdDewPtFROSTM)[2],2), "x ",
            round(coef(lmBirdDewPtFROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdDewPtFROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.0375")}
      if(input$variable == "DewPtF" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdDewPtFROSTF)[3],3), "x^2 + ",
            round(coef(lmBirdDewPtFROSTF)[2],2), "x ",
            round(coef(lmBirdDewPtFROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdDewPtFROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.0270")}
      
      #Humid
      
      if(input$variable == "Humid" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdHumidCOTEM)[3],3), "x^2 + ",
            round(coef(lmBirdHumidCOTEM)[2],2), "x ",
            round(coef(lmBirdHumidCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdHumidCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "Humid" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdHumidCOTEF)[3],2), "x^2 + ",
            round(coef(lmBirdHumidCOTEF)[2],2), "x + ",
            round(coef(lmBirdHumidCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdHumidCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "Humid" & input$sexInput == "M" & input$speciesInput == "ROST") {
        cat("The equation is: y = ", 
            round(coef(lmBirdHumidROSTM)[2],3), "x + ",
            round(coef(lmBirdHumidROSTM)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdHumidROSTM)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "0.216")}    
      if(input$variable == "Humid" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdHumidROSTF)[3],3), "x^2 + ",
            round(coef(lmBirdHumidROSTF)[2],2), "x ",
            round(coef(lmBirdHumidROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdHumidROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      #SeaLvlPrs
      
      if(input$variable == "SeaLvlPrs" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdSeaLvlPrsCOTEM)[3],2), "x^2 + ",
            round(coef(lmBirdSeaLvlPrsCOTEM)[2],2), "x ",
            round(coef(lmBirdSeaLvlPrsCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdSeaLvlPrsCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "SeaLvlPrs" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdSeaLvlPrsCOTEF)[3],2), "x^2 + ",
            round(coef(lmBirdSeaLvlPrsCOTEF)[2],2), "x ",
            round(coef(lmBirdSeaLvlPrsCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdSeaLvlPrsCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "SeaLvlPrs" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdSeaLvlPrsROSTM)[3],2), "x^2 + ",
            round(coef(lmBirdSeaLvlPrsROSTM)[2],2), "x ",
            round(coef(lmBirdSeaLvlPrsROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdSeaLvlPrsROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "SeaLvlPrs" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdSeaLvlPrsROSTF)[3],2), "x^2 + ",
            round(coef(lmBirdSeaLvlPrsROSTF)[2],2), "x ",
            round(coef(lmBirdSeaLvlPrsROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdSeaLvlPrsROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      #WindSpd
      
      if(input$variable == "WindSpd" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindSpdCOTEM)[3],2), "x^2 + ",
            round(coef(lmBirdWindSpdCOTEM)[2],2), "x ",
            round(coef(lmBirdWindSpdCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindSpdCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "WindSpd" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindSpdCOTEF)[3],2), "x^2 + ",
            round(coef(lmBirdWindSpdCOTEF)[2],2), "x ",
            round(coef(lmBirdWindSpdCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindSpdCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "WindSpd" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindSpdROSTM)[3],2), "x^2 + ",
            round(coef(lmBirdWindSpdROSTM)[2],2), "x ",
            round(coef(lmBirdWindSpdROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindSpdROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "WindSpd" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindSpdROSTF)[3],2), "x^2 + ",
            round(coef(lmBirdWindSpdROSTF)[2],2), "x ",
            round(coef(lmBirdWindSpdROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindSpdROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      #WindDirCirc
      
      if(input$variable == "WindDirCirc" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindDirCircCOTEM)[3],4), "x^2 + ",
            round(coef(lmBirdWindDirCircCOTEM)[2],2), "x ",
            round(coef(lmBirdWindDirCircCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindDirCircCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "WindDirCirc" & input$sexInput == "F" & input$speciesInput == "COTE") {
        cat("The equation is: y = ", 
            round(coef(lmBirdWindDirCircCOTEF)[2],3), "x ",
            round(coef(lmBirdWindDirCircCOTEF)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdWindDirCircCOTEF)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "<0.001")}   
      if(input$variable == "WindDirCirc" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindDirCircROSTM)[3],5), "x^2 + ",
            round(coef(lmBirdWindDirCircROSTM)[2],3), "x ",
            round(coef(lmBirdWindDirCircROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindDirCircROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "WindDirCirc" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdWindDirCircROSTF)[3],5), "x^2 + ",
            round(coef(lmBirdWindDirCircROSTF)[2],2), "x ",
            round(coef(lmBirdWindDirCircROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdWindDirCircROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      
      #ChkAgeMean
      
      if(input$variable == "ChkAgeMean" & input$sexInput == "M" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdChkAgeMeanCOTEM)[3],3), "x^2 + ",
            round(coef(lmBirdChkAgeMeanCOTEM)[2],2), "x ",
            round(coef(lmBirdChkAgeMeanCOTEM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdChkAgeMeanCOTEM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "ChkAgeMean" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdChkAgeMeanCOTEF)[3],3), "x^2 + ",
            round(coef(lmBirdChkAgeMeanCOTEF)[2],2), "x ",
            round(coef(lmBirdChkAgeMeanCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdChkAgeMeanCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.00177")}
      if(input$variable == "ChkAgeMean" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdChkAgeMeanROSTM)[3],2), "x^2 + ",
            round(coef(lmBirdChkAgeMeanROSTM)[2],2), "x ",
            round(coef(lmBirdChkAgeMeanROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdChkAgeMeanROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "<0.001")}
      if(input$variable == "ChkAgeMean" & input$sexInput == "F" & input$speciesInput == "ROST") {
        cat("The equation is: y = ", 
            round(coef(lmBirdChkAgeMeanROSTF)[2],3), "x + ",
            round(coef(lmBirdChkAgeMeanROSTF)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdChkAgeMeanROSTF)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "<0.001")}  
      
      #Precip
      
      if(input$variable == "Precip" & input$sexInput == "M" & input$speciesInput == "COTE") {
        cat("The equation is: y = ", 
            round(coef(lmBirdPrecipCOTEM)[2],2), "x + ",
            round(coef(lmBirdPrecipCOTEM)[1],2), sep = "","
            
") 
        cat("This model explains ", 
            round(summary(lmBirdPrecipCOTEM)$r.squared*100,2), 
            "% of the variance", "
            
")
        cat("The p-value is:", "0.615")} 
      if(input$variable == "Precip" & input$sexInput == "F" & input$speciesInput == "COTE") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdPrecipCOTEF)[3],2), "x^2 ",
            round(coef(lmBirdPrecipCOTEF)[2],2), "x + ",
            round(coef(lmBirdPrecipCOTEF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdPrecipCOTEF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.00643")}
      if(input$variable == "Precip" & input$sexInput == "M" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdPrecipROSTM)[3],2), "x^2 ",
            round(coef(lmBirdPrecipROSTM)[2],2), "x + ",
            round(coef(lmBirdPrecipROSTM)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdPrecipROSTM)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.0141")}
      if(input$variable == "Precip" & input$sexInput == "F" & input$speciesInput == "ROST") {
        
        cat("The equation is: y = ",
            round(coef(lmBirdPrecipROSTF)[3],2), "x^2 + ",
            round(coef(lmBirdPrecipROSTF)[2],2), "x + ",
            round(coef(lmBirdPrecipROSTF)[1],2), sep = "", "
            
")
        cat("This model explains ",
            round(summary(lmBirdPrecipROSTF)$r.squared*100,2),
            "% of the variance", "
            
")
        
        
        cat("The p-value is:", "0.00231")}
      
      
    })
      })
  

  

  
  
  
  
  a<-c(my.p1, my.p7a, my.p7b, my.p2, my.p3, my.p4, my.p5, my.p6, my.p7)
  b<-c(r1, r7a, r7b, r2,r3,r4,r5,r6,r7)
  c<-c(bf1, bf7a, bf7b, bf2, bf3, bf4, bf5, bf6, bf7)
  
  trial <- matrix(c(a,b,c), ncol=3, nrow=9)
  rownames(trial) <- c("All Terns", "Female Terns", "Male Terns", "Visibility", "Temperature", "Dew Point", "Humidity", "Sea Level Pressure",
                       "Wind Speed")
  colnames(trial) <- c("p-Value   ", "% of variance explained by model   ", "Model Equation")
  trial.table <- as.table(trial)
  
  output$summary1<-renderPrint({
    trial.table
  })
  
  l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "",
    bordercolor = "#FFFFFF",
    borderwidth = 2)
  
  a2<-c(my.p8, my.p14a, my.p14b, my.p9, my.p10, my.p11, my.p12, my.p13, my.p14)
  b2<-c(r8, r14a, r14b, r9,r10,r11,r12,r13,r14)
  c2<-c(bf8, bf14a, bf14b, bf9, bf10, bf11, bf12, bf13, bf14)
  trial2 <- matrix(c(a2,b2, c2), ncol=3, nrow=9)
  rownames(trial2) <- c("All Roseate Terns", "Female Roseate Terns", "Male Roseate Terns", "Visibility", "Temperature", "Dew Point", "Humidity", "Sea Level Pressure","Wind Speed")
  colnames(trial2) <- c("p-Value   ", "% of variance explained by model   ", "Model Equation")
  trial2.table <- as.table(trial2)
  
  output$summary2<-renderPrint({
    trial2.table
  })
  
  a3<-c(my.p15, my.p21a, my.p21b, my.p16, my.p17, my.p18, my.p19, my.p20, my.p21)
  b3<-c(r15, r21a, r21b, r16,r17,r18,r19,r20,r21)
  c3<-c(bf15, bf21a, bf21b, bf16, bf17, bf18, bf19, bf20, bf21)
  trial3 <- matrix(c(a3,b3, c3), ncol=3, nrow=9)
  rownames(trial3) <- c("All Common Terns", "Female Common Terns", "Male Common Terns", "Visibility", "Temperature", "Dew Point", "Humidity", "Sea Level Pressure","Wind Speed")
  colnames(trial3) <- c("p-Value   ", "% of variance explained by model   ", "Model Equation")
  trial3.table <- as.table(trial3)
  
  
  output$summary3<-renderPrint({
    trial3.table
  })
  
  
  
  output$plot <- renderPlotly({
    
    x <- list(
      title = "")
    y <- list(
      title = "Bird Detections", tickfont = list(color = "blue"))
    
    
    rightaxis <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Weather Variables")
    
    AllFemale <- newROSTF$Birds + newCOTEF$Birds
    AllMale <- newROSTM$Birds + newCOTEM$Birds
    
    
    p<- plot_ly() %>% 
      
      add_trace(data=newAll, x = ~newAll$DailyDate,
                y = ~newAll$Birds, type="scatter", mode="markers", name="All Terns", marker=list(color="blue")) %>%
      
      add_trace(x = ~newROSTF$DailyDate,
                y = ~AllFemale, type="scatter", mode="markers", name="Female Terns", marker=list(color="#130")) %>%
      
      add_trace(x = ~newROSTM$DailyDate,
                y = ~AllMale, type="scatter", mode="markers", marker=list(color="cyan"),
                name="Male Terns") %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$VisM, type="scatter", mode = "lines", name="Visibility (Miles)", 
                yaxis = "y2", lines=list(color="green")) %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$TempF, type="scatter", mode = "lines", name="Temperature (F)", 
                yaxis = "y2", lines=list(color="red")) %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$DewPtF, type="scatter", mode = "lines", name="Dew Point (F)", 
                yaxis = "y2", lines=list(color="purple")) %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$Humid, type="scatter", mode = "lines", name="Humidity (%)", 
                yaxis = "y2", lines=list(color="brown")) %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$SeaLvlPrs, type="scatter", mode = "lines", name="Sea Level Pressure (Inches Hg)", 
                yaxis = "y2") %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$WindSpd, type="scatter", mode = "lines", name="Wind Speed (MPH)", 
                yaxis = "y2") %>%
      
      add_trace(data=newM, x = ~newM$DailyDate,
                y = ~newM$Precip, type="scatter", mode = "lines", name="Precipitation (Inches)", 
                yaxis = "y2") %>%
      
      
      layout(
        xaxis=x,
        yaxis=y,
        yaxis2 = rightaxis,
        legend = list(x = 1.1, y = 1.0))
    
    
    
  })
  
  output$plot1 <- renderPlotly({
    
    #Name axes
    
    x <- list(
      title = "")
    y <- list(
      title = "Bird Detections", tickfont = list(color = "blue"))
    
    rightaxis <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Weather Variables")
    
    AllRoseate <- newROSTF$Birds + newROSTM$Birds
    
    r<- plot_ly() %>% 
      
      add_trace(x = ~newROSTF$DailyDate,
                y = ~AllRoseate, type="scatter", mode="markers", 
                name= "All Roseate Terns", marker=list(color="Blue")) %>%
      
      
      add_trace(data=newROSTF, x = ~newROSTF$DailyDate,
                y = ~newROSTF$Birds, type="scatter", mode="markers", 
                name= "Female Roseate Terns", marker=list(color="#130")) %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$Birds, type="scatter", mode="markers", marker=list(color="cyan"),
                name= "Male Roseate Terns") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$VisM, type="scatter", mode = "lines", name="Visibility (Miles)", yaxis="y2") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$TempF, type="scatter", mode = "lines", name="Temperature (F)", yaxis="y2") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$DewPtF, type="scatter", mode = "lines", name="Dew Point (F)", yaxis="y2") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$Humid, type="scatter", mode = "lines", name= "Humidity (%)", yaxis="y2") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$SeaLvlPrs, type="scatter", mode = "lines", 
                name= "Sea Level Pressure (Inches Hg)", col="yellow", yaxis="y2") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$WindSpd, type="scatter", mode = "lines", name= "Wind Speed (MPH)", yaxis="y2") %>%
      
      add_trace(data=newROSTM, x = ~newROSTM$DailyDate,
                y = ~newROSTM$Precip, type="scatter", mode = "lines", name="Precipitation (Inches)", 
                yaxis = "y2") %>%
      
      layout(
        xaxis=x,
        yaxis=y,
        yaxis2 = rightaxis,
        legend = list(x = 1.1, y = 1.0))
    
    
  })
  
  output$plot2 <- renderPlotly({
    
    #Name axes
    
    x <- list(
      title = "")
    y <- list(
      title = "Bird Detections", tickfont = list(color = "blue"))
    
    rightaxis <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Weather Variables")
    
    AllCommon <- newCOTEF$Birds + newCOTEM$Birds
    
    
    #Specify data for graph.
    r<- plot_ly() %>% 
      
      ###COTE(F)
      add_trace(x = ~newCOTEF$DailyDate,
                y = ~AllCommon, type="scatter", mode="markers", marker=list(color="Blue"),
                name= "All Common Terns") %>%

      
      add_trace(data=newCOTEF, x = ~newCOTEF$DailyDate,
                y = ~newCOTEF$Birds, type="scatter", mode="markers", marker=list(color="#130"),
                name= "Female Common Terns") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$Birds, type="scatter", mode="markers", marker=list(color="cyan"),
                name= "Male Common Terns") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$VisM, type="scatter", mode = "lines", name= "Visibility (Miles)", yaxis="y2") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$TempF, type="scatter", mode = "lines", name= "Temperature (F)", yaxis="y2") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$DewPtF, type="scatter", mode = "lines", name= "Dew Point (F)", yaxis="y2") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$Humid, type="scatter", mode = "lines", name= "Humidity (%)", yaxis="y2") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$SeaLvlPrs, type="scatter", mode = "lines", 
                name= "Sea Level Pressure (Inches Hg)", col="yellow", yaxis="y2") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$WindSpd, type="scatter", mode = "lines", name= "Wind Speed (MPH)", yaxis="y2") %>%
      
      add_trace(data=newCOTEM, x = ~newCOTEM$DailyDate,
                y = ~newCOTEM$Precip, type="scatter", mode = "lines", name="Precipitation (Inches)", 
                yaxis = "y2") %>%
      
      layout(
        xaxis=x,
        yaxis=y,
        yaxis2 = rightaxis,
        legend = list(x = 1.1, y = 1.0))
    
    
    
  })
  
  
  output$plot3 <- renderPlotly({
    
    #Name axes
    
    x <- list(
      title = "")
    y <- list(
      title = "Bird Detections", tickfont = list(color = "blue"))
    
    rightaxis <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Weather Variables")
    
    AllHrFemale <- newROSTF$HrBirds + newCOTEF$HrBirds
    AllHrMale <- newROSTM$HrBirds + newCOTEM$HrBirds
    AllHrTerns <- AllHrFemale + AllHrMale
    
    
    r<- plot_ly() %>% 
      
      ###COTE(F)
      
      add_trace(x = ~newROSTF$tsHr,
                y = ~AllHrTerns, type="scatter", mode="markers", marker=list(color="blue"),
                name= "All Terns") %>%
      
      add_trace(x = ~newROSTF$tsHr,
                y = ~AllHrFemale, type="scatter", mode="markers", marker=list(color="#130"),
                name= "Female Terns") %>%
      
      add_trace(data=newM, x = ~newROSTM$tsHr,
                y = ~AllHrMale, type="scatter", mode="markers", marker=list(color="cyan"),
                name= "Male Terns") %>%
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrVisM, type="scatter", mode = "lines", name= "Visibility (Miles)", yaxis="y2") %>%
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrTempF, type="scatter", mode = "lines", name= "Temperature (F)", yaxis="y2") %>%
      
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrDewPtF, type="scatter", mode = "lines", name= "Dew Point (F)", yaxis="y2") %>%
      
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrHumid, type="scatter", mode = "lines", name= "Humidity (%)", yaxis="y2") %>%
      
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrSeaLvlPrs, type="scatter", mode = "lines", 
                name= "Sea Level Pressure (Inches Hg)", col="yellow", yaxis="y2") %>%
      
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrWindSpd, type="scatter", mode = "lines", name= "Wind Speed (MPH)", yaxis="y2") %>%
      
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrPrecip, type="scatter", mode = "lines", name= "Precipitation (Inches)", yaxis="y2") %>%
      
      
      add_trace(data=newM, x = ~newM$tsHr,
                y = ~newM$HrTide, type="scatter", mode = "lines", name= "Tidal Height (Meters)", yaxis="y2") %>%
      
      
      layout(
        xaxis=x,
        yaxis=y,
        yaxis2 = rightaxis,
        legend = list(x = 1.1, y = 1.0))
    
    
    
  })
  
  
  
  
  
      }

shinyApp(ui, server)

###Next Step: Add Regression Line




