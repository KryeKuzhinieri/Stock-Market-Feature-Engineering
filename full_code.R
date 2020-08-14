library(data.table)

DB = read.csv(file.choose(), header = TRUE)
attach(DB)

vol1 = c()
vol2 = c()
vol1  = as.numeric(gsub("M", "",DB[,6]))*1000000
vol2 = as.numeric(gsub("K", "",DB[,6]))*1000
vol1[is.na(vol1)] <- 0
vol2[is.na(vol2)] <- 0
Volume = vol1+vol2
names(DB) = c("Date", "Price", "Open", "High", "Low", "Vol","Change")
DB$Vol = Volume

#k
sv= c()
StochasticK <- function(i, j) {
  while(j<=length(DB$High)) {
    hh = (max(DB$High[i:j]))
    ll = (min(DB$Low[i:j]))
    p = DB$Price[j]
    i = i + 1
    j = j + 1
    stockk = ((p-ll)/(hh-ll))*100
    sv = c(sv, stockk)
  }
  return(sv)
  
}
sv = StochasticK(1,7)
DB$StochasticK[1:6] <- c()
DB$StochasticK[7:length(Price)] <- sv[1:(length(Price)-6)]

#D

sv= c()
StochasticD <- function(i, j) {
  while(j<=length(DB$StochasticK)) {
    stockd = (sum(DB$StochasticK[i:j]))/7
    sv = c(sv, stockd)
    i = i + 1
    j = j + 1
  }
  return(sv)
}
sv = StochasticD(7,13)
DB$StochasticD[1:12] <- c()
DB$StochasticD[13:length(Price)] <- sv[1:(length(Price)-12)]

#S

sv= c()
StochasticS <- function(i, j) {
  while(j<=length(DB$StochasticD)) {
    stocks = (sum(DB$StochasticD[i:j]))/7
    sv = c(sv, stocks)
    i = i + 1
    j = j + 1
  }
  return(sv)
}
sv = StochasticS(13,19)
DB$StochasticS[1:18] <- c()
DB$StochasticS[19:length(Price)] <- sv[1:(length(Price)-18)]

#Momentum

mom = Price - shift(Price, n=4, type ="lag")
DB$Momentum = mom

#ROC

ROC = Price/((shift(Price, n=7, type ="lag"))*100)
DB$ROC = ROC

#Larry William

sv= c()
lw <- function(i, j) {
  while(j<=length(DB$High)) {
    hh = (max(DB$High[i:j]))
    ll = (min(DB$Low[i:j]))
    p = DB$Price[j]
    i = i + 1
    j = j + 1
    stockk = ((hh-p)/(hh-ll))*100
    sv = c(sv, stockk)
  }
  return(sv)
  
}
sv = lw(1,7)
DB$LarryWilliams[1:6] <- c()
DB$LarryWilliams[7:length(Price)] <- sv[1:(length(Price)-6)]

#A/O Oscillator

AO = (High - (shift(Price, n = 1, type = 'lag')))/(High - Low)
DB$Oscillator = AO

#Disparity in 5 days

sv= c()
d5 <- function(i, j) {
  while(j<=length(DB$High)) {
    p = DB$Price[j]
    ma5 = (sum(Price[i:j]))/5
    i = i + 1
    j = j + 1
    dis5 = (p/ma5)*100
    sv = c(sv, dis5)
  }
  return(sv)
  
}
sv = d5(1,5)
DB$Disparityin5[1:4] <- c()
DB$Disparityin5[5:length(Price)] <- sv[1:(length(Price)-4)]

#Disparity in 10 days

sv= c()
d10 <- function(i, j) {
  while(j<=length(DB$High)) {
    p = DB$Price[j]
    ma10 = (sum(Price[i:j]))/10
    i = i + 1
    j = j + 1
    dis10 = (p/ma10)*100
    sv = c(sv, dis10)
  }
  return(sv)
  
}
sv = d10(1,10)
DB$Disparityin10[1:9] <- c()
DB$Disparityin10[10:length(Price)] <- sv[1:(length(Price)-9)]

#moving average 5

moving5=c()
sv= c()
mov5 <- function(i, j) {
  while(j<=length(DB$High)) {
    ma5 = (sum(Price[i:j]))/5
    i = i + 1
    j = j + 1
    sv = c(sv, ma5)
  }
  return(sv)
  
}
sv = mov5(1,5)
moving5[1:4] <- c()
moving5[5:length(Price)] <- sv[1:(length(Price)-4)]

#moving average 10

moving10=c()
sv= c()
mov10 <- function(i, j) {
  while(j<=length(DB$High)) {
    ma10 = (sum(Price[i:j]))/10
    i = i + 1
    j = j + 1
    sv = c(sv, ma10)
  }
  return(sv)
  
}
sv = mov10(1,10)
moving10[1:9] <- c()
moving10[10:length(Price)] <- sv[1:(length(Price)-9)]

#OSCP(Price oscillator)

OSCP = moving5-(moving10/moving5)
DB$OSCP = OSCP

#CCI(commodity channel index)

#Mt
Mt = (High+Low+Price)/3

#SMt
SMt=c()
sv= c()
SM <- function(i, j) {
  while(j<=length(Mt)) {
    ma = (sum(Mt[i:j]))/5
    i = i + 1
    j = j + 1
    sv = c(sv, ma)
  }
  return(sv)
  
}
sv = SM(1,5)
SMt[1:4] <- c()
SMt[5:length(Price)] <- sv[1:(length(Price)-4)]

#Dt

Dt = c()
sv= c()
Dtf <- function(i, j) {
  while(j<=length(Mt)) {
    D = sum(abs((Mt[i:j])-(SMt[i:j])))/5
    i = i + 1
    j = j + 1
    sv = c(sv, D)
  }
  return(sv)
  
}
sv = Dtf(1,5)
Dt[1:4] <- c()
Dt[5:length(Price)] <- sv[1:(length(Price)-4)]

#CCI(Commodity Channel Index)

cci = (Mt-SMt)/(0.015*Dt)
DB$CCI = cci

#RSI(Relative Strength Index)

#Positive change

Upt= c()
sv = c(0)
Uptf = function(i){
  while (i<=length(Price)) {
    change = Price[i] - Price[i-1]
    if(change>0){
      sv = c(sv, change)
    }
    else{
      sv = c(sv, 0)
    }
    i = i+1
  }
  return(sv)
}

Upt = Uptf(2)

#Negative change

Dwt= c()
sv = c(0)
Dwtf = function(i){
  while (i<=length(Price)) {
    change = Price[i] - Price[i-1]
    if(change<0){
      sv = c(sv, change)
    }
    else{
      sv = c(sv, 0)
    }
    i = i+1
  }
  return(sv)
}

Dwt = Dwtf(2)

#Average Upt

avgUpt = c()
sv = c()
avgUptf = function(i, j, n){
  while (j<=(length(Price)-1)) {
    avg = (sum(Upt[i:j]))/n
    i=i+1
    j=j+1
    sv = c(sv, avg)
  }
  return(sv)
}

avgUpt[1:5] = c()
avgUpt[6:length(Price)] = avgUptf(1,5,5)


#Average Dwt

avgDwt = c()
sv = c()
avgDwtf = function(i, j, n){
  while (j<=(length(Price)-1)) {
    avg = (sum(Dwt[i:j]))/n
    i=i+1
    j=j+1
    sv = c(sv, avg)
  }
  return(sv)
}

avgDwt[1:5] = c()
avgDwt[6:length(Price)] = avgDwtf(1,5,5)

#RSI(Relative Strength Index)

rsi = 100-(100/(1+(avgUpt/abs(avgDwt))))
DB$RSI = rsi

#Direction

dirf = c()

for (value in Upt){
  if (value >0){
    dirf = c(dirf, 1)
  }
  else{
    dirf = c(dirf, 0)
  }
  
}

DB$Direction = dirf

DB$Price = NULL
DB$Date = NULL
DB$Open = NULL
DB$Low = NULL
DB$High = NULL
DB$Change = NULL

DB$Direction = factor(DB$Direction, labels = c("Down","Up"))

DB=na.omit(DB)




write.csv(DB,"clean_APPLE.csv",row.names = FALSE)
