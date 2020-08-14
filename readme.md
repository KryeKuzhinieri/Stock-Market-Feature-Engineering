These features were enginered with the help of my friend **Alixsander Haj Saw**


# Introduction

As with many machine learning projects, data preprocessing is an important step in *stock market* price prediction. Many researchers have not used the features provided by the Stock Exchanges due to their simplicity. However, while consulting experts of the field, they have created new features that provide better results. In this tutorial, we will be calculating the features used by **Qiu and Song (2016)** in their paper *"Predicting the Direction of Stock Market Index Movement Using an Optimized Artificial Neural Network Model".*

# Required Libraries 
The following libraries will be used throughout this tutorial.



```r
library(data.table)
options(warn=-1)
```


# Feature Engineering

The dataset that will be used is **AAPL**  *(Apple's stock price values)*. 
For simplification reasons, I will read the data from my PC. However, you could load it without having to donwload it. 

### Reading Dataset


```r
# Reading the dataset
DB = read.csv(file.choose(), header = TRUE)

attach(DB)

#Changing the column names to all known names.
names(DB) = c("Date", "Price", "Open", "High", "Low", "Vol","Change")


#Converting the volume from character to numeric. Ie: 24.63M to 24 000 000.
vol1 = c()
vol2 = c()
vol1  = as.numeric(gsub("M", "",DB[,6]))*1000000 # For Million Values
vol2 = as.numeric(gsub("K", "",DB[,6]))*1000     # For thousands
vol1[is.na(vol1)] <- 0
vol2[is.na(vol2)] <- 0
Volume = vol1+vol2
DB$Vol = Volume

head(DB)
```

```
##           Date  Price   Open   High    Low      Vol Change
## 1 Dec 19, 2019 280.02 279.60 281.17 278.95 24630000  0.10%
## 2 Dec 18, 2019 279.74 279.80 281.90 279.12 29020000 -0.24%
## 3 Dec 17, 2019 280.41 279.57 281.77 278.80 28580000  0.20%
## 4 Dec 16, 2019 279.86 277.00 280.79 276.98 32080000  1.71%
## 5 Dec 13, 2019 275.15 271.46 275.30 270.93 33430000  1.36%
## 6 Dec 12, 2019 271.46 267.78 272.56 267.32 34440000  0.25%
```

### Stockastic %K
* A stochastic oscillator is a momentum indicator comparing a particular closing price of a security to a range of its prices over a certain period of time. The sensitivity of the oscillator to market movements is reducible by adjusting that time period or by taking a moving average of the result. **(Hayes, 2020)**
* Formula: <img src="https://render.githubusercontent.com/render/math?math=StockasticK=\dfrac{C_{t}-L_{n}}{H_{n}+L_{n}}*100"> 


```r
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

cat("Some of the Stockastic K values are: ", tail(DB$StochasticK, 3))
```

```
## Some of the Stockastic K values are:  6.976744 27.18447 17.14286
```


### Stochastic %D
* Stochastic %D is the number of time periods used when calculating a moving average of %K. The moving average is called "%D". **(Steven B. Achelis)**
* Formula: $$ StochasticD=\sum_{i=0}^{n-1}\dfrac{StochasticK_{t-i}}{n} $$
<img src="https://render.githubusercontent.com/render/math?math=StochasticD=\sum_{i=0}^{n-1}\dfrac{StochasticK_{t-i}}{n}"> 


```r
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

cat("Some of the Stockastic D values are: ", tail(DB$StochasticD, 3))
```

```
## Some of the Stockastic D values are:  25.50609 23.32897 19.13344
```


### Stochastic Slow %D
* Similar to the relationship between Stochastic %K and %D, Stochastic Slow is a moving average of Stochastic %D.
* Formula: $$ StochasticSlow=\sum_{i=0}^{n-1}\dfrac{StochasticD_{t-i}}{n} $$ 
<img src="https://render.githubusercontent.com/render/math?math=StochasticSlow=\sum_{i=0}^{n-1}\dfrac{StochasticD_{t-i}}{n}">


```r
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

cat("Some of the Stockastic Slow D values are: ", tail(DB$StochasticS, 3))
```

```
## Some of the Stockastic Slow D values are:  40.29916 34.75473 29.48191
```


### Momentum
* Momentum is the speed or velocity of price changes in a stock, security, or tradable instrument. Momentum shows the rate of change in price movement over a period of time to help investors determine the strength of a trend. **(Investopedia Staff)**
* Formula: $$Momentum=C_{t}-C_{t-4}$$
<img src="https://render.githubusercontent.com/render/math?math=Momentum=C_{t}-C_{t-4}">


```r
mom = DB$Price - shift(DB$Price, n=4, type ="lag")
DB$Momentum = mom

cat("Some of the Momentum values are: ", tail(DB$Momentum, 3))
```

```
## Some of the Momentum values are:  -0.1 -0.23 -0.67
```


### Rate of Change

* The Rate-of-Change (ROC) indicator, which is also referred to as simply Momentum, is a pure momentum oscillator that measures the percent change in price from one period to the next. The ROC calculation compares the current price with the price “n” periods ago. **(Stockcharts)**
* Formula: $$ROC=\dfrac{C_{t}}{C_{t-n}}*100$$
<img src="https://render.githubusercontent.com/render/math?math=ROC=\dfrac{C_{t}}{C_{t-n}} *100}">

```r
ROC = Price/((shift(Price, n=7, type ="lag"))*100)
DB$ROC = ROC

cat("Some of the ROC values are: ", tail(DB$ROC, 3))
```

```
## Some of the ROC values are:  0.009346979 0.009152216 0.009071359
```


### Larry William's %R

* Williams %R, also known as the Williams Percent Range, is a type of momentum indicator that moves between 0 and -100 and measures overbought and oversold levels. The Williams %R may be used to find entry and exit points in the market. **(Mitchell, 2019)**
* Formula: $$ LW=\dfrac{H_{n}-C_{t}}{H_{n}-L_{n}}*100 $$
<img src="https://render.githubusercontent.com/render/math?math=LW=\dfrac{H_{n}-C_{t}}{H_{n}-L_{n}}*100">


```r
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

cat("Some of the LW values are: ", tail(DB$LarryWilliams, 3))
```

```
## Some of the LW values are:  93.02326 72.81553 82.85714
```

### A/O Oscillator

* Accumulation/distribution is a cumulative indicator that uses volume and price to assess whether a stock is being accumulated or distributed. The accumulation/distribution measure seeks to identify divergences between the stock price and volume flow. This provides insight into how strong a trend is. **(Mitchell, 2019)**
* Formula: $$AO=\dfrac{H_{t}-C_{t-1}}{H_{t}-L_{t}}$$
<img src="https://render.githubusercontent.com/render/math?math=AO=\dfrac{H_{t}-C_{t-1}}{H_{t}-L_{t}}">


```r
AO = (High - (shift(Price, n = 1, type = 'lag')))/(High - Low)
DB$Oscillator = AO

cat("Some of the AO values are: ", tail(DB$Oscillator, 3))
```

```
## Some of the AO values are:  -1.111111 -0.15625 -0.9047619
```


### Disparity in 5 and 10 days

* The disparity index is a technical indicator that measures the relative position of an asset's most recent closing price to a selected moving average and reports the value as a percentage.
A value greater than zero—a positive percentage—shows that the price is rising, suggesting that the asset is gaining upward momentum. Conversely, a value less than zero—a negative percentage—can be interpreted as a sign that selling pressure is increasing, forcing the price to drop. **(Segal, 2019)**
* Formula: $$ D5=\dfrac{C_{t}}{MA_{5or10}}*100$$
<img src="https://render.githubusercontent.com/render/math?math=D5=\dfrac{C_{t}}{MA_{5or10}}*100">


```r
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

cat("Some of the D5 values are: ", tail(DB$Disparityin5, 3))
```

```
## Some of the D5 values are:  98.11745 97.57601 96.20568
```

```r
cat("Some of the D10 values are: ", tail(DB$Disparityin10, 3))
```

```
## Some of the D10 values are:  95.06344 95.17131 94.19407
```


### Moving Average 5 and 10
* The moving average (MA) is a simple technical analysis tool that smooths out price data by creating a constantly updated average price. The average is taken over a specific period of time, like 10 days, 20 minutes, 30 weeks or any time period the trader chooses. **(Mitchell, 2020)**
* Formula: $$MA_{5or10}=\dfrac{\sum_{i=1}^{5or10}C_{t-1+i}}{5or10}$$
<img src="https://render.githubusercontent.com/render/math?math=MA_{5or10}=\dfrac{\sum_{i=1}^{5or10}C_{t-1+i}}{5or10}">


```r
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
```


### OSCP Price Oscillator
* To calculate it, we will use the moving averages calculated in the previous step.
* Formula: $$OSCP=\dfrac{MA_{5}-MA_{10}}{MA_{5}}$$
<img src="https://render.githubusercontent.com/render/math?math=OSCP=\dfrac{MA_{5}-MA_{10}}{MA_{5}}">


```r
OSCP = moving5-(moving10/moving5)
DB$OSCP = OSCP

cat("Some of the OSCP values are: ", tail(DB$OSCP, 3))
```

```
## Some of the OSCP values are:  8.741874 8.710733 8.624644
```

### Commodity Channel Index (CCI)
* The commodity channel index measures an asset value’s deviation from its statistical average. **(DayTrading)**
* Formula: $$CCI=\dfrac{M_{t}-SM_{t}}{0.015*D_{t}}$$
<img src="https://render.githubusercontent.com/render/math?math=CCI=\dfrac{M_{t}-SM_{t}}{0.015*D_{t}}">

```r
#Calculating the values one by one in the fromula above
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


cat("Some of the CCI values are: ", tail(DB$CCI, 3))
```

```
## Some of the CCI values are:  -62.37006 -128.3971 -135.968
```

### Relative Strength Index (RSI)
* The Relative Strength Index (RSI), developed by J. Welles Wilder, is a momentum oscillator that measures the speed and change of price movements. The RSI oscillates between zero and 100. Traditionally the RSI is considered overbought when above 70 and oversold when below 30. **(Fidelity)**
* Formula: $$ RSI=100-\dfrac{100}{\dfrac{1+\dfrac{\sum_{i=0}^{n-1}UP_{t-i}}{n}}{\dfrac{\sum_{i=0}^{n-1}DW_{t-1}}{n}}}$$
<img src="https://render.githubusercontent.com/render/math?math=RSI=100-\dfrac{100}{\dfrac{1+\dfrac{\sum_{i=0}^{n-1}UP_{t-i}}{n}}{\dfrac{\sum_{i=0}^{n-1}DW_{t-1}}{n}}}">

```r
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

cat("Some of the RSI values are: ", tail(DB$RSI, 3))
```

```
## Some of the RSI values are:  26.26263 22.41379 36.61972
```

### Direction Calculation and NA cleaning
* To calculate the direction, check the previous price. If lower than today's, then 0 "Down"; else "Up".
* Since we consider n periods, some of the columns will have NA due to their moving averages. Hence, we remove NAs.


```r
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


#write.csv(DB,"clean_APPLE.csv",row.names = FALSE)
```

### Final Dataset
* Our final dataset will look like the following:


```r
head(DB, 3)
```

```
##         Vol StochasticK StochasticD StochasticS Momentum        ROC
## 19 16330000    45.90301    60.70524    40.35099    -5.47 0.01000153
## 20 30350000    15.78947    59.91275    46.02475    -5.83 0.01009867
## 21 26610000    36.71053    57.51410    50.74690    -1.10 0.00996328
##    LarryWilliams Oscillator Disparityin5 Disparityin10     OSCP       CCI
## 19      54.09699 -1.3632479     98.59664      98.81586 264.5082 -99.77100
## 20      84.21053  0.7872340     99.07433      99.22855 263.4596 -67.53764
## 21      63.28947  0.7165493     99.87174      99.76574 262.5269 -12.92855
##         RSI Direction
## 19 74.67903      Down
## 20 41.43885        Up
## 21 26.26812        Up
```

# References

* *Qiu, Mingyue & Song, Yu. (2016). Predicting the Direction of Stock Market Index Movement Using an Optimized Artificial Neural Network Model. PLOS ONE. 11. e0155133. 10.1371/journal.pone.0155133.*
* *https://www.investopedia.com/terms/s/stochasticoscillator.asp*
* *https://www.metastock.com/customer/resources/taaz/?p=106*
* *https://www.investopedia.com/articles/technical/081501.asp* 
* *https://school.stockcharts.com/doku.php?id=technical_indicators:rate_of_change_roc_and_momentum*
* *https://www.investopedia.com/terms/w/williamsr.asp*
* *https://www.investopedia.com/terms/a/accumulationdistribution.asp*
* *https://www.investopedia.com/terms/d/disparityindex.asp*
* *https://www.investopedia.com/articles/active-trading/052014/how-use-moving-average-buy-stocks.asp*
* *https://www.daytrading.com/commodity-channel-index*
* *https://www.fidelity.com/learning-center/trading-investing/technical-analysis/technical-indicator-guide/RSI#*

# Happy Cooking!
