library(quantmod) 
library(PerformanceAnalytics)
library(TTR) 
library(knitr)
library(tidyverse)
library(strucchange)
library(TSstudio) 
library(forecast) 
library(tidyverse)

##STRUCUTRAL BREAK TESTING:
#retrieving the data- (Data downloaded from Yahoo finance)
nifty <- read.csv("nifty 50.csv")
view(nifty)
#converting it in a Time Series object -
nifty50 <- ts(nifty$Close, start = c(2007,1), end = c(2021,5), frequency = 365)

#visualisation -
plot.ts(nifty50, type = "l", main = "Nifty 50", sub = "Time series", 
        xlab = "Years", ylab = "Closing Price")


##Checking whether there are structural changes-
#SC test -
model1 <- Fstats(nifty50 ~ 1) #Chow test for structural changes
sctest(model1)
#p<0.05,hence we reject the null hypothesis:H0-No structural changes

#breakpoints -
bp.nifty50 <- breakpoints(nifty50 ~ 1) #Gets the breakpoints based on the F-stats
summary(bp.nifty50)
plot(bp.nifty50)

#plotting the breaks -
plot(nifty50, type = "l", main = "Nifty 50 Time Series", xlab = "Years")
lines(bp.nifty50)

## TECHNICAL ANALYSIS:
#Retrieivng the data using 'Quantmod'
getSymbols("^NSEI", src = "yahoo", from ="2020-01-01",to = "2021-05-31")
barChart(NSEI)
#Data cleaning -
NSEI =na.approx(NSEI)
View(NSEI)

#Trade indicators-
#MOVING AVERAGE -
#if the short-term 20-day moving average goes above the long-term 50-day moving average, 
#it indicates an upward price trend and generates a buy signal. 
#The opposite is true for a sell signal.
#SMA cyan = 50 days and red=20 days MA

#1-SMA -
#The standard interval of time we are going to use is 20 days SMA and 50 days SMA

sma20_nifty <- SMA(NSEI$NSEI.Close, n = 20)
sma50_nifty <- SMA(NSEI$NSEI.Close, n = 50)
lineChart(NSEI, theme = chartTheme('black'))
addSMA(n = 20, col = 'red')
addSMA(n = 50, col = 'cyan')
legend('left', col = c('green','red','cyan'),
       legend = c('NIFTY 50','SMA20','SMA50'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.5)

#2- WPR -

wpr_nifty <- WPR(HLC(NSEI), n = 14)
colnames(wpr_nifty) <- 'wpr'
barChart(NSEI, subset = '2020::2021',theme = 'white')
addWPR(n = 14)

#3-BBands-
chartSeries(NSEI, subset = '2020::2021', theme = "white", TA = NULL)
addBBands(n=20, sd=2)

#4 MACD -
chartSeries(NSEI, subset = '2020::2021', theme = "white")
addMACD(fast=12,slow=26,signal=9)


#Creating Trade Signals -
#1. SMA -
#SMA 20 crossover
sma20_nifty_ts <- Lag(
  ifelse(Lag(Cl(NSEI)) < Lag(sma20_nifty) & Cl(NSEI) > sma20_nifty,1,
         ifelse(Lag(Cl(NSEI)) > Lag(sma20_nifty) & Cl(NSEI) < sma20_nifty,-1,0)))
sma20_nifty_ts[is.na(sma20_nifty_ts)] <- 0


# SMA 50 Crossover Signal
sma50_nifty_ts <- Lag(
  ifelse(Lag(Cl(NSEI)) < Lag(sma50_nifty) & Cl(NSEI) > sma50_nifty,1,
         ifelse(Lag(Cl(NSEI)) > Lag(sma50_nifty) & Cl(NSEI) < sma50_nifty,-1,0)))

sma50_nifty_ts[is.na(sma50_nifty_ts)] <- 0

# SMA 20 and SMA 50 Crossover Signal
sma_nifty_ts <- Lag(
  ifelse(Lag(sma20_nifty) < Lag(sma50_nifty) & sma20_nifty > sma50_nifty,1,
         ifelse(Lag(sma20_nifty) > Lag(sma50_nifty) & sma20_nifty < sma50_nifty,-1,0)))

sma_nifty_ts[is.na(sma_nifty_ts)] <- 0

which(sma_nifty_ts==1)
which(sma_nifty_ts == -1)


#2- WPR -
wpr_nifty_ts <- Lag(
  ifelse(Lag(wpr_nifty) > 0.8 & wpr_nifty < 0.8,1,
         ifelse(Lag(wpr_nifty) > 0.2 & wpr_nifty < 0.2,-1,0)))

wpr_nifty_ts[is.na(wpr_nifty_ts)] <- 0

which(wpr_nifty_ts ==1)
which(wpr_nifty_ts == -1)

chartSeries(NSEI,
            type = 'line',
            theme=chartTheme('white'))
addTA(sma_nifty_ts,type='S',col='red')
addTA(wpr_nifty_ts, type = 's', col = 'blue')

#Trading Strategies and checking the signals created -
#1 SMA
sma_nifty_strategy <- ifelse(sma_nifty_ts > 1,0,1)
for (i in 1 : length(Cl(NSEI))) {
  sma_nifty_strategy[i] <- ifelse(sma_nifty_ts[i] == 1,1,ifelse(sma_nifty_ts[i] == -1,0,sma_nifty_strategy[i-1]))
}
sma_nifty_strategy[is.na(sma_nifty_strategy)] <- 1
sma_nifty_strategycomp <- cbind(sma20_nifty, sma50_nifty, sma_nifty_ts, sma_nifty_strategy)
colnames(sma_nifty_strategycomp) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')

#2 WPR-
wpr_nifty_strategy <- ifelse(wpr_nifty_ts > 1,0,1)
for (i in 1 : length(Cl(NSEI))) {
  wpr_nifty_strategy[i] <- ifelse(wpr_nifty_ts[i] == 1,1,ifelse(wpr_nifty_ts[i] == -1,0,wpr_nifty_strategy[i-1]))
}
wpr_nifty_strategy[is.na(wpr_nifty_strategy)] <- 1
wpr_nifty_stratcomp <- cbind(wpr_nifty, wpr_nifty_ts, wpr_nifty_strategy)
colnames(wpr_nifty_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

View(wpr_nifty_stratcomp)
View(sma_nifty_strategycomp)

## BACKTESTING-
#choose the closing price of NSE data to calculate the averages
data <- NSEI[,4]

#Backtesting the SMA strategy -
# Calculating the SMA
# Set "n=" to the SMA period
sma <- SMA(Cl(data), n = 50)

# Calculate MACD
# Set "nsig=" to the SMA period
macd <- MACD(data, nFast=12, nSlow=26, nSig=50, maType=SMA, percent = FALSE)

# Calculate the buy/sell signals
signal <- Lag(ifelse(macd$macd < macd$signal, -1, 1))

# Calculate the returns from the Trading Strategy
returns <- ROC(data)*signal

# Remove not available values from the returns
returns <- na.omit(returns)

# Calculate the Portfolio
portfolio <- exp(cumsum(returns))
plot(portfolio)
kable(table.Drawdowns(returns), caption = "Table of Drawdowns")
kable(table.DownsideRisk(returns), caption = "Downside Risk Table")

# Performance Summary Chart
charts.PerformanceSummary(returns, main = "Performance Summary Chart of Nifty 50",)

chart.RiskReturnScatter(returns)

Drawdowns(returns, geometric = TRUE)
findDrawdowns(returns, geometric = TRUE)
