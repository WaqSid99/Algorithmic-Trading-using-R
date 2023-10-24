'GROUP 33 Waqas Siddiqui and Shafeek Manayil

Contribution was fairly equal and everyone stuck to their assingned tasks. Most of the assingment was
worked together as we meet regularly and discussed the approach, work distribution etc.

For this assingment, we worked to develop a strategy to deal with stock trading in the market.
The idea of the strategy is to buy and hold the asset when bullish (price increase) and sell when bearish (price decrease)
There are various methodologies to determine the nature of an assets price but for this assingment
the approach of algorithmic trading with technical indicators and a genetic programming approach was adapted and compared.

The technical indicator approach further consists of 4 sub stratagies which made use of technical parameters
like Moving Average Convergence Divergence (MACD) and Relative Strength Index (RSI).

MACD is based as the difference between the 12-period sma and 26-period sma. The 12 day period is considered as fast sma as it captures 
recent changes more quickly whereas 26 days period tells more about the historical performance of the asset. When the avg value 12 day period
goes higher than 26 day period, it means the stock prise is expected to rise and is considered a good time to buy or hold the stock.

RSI in simple terms, captures the momentum of the stock price as in the movement of rise or fall of price.
Stocks which have had more or stronger positive changes have a higher RSI than stocks which have had more or stronger negative changes
RSI provides signals that tell investors to buy when the asset is oversold and to sell when it is overbought.
An asset in oversold when it is trading bellow than its true value. This provides oppertunity to buy cheap
An asset is overbought when it is trading higher than its true value. Investors avoids this so they dont overpay
RSI is measured on a scale from 0 to 100, with high and low levels marked at 70 and 30, respectively

The 3 sub startegies are as follows: ....

1. Buy and Hold when MACD>Signal else sell:
2. Buy and Hold when MACD>Signal and RSI>70 else sell:  
3. Buy and Hold when MACD<Signal and RSI<30 else sell

The stocks analysed in this assingment are AMAZON, FACEBOOK and S&P500. They were chosen because since they
belong to different industries, they would give a hollistic overview of the trading strategy being developed. 

It is essential to note that accurately predicting the market just using algorithm is almost impossible and there is
always certain leeway with the predictions.
'

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")


getSymbols(c("AMZN","^GSPC","FB"), from = '2010-01-01', to = '2022-01-01')

stock <- FB
stock<-na.omit(stock)

rtn_daily <- dailyReturn(stock) # returns by day 
rtn.weekly <- weeklyReturn(stock) # returns by week 
rtn_monthly <- monthlyReturn(stock) # returns by month 
rtn.allperiods <- allReturns(stock) # daily,weekly,monthly,quarterly, yearly 

'From the following figures it can be noted that daily return for stock shows tail-heavy distribution
which indicates presence of kurtosis. This could potentially correspond to high fluctuation of price
on a daily basis.
Where as monthly return for stock seems similar to normal distribution and is relativly more stable/predictable'

#Daily Return 
print(paste0("Standard Deviation of daily returns: ", round(sd(rtn_daily),2)))
print(paste0("Kurtosis of daily returns: ", round(kurtosis(rtn_daily),2)))
options(repr.plot.width = 4, repr.plot.height = 4)
hist(rtn.daily, breaks=100, prob=TRUE) #PRobability dist
#standard normal distribution curve
curve(dnorm(x, mean=mean(rtn_daily), sd=sqrt(var(rtn_daily))), col="red", lwd=2, add=TRUE )

#Monthly Return 
print(paste0("Standard Deviation of monthly returns: ", round(sd(rtn_monthly),2)))
print(paste0("Kurtosis of monthly returns: ", round(kurtosis(rtn_monthly),2)))
options(repr.plot.width = 4, repr.plot.height = 4)
hist(rtn_monthly, breaks=100, prob=TRUE) #PRobability dist
#standard normal distribution curve
curve(dnorm(x, mean=mean(rtn_monthly), sd=sqrt(var(rtn_monthly))), col="red", lwd=2, add=TRUE )
# 

'Calculating MACD AND RSI. 
MACD is the difference between the 12-period simple moving average (SMA) and 26-period SMA'
macd <- MACD(FB$FB.Adjusted, nFast = 12, nSlow = 26, nSig = 9, 
             maType = "SMA", percent = FALSE)
rsi <- RSI(FB$FB.Adjusted, n = 14, maType = "SMA")

#PLOTTING THE MACD LINE WITH 12DAY AND 26 MOVING AVERAGES
options(repr.plot.width = 6, repr.plot.height = 3)
#Fast 12 day sma is blue line
#Slow 26 day sma is red line
chartSeries(stock, subset = "2020::2022-03",bar.type='hlc',
            TA = c(addSMA(n=12,col="blue"),addSMA(n=26,col="red")),theme = chartTheme("white"))

# Strategy 1: if macd>signal, buy and stay in the market. 
# Else, exit the market.
strategy_1 <- ifelse ((macd$signal < macd$macd) , 1, 0)
strategy_1[is.na(strategy_1)] <-0

# Strategy 2: if overbought, enter and stay in the market.
strategy_2 <- ifelse ((macd$signal < macd$macd) & (rsi$rsi > 70), 1, 0)
strategy_2[is.na(strategy_2)] <-0

# Strategy 3: if oversold, enter and stay in the market.
strategy_3 <- ifelse ((macd$signal > macd$macd) & (rsi$rsi < 30), 1, 0)
strategy_3[is.na(strategy_3)] <-0

#BACKTESTING
backtest <- function(df,from_date,to_date,strategy,strategy_name){
  return_ <- rtn_daily[index(rtn_daily)<=to_date & index(rtn_daily)>=from_date]
  #Lagged because daily return of a certain day will be applied to strategy of next day.
  #I.e: the closing price of day x is open price of day x+1
  trade_return <- return_ * stats::lag(strategy, na.pad = FALSE)
  
  #Cummulative return indicates the return from throughout the time period 
  #Annual return indicate indicates the return from a single year 
  #Sharpe ratio is a measure of return per risk associated with stock
  cumm_return <- Return.cumulative(trade_return)
  annual_return <- Return.annualized(trade_return) 
  SharpeRatio <- SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")
  SharpeRatioAnnualized <- SharpeRatio.annualized(trade_return, Rf = 0)
  
  output <- as.data.frame(c(cumm_return,annual_return,SharpeRatio,SharpeRatioAnnualized))
  output <- round(output,4)
  colnames(output) <- strategy_name
  row.names(output) <- c('Cumulative Return','Annual Return',
                      'Sharpe Ratio','Annual Sharpe Ratio')
  
  return(output)
}

# Strategy 1
strategy1_performance <- backtest(FB, from_date = '2007-01-01', 
                                  to_date = '2015-12-31', strategy_1,"Strategy1")
# Strategy 2
strategy2_performance <- backtest(FB, from_date = '2007-01-01', 
                                  to_date = '2015-12-31', strategy_2,"Strategy2")
# Strategy 3
strategy3_performance <- backtest(FB, from_date = '2007-01-01', 
                                  to_date = '2015-12-31', strategy_3,"Strategy3")

performance<-data.frame(strategy1_performance,strategy2_performance,strategy3_performance)
performance
'Of the 3 stocks considered, strategy 1 (buy when MACD>signal) seems to perform the best as it provided the best cumulative return
while having a higher sharpe ratio which could potentially indicate the stock has performed well relative to the risk it has been taken on. 
Altough it must be noted that for AMZN, strategy 3 performed slightly better. Thus further experimention with
varity of other stocks could be done to better understand the quality of strategy developed

The result is unexpected as we were anticipating that the involvment of RSI factor would improve the strategy and provide better return. 
But as mentioned earlier, accurately "timing the market" is not possible and there are various other influencing factors which may not have 
been considered in our strategy.
'
                                       #----------------- Genetic Programming----------------- #


library("gramEvol")
someStocks <- c("FB")
getSymbols(someStocks, src="yahoo", from="2019-01-01", to="2021-01-01", freq="daily")

mydata <- data.frame(x3=Lag(FB$FB.Adjusted,3), x2=Lag(FB$FB.Adjusted,2), x1=Lag(FB$FB.Adjusted,1), FB$FB.Adjusted)
mydata.ts <- as.xts(FB$FB.Adjusted)

close_price<-mydata.ts
open_price<-c()
predictions<-c()

for (i in 1:length(close_price)){
  if (i == 1){
    open_price<-append(open_price,close_price[i])
    
  }else{
    open_price<-append(open_price,close_price[i-1])}
}

lag_price <- data.frame(x5=Lag(close_price,5),x4=Lag(close_price,4),x3=Lag(close_price,3), x2=Lag(close_price,2), x1=Lag(close_price,1), close_price)
names(lag_price) <- c('x5','x4','x3','x2','x1','x')

#75-25 split
traindata<-lag_price[6:378,]
testdata<-lag_price[379:505,]

newRules <- list(expr = grule(op(expr, expr), func(expr), var),
                 func = grule(sin, cos, exp, log),
                 op = grule('+', '-', '*', '/', '^'),
                 var = grule(mydata$x5, mydata$x4, mydata$x3, mydata$x2, mydata$x1))

newGram <- CreateGrammar(newRules)

newFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (sqrt(mean((mydata$x - result)^2)))
  # return (mean(log(1 + abs(mydata$x - result))))
}


mydata <- traindata
ge <- GrammaticalEvolution(newGram, newFitFunc, terminationCost = 0.05, max.depth = 8)

mydata <-testdata[1,1:5]

for(i in 1:9){
  
  predictions[i] <- eval(ge$best$expressions)
  mydata[5] <- mydata[4]
  mydata[4] <- mydata[3]
  mydata[3] <- mydata[2] 
  mydata[2] <- mydata[1]
  mydata[1] <- predictions[i]
  
}

error<-sqrt(mean((testdata$x[1:9]-predictions)^2))
'At this point we concluded that the error between the actual price and predicted price is large 
and decided not to further optimise it. It can definetly be improved by either improving the fitness function,
better grammar rule etc but due to the time constraint we did not get time to further explore those options'

