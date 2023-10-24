# Algorithmic-Trading-using-R


In this assingment, we worked to develop a strategy to deal with stock trading in the market.
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
It was observed that optimising techinical indicator approach yielded better results as it was able to accurately predict 82% future stock prices 

It is essential to note that accurately predicting the market just using algorithm is almost impossible and there is
always certain leeway with the predictions.
