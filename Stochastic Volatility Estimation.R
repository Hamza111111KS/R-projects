set.seed(123)
library(stochvol)
library(tseries)
library(readr)
library(zoo)


# Price Data
market_price <- read_csv("E:/AF/IJE/IJE/Projects/Projet_1 ( Risk Measuring )/market-price.csv")
View(market_price)
colnames(market_price) <- c('Date','Price')
prices <- market_price$Price
logprices <- log(prices)
priceplot <- data.frame(as.Date(market_price$Date), market_price$Price)
plot(priceplot, type = 'l', col = 'blue', lwd = '2', ylab = "", xlab = "",
     main = 'Historical Bitcoin Prices ($)')

# Volume Data
trade_volume <- read_csv("E:/AF/IJE/IJE/Projects/Projet_1 ( Risk Measuring )/trade-volume.csv")
View(trade_volume)
colnames(trade_volume) <- c('Date','Volume')
plot(trade_volume$Volume, type = 'l')
volumes <- trade_volume$Volume

# Returns
returns <- logprices[2:length(logprices)] - logprices[1:(length(logprices)-1)]
plot(returns, type = 'l')
plot(prices, type = 'l')
plot(logprices, type = 'l')



#########################################################
#########################################################
# BITCOIN ###############################################                                               
#########################################################
#January 8th, 2012 is an important date in the history of Bitcoin, as it was the date on which the Bitcoin block reward was halved for the first time. This is a significant event in the Bitcoin network's history, as it marked a major milestone in the cryptocurrency's mining process.
bitcoin_volume <- trade_volume[547:nrow(trade_volume),]
bitcoin_volume$Date <- as.Date(bitcoin_volume$Date)
plot(bitcoin_volume, type = 'l', main = 'Volume Traded', col = 'blue')

bitcoin_prices <- market_price[547:nrow(market_price),]
bitcoin_prices$Date <- as.Date(bitcoin_prices$Date)
View(bitcoin_prices)
plot(bitcoin_prices, type = 'l')
bitcoin_ret <- logret(bitcoin_prices$Price, demean = TRUE) #The output "log_returns" will be a time series object containing the logarithmic returns for each period

plot(bitcoin_prices$Date[2:nrow(bitcoin_prices)], bitcoin_ret, type = 'l',
     col = 'purple', main = 'Bitcoin Returns', ylab = "", lwd = '2')
plot(bitcoin_prices$Date[2:nrow(bitcoin_prices)], sqrt(bitcoin_ret^2), type = 'l',
     col = 'brown', main = 'Bitcoin Observed Volatility', ylab = "", lwd = '2')
adf.test(bitcoin_ret)$p.value #ADF is a test to verify the staionnarity of the time series
PP.test(bitcoin_ret)$p.value #In adf.test(bitcoin_ret) : p-value smaller than printed p-value P value less the the level of significance 0.0001 meaning that we dont have staionnarity


#########################################################
# Stochastic Volatility Estimates
res <- svsample(bitcoin_ret, priormu = c(-10, 1), priorphi = c(20,1.5),priorsigma = 0.1) #-10 : reflecting a negative decline ()pov, 1 for long tail(high volatility)
summary(res, showlatent = TRUE)
mean_exp_ht_2 <- as.numeric(summary(res, showlatent = TRUE)$latent[,6])
sd_exp_ht_2 <- as.numeric(summary(res, showlatent = TRUE)$latent[,7])
volatility <- sd_exp_ht_2
stoch_vol <- data.frame(bitcoin_prices$Date, volatility)
colnames(stoch_vol) <- c('Date','volatility')
stoch_vol$volatility <- stoch_vol$vol
stoch_vol <- stoch_vol[2:nrow(stoch_vol),]
plot(stoch_vol, type = 'l', main = 'Estimated (Median) Stochastic Volatility', col = 'brown',
     lwd = '2')

volplot(res,dates = bitcoin_prices$Date[-1])
plot(res, dates = bitcoin_prices$Date[-1])
plot(res, forecast = 10, dates = bitcoin_prices$Date[-1])
plot(resid(res),bitcoin_ret)

RMSE_stochvol <- sqrt((1/nrow(stoch_vol))*sum(stoch_vol[,2]^2 - (sqrt(bitcoin_ret^2))^2)^2)
print(RMSE_stochvol)
summary(res, showlatent = FALSE)

#########################################################

# GARCH(1,1) Volatility Estimates using library fGarch
bitcoin_returns_zoo <- zoo(bitcoin_ret,order.by = as.Date(stoch_vol$Date))
library(fGarch)
fit = garchFit( ~ garch(1, 1), data = bitcoin_returns_zoo)
garchEstimates <- volatility(fit, type = 'sigma')
predict(fit,1)
plot(bitcoin_prices$Date[-1], garchEstimates, type = 'l', 
     col = 'brown', lwd = "2", main = 'Estimated GARCH(1,1) Volatility')
#########################################################

# Bitcoin Volatility vs GARCH volatility vs Observed Volatility

plot(stoch_vol, type = 'l', col = 'black', lwd = "2", ylim = range(0,1),
     main = 'Comparing Volatility Estimates')
par(new = TRUE)
plot(garchEstimates, type = 'l', col = 'red', lwd = "2",                                #The resulting garchEstimates object is a time series of conditional volatilities for each period in the original time series
     ylim = range(0,1), axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(sqrt(bitcoin_ret^2), type = 'l', col = 'blue', lwd = "1",
     ylim = range(0,1), axes = FALSE, xlab = "", ylab = "")
legend('topright', c('SV', 'GARCH','Observed'), lty = c(1,1,1),
       lwd = c(2,2,1), col = c('black', 'red', 'blue'))
#########################################################

# Rolling Window GARCH Volatility Estimation
w <- 100 # window size set to ~ 10% of the data size
rollingWindowGarch <- garchEstimates[1:w]
fit_w <- garchFit( ~ garch(1, 1), data = bitcoin_returns_zoo[1:w])
for(i in (w+1):length(garchEstimates)){
  fit_w <- garchFit( ~ garch(1, 1), data = bitcoin_returns_zoo[(i-(w-1)):i])
  rollingWindowGarch <- append(rollingWindowGarch, 
                               as.numeric(predict(fit_w,1)[3]))
}

print(rollingWindowGarch)
plot(bitcoin_prices$Date[-1], garchEstimates, type = 'l', 
     col = 'brown', lwd = "2", 
     main = 'Comparing Rolling Window and Plain Vanilla GARCH Volatilities')
par(new = TRUE)
plot(rollingWindowGarch, type = 'l',
     axes = FALSE, xlab = "", ylab = "",
     col = 'black', lty = 2, lwd = 2)
legend('topright', c('GARCH', 'RW-GARCH'), lty = c(1,2),
       lwd = c(2,2), col = c('brown', 'black'))
#########################################################

# RMSE of the 3 Models to quantify the difference between the actual data and the predicted values from the model.
RMSE_garch <- sqrt((1/nrow(stoch_vol))*sum(garchEstimates^2 - (sqrt(bitcoin_ret^2))^2)^2)
RMSE_stochvol <- sqrt((1/nrow(stoch_vol))*sum(stoch_vol[,2]^2 - (sqrt(bitcoin_ret^2))^2)^2)
RMSE_RWgarch <- sqrt((1/nrow(stoch_vol))*sum(rollingWindowGarch^2 - (sqrt(bitcoin_ret^2))^2)^2)
print(RMSE_garch)
print(RMSE_stochvol)
print(RMSE_RWgarch)
#GARCH Model is superior >>>> ;)
#The Difference between the actual volatility of the data and the predicted volatility from the model, a low rmse indicates that the model is doing a good job of capturing the volatilty dyanamics in the data.













###########################################################
#Comparing the two models
library(tidyverse)
library(lubridate)
library(quantmod)

set.seed(123)

library(stochvol)
library(tseries)
library(readr)
library(zoo)
library(fGarch)

library(quantmod)

# Specify the start and end date for the data
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2022-02-24")

# Download the S&P 500 data from Yahoo Finance
getSymbols("SPY", src = "yahoo", from = start_date, to = end_date)

# Convert to numeric and plot the data
prices <- data.frame(Date = index(SPY), Price = as.numeric(SPY[, "SPY.Close"]))
plot(prices$Price, type = 'l', col = 'blue', lwd = '2', ylab = "Price ($)", xlab = "",
     main = 'Historical S&P500 Prices')

# Calculate returns and plot them
logprices <- log(prices$Price)
returns <- logprices[2:length(logprices)] - logprices[1:(length(logprices)-1)]
plot(returns, type = 'l')

plot(prices, type = 'l')
plot(logprices, type = 'l')

#########################################################
#########################################################
# S&P500 ###############################################
# Create a new data frame with Name and Price columns
#prices_df <- data.frame(Price = as.numeric(prices$Price))#
library(xts)

# Convert prices_df to an xts object with a regular time index
prices_xts <- xts(prices$Price, order.by = index(prices))

# Calculate log returns using Delt()
sp500_ret <- Delt(prices_xts, type = "log")
# Calculate log returns
sp500_ret <- Delt(prices_df, type = "log")
#sp500_ret <- logret(prices)#
plot(sp500_ret, type = 'l', col = 'purple', main = 'S&P500 Returns', ylab = "", lwd = '2')
plot(sqrt(sp500_ret^2), type = 'l', col = 'brown', main = 'S&P500 Observed Volatility', ylab = "")

adf.test(sp500_ret)$p.value
PP.test(sp500_ret)$p.value

#########################################################
# Stochastic Volatility Estimates
res <- svsample(sp500_ret, priormu = c(-10, 1), priorphi = c(20,1.5),priorsigma = 0.1)
summary(res, showlatent = TRUE)
mean_exp_ht_2 <- as.numeric(summary(res, showlatent = TRUE)$latent[,6])
sd_exp_ht_2 <- as.numeric(summary(res, showlatent = TRUE)$latent[,7])
volatility <- sd_exp_ht_2
stoch_vol <- data.frame(sp500_price$Name[-1], volatility)
colnames(stoch_vol) <- c('Name','volatility')
stoch_vol$volatility <- stoch_vol$vol
stoch_vol <- stoch_vol[2:nrow(stoch_vol),]
plot(stoch_vol$volatility, type = 'l', main = 'Estimated (Median) Stochastic Volatility', col = 'brown',
     lwd = '2', xlab = "Time", ylab = "Volatility")

volplot(res,dates = sp500_price$Name[-1])
plot(res, dates = sp500_price$Name[-1])
plot(res, forecast = 10, dates = sp500_price$Name[-1])
plot(resid(res),sp500_ret)

RMSE_stochvol <- sqrt((1/nrow(stoch_vol))*sum(stoch_vol[,2]^2 - (sqrt(sp500_ret^2))^2)^2)
print(RMSE_stochvol)
summary(res, showlatent = FALSE)

#########################################################
# GARCH(1,1) Vol

