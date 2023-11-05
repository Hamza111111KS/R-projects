library(ggplot2)

# plot closing prices of BTC and S&P 500
ggplot(data = data) +
  geom_line(aes(x = index(data), y = btc, color = "BTC")) +
  geom_line(aes(x = index(data), y = sp500, color = "S&P 500")) +
  labs(title = "Closing Prices of BTC and S&P 500", x = "Date", y = "Price") +
  scale_color_manual(values = c("BTC" = "blue", "S&P 500" = "red"))

#Line plot of BTC and S&P 500 closing prices:
ggplot(data = data) +
  geom_line(aes(x = index(data), y = btc, color = "BTC")) +
  geom_line(aes(x = index(data), y = sp500, color = "S&P 500")) +
  labs(title = "Closing Prices of BTC and S&P 500", x = "Date", y = "Price") +
  scale_color_manual(values = c("BTC" = "blue", "S&P 500" = "red"))


library(ggpubr)
#Scatter plot of BTC returns vs. S&P 500 returns:
ggscatter(data, x = "sp500ret", y = "btcret",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson") +
  labs(title = "Scatter Plot of BTC Returns vs. S&P 500 Returns", x = "S&P 500 Returns", y = "BTC Returns")

#Density plot of BTC returns and S&P 500 returns:
ggplot(data = data) +
  geom_density(aes(x = btcret, fill = "BTC")) +
  geom_density(aes(x = sp500ret, fill = "S&P 500")) +
  labs(title = "Density Plot of BTC Returns and S&P 500 Returns", x = "Returns", y = "Density") +
  scale_fill_manual(values = c("BTC" = "blue", "S&P 500" = "red"))

#Box plot of BTC returns and S&P 500 returns:
ggplot(data = data, aes(x = asset, y = ret)) +
  geom_boxplot(aes(fill = asset)) +
  labs(title = "Box Plot of BTC Returns and S&P 500 Returns", x = "Asset", y = "Returns") +
  scale_fill_manual(values = c("BTC" = "blue", "S&P 500" = "red")) +
  theme(legend.position = "none")

#Bar plot of rolling correlation between BTC and S&P 500:
library(PerformanceAnalytics)

corr <- rollapply(data[, c("btcret", "sp500ret")], width = 30, FUN = function(x) cor(x[, 1], x[, 2]), by.column = FALSE, align = "right")
dates <- index(data)[30:length(data)]

df <- data.frame(Date = dates, Correlation = corr)

ggplot(df, aes(x = Date, y = Correlation)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Rolling Correlation Between BTC and S&P 500", x = "Date", y = "Correlation")


#Heatmap of rolling correlation between BTC and S&P 500:
library(gplots)

corr <- rollapply(data[, c("btcret", "sp500ret")], width = 30, FUN = function(x) cor(x[, 1], x[, 2]), by.column = FALSE, align = "right")
dates <- index(data)[30:length(data)]

df <- data.frame(Date = dates, Correlation = corr)

m <- as.matrix(df[, 2])
colnames(m) <- c("Correlation")
rownames(m) <- df[, 1]
heatmap.2(m, trace = "none", main = "Rolling Correlation Between BTC and S&P 500", xlab = "Date")

#Histogram of daily returns for BTC-USD:
library(quantmod)
getSymbols("BTC-USD", src = "yahoo")
daily_returns <- dailyReturn(BTCUSD$BTCUSD.Close)
hist(daily_returns, main = "Histogram of Daily Returns for BTC-USD")


#Line plot of the spread between the two assets:
spread <- data$btc - data$sp500
plot(spread, type = "l", main = "Spread between BTC-USD and S&P 500")


library(rugarch)

# Specify GARCH model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0)))

# Initialize empty model
model <- ugarchfit(spec, data = NULL)

# Start loop for streaming data
while(TRUE) {
  # Read in new data (assuming it is in a dataframe called "newdata")
  newreturns <- diff(log(newdata$btc))
  
  # Update model with new data
  model <- ugarchfit(spec, data = newreturns, model = model, solver = "hybrid")
  
  # Make predictions or perform other analysis with updated model
  # ...
  
  # Wait for some period of time before updating again (e.g. 1 minute)
  Sys.sleep(60)
}  
