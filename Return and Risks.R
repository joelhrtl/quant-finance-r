## Return and risks ##
# Define some example variables
inflation_rate = 0.02 # Inflation rate
library(quantmod)
library(e1071)
library(sn)

# Define the symbols for the market index and a stock
market_symbol= "^GSPC" # S&P 500
stock_symbol = "AMZN" # Amazon
risk_free_symbol = "^IRX" # 13-week T-bill rate

# Download historical data from Yahoo Finance
getSymbols(market_symbol, src = "yahoo", from = "2010-01-01", to = "2024-06-14")
getSymbols(stock_symbol, src = "yahoo", from = "2010-01-01", to = "2024-06-14")
getSymbols(risk_free_symbol, src = "yahoo", from = "2010-01-01", to = "2024-06-14")

# View the first few rows of the downloaded data
head(GSPC)
head(AMZN)
head(IRX)

# Define the adjusted closing prices for AMZN, GSPC, and IRX
amzn_prices = AMZN[, "AMZN.Adjusted"]
gspc_prices = GSPC[, "GSPC.Adjusted"]
rf_rates = IRX[, "IRX.Adjusted"]

# Calculate the daily holding period returns for AMZN and GSPC
# The Delt function calculates the percentage change between the closing prices on consecutive days, giving the daily return.
amzn_hpr = Delt(amzn_prices)
gspc_hpr = Delt(gspc_prices)

# Convert the risk-free rate to a daily rate (IRX is given in percentage points per annum)
rf_rates_daily = rf_rates / 100 / 252

# Remove NA values
amzn_hpr = na.omit(amzn_hpr)
gspc_hpr = na.omit(gspc_hpr)
rf_rates_daily <- na.omit(rf_rates_daily)

# Align the dates of risk-free rates with the stock and market returns
rf_rates_daily = rf_rates_daily[index(amzn_hpr)]

# Calculate the expected returns and standard deviation
expected_stock_return = mean(amzn_hpr)
expected_market_return = mean(gspc_hpr)
stock_sd = sd(amzn_hpr)
market_sd = sd(gspc_hpr)

# Display the results
expected_stock_return
expected_market_return
stock_sd
market_sd

## Interpretation
# expected Daily Return:
# Amazon = 0.112% (or approximately 0.001124441)
# S&P 500 = 0.049% (or approximately 0.0004913393)

# Standard Deviation (Volatility):
# Amazon = 2.07%
# S&P 500 = 1.09%

# Calculate the daily risk premium and excess returns
# We calculate the risk premium as the expected return minus the mean daily risk-free rate.
risk_premium = expected_stock_return - mean(rf_rates_daily)
excess_stock_return = amzn_hpr - mean(rf_rates_daily)

# Display the risk premium
risk_premium

# The positive value of approximately 0.108% (0.001082423) indicates that the expected return of Amazon exceeds the risk-free rate, providing compensation for the risk taken by investors.

# Calculate geometric and arithmetic averages of returns
geometric_mean = function(returns) {
  prod(1 + returns)^(1/length(returns)) - 1
}

geometric_stock_return = geometric_mean(amzn_hpr)
arithmetic_stock_return = mean(amzn_hpr)

geometric_stock_return
arithmetic_stock_return

#Geometric Mean = approximately 0.091% (0.000911736). This measure accounts for compounding effects and is typically lower than the arithmetic mean.
# The geometric mean gives the true average compound growth rate per period over multiple periods.
# It accounts for the effects of compounding, making it more accurate for assessing long-term investment performance.
# The geometric mean will always be less than or equal to the arithmetic mean unless all returns are identical.
# It is particularly useful for understanding the actual growth rate of an investment portfolio over time.

#Arithmetic Mean = approximately 0.112% (0.001124441). This is the simple average of daily returns.
# The arithmetic mean provides the average return per period without considering compounding.
# It is useful for assessing the expected return in a single period, especially for short-term investments.
# The arithmetic mean can overstate the long-term performance of an investment because it does not account for the variability of returns.
# It is often used in theoretical models of portfolio performance and risk-return trade-offs.

# Calculate log returns
log_returns = diff(log(amzn_prices))
mean_return = mean(na.omit (log_returns))
# Log Returns are often positioned between the arithmetic and geometric means, reflecting continuous compounding and additivity over time.
# Log returns are often closer to the geometric mean than the arithmetic mean. 
# They approximate continuous compounding, which can provide a smoother and more accurate measure over long periods.

# Calculate variance and standard deviation
stock_variance = var(amzn_hpr)
stock_sd = sd(amzn_hpr)

stock_variance
stock_sd
# Variance: Approximately 0.000427 (0.0004269061), indicating the degree of dispersion of returns.
# Standard Deviation: Confirmed as 2.07% (0.02066171), indicating the volatility of Amazon's returns.

# Calculate the Sharpe ratio
sharpe_ratio = (expected_stock_return - mean(rf_rates_daily)) / stock_sd

sharpe_ratio
#Approximately 0.0524 (0.05238789). 
#This positive value indicates that Amazon's returns exceed the risk-free rate, adjusted for the volatility. 
# A higher Sharpe ratio is generally preferred as it indicates better risk-adjusted returns.

# Normality
# Plot returns
plot(amzn_hpr, type = "l", lwd = 2, col = "blue", main = "Returns",
     xlab = "Returns", ylab = "Density")
abline(v = mean_return, col = "red", lwd = 2, lty = 2)
text(mean_return, 0.02, "Mean", pos = 4, col = "red")

# Define the parameters for the normal distribution
mean_return = geometric_stock_return
sd_return = stock_sd

# Generate a sequence of returns from -3*sd to +3*sd around the mean
returns = seq(mean_return - 3 * sd_return, mean_return + 3 * sd_return, length.out = 1000)

# Calculate the density of the normal distribution for these returns
density = dnorm(returns, mean = mean_return, sd = sd_return)

# Plot the normal distribution
plot(returns, density, type = "l", lwd = 2, col = "blue", main = "Normal Distribution of Returns",
     xlab = "Returns", ylab = "Density")
abline(v = mean_return, col = "red", lwd = 2, lty = 2)
text(mean_return, max(density) * 0.9, "Mean", pos = 4, col = "red")

## Visualizing Different Distributions
# Generate data for normal, skewed, and fat-tailed distributions
set.seed(123)
normal_dist = rnorm(1000, mean = mean_return, sd = sd_return)
skewed_dist = rsn(1000, xi = mean_return, omega = sd_return, alpha = -5) # Skewed normal distribution
fat_tailed_dist = rt(1000, df = 5) * sd_return / sqrt(5 / 3) + mean_return # t-distribution with 5 df

# Plot the distributions
par(mfrow = c(1, 2))
hist(normal_dist, breaks = 50, probability = TRUE, main = "Normal Distribution", col = "grey")
lines(density(normal_dist), col = "blue", lwd = 2)
hist(fat_tailed_dist, breaks = 50, probability = TRUE, main = "Fat-Tailed Distribution", col = "grey")
lines(density(fat_tailed_dist), col = "blue", lwd = 2)
hist(skewed_dist, breaks = 50, probability = TRUE, main = "Skewed Distribution", col = "grey")
lines(density(skewed_dist), col = "blue", lwd = 2)

## Calculate skewness and kurtosis
skewness_value = skewness(amzn_hpr)
cat("Skewness:", skewness_value, "\n")
# Skewness measures the asymmetry of the distribution of returns. 
# It indicates whether the distribution tails off to the left or the right.
# Interpretation: positive Skewness (0.2872202) indicates that the distribution of returns is slightly skewed to the right. 
# This means that there are more frequent small losses and a few larger gains.
# The skewness value of 0.2872202 suggests mild skewness. Values close to zero indicate a symmetric distribution. Values greater than +1 or less than -1 indicate a highly skewed distribution.

kurtosis_value = kurtosis(amzn_hpr)
cat("Kurtosis:", kurtosis_value, "\n")
# Kurtosis measures the "tailedness" of the distribution of returns. 
# It indicates the presence of extreme values (outliers) in the distribution.
# Excess Kurtosis -  the kurtosis value of a normal distribution is 3. 
# The excess kurtosis is calculated as kurtosis - 3. 
# For Amazon:   Excess Kurtosis =  6.322646−3=3.322646
# Positive Excess Kurtosis (3.322646) indicates that the distribution has fatter tails than a normal distribution. 
# There are more extreme values (outliers) than would be expected in a normal distribution.
# A kurtosis value greater than 3 indicates leptokurtic distribution, which is prone to producing outliers. 
# Values significantly higher than 3 suggest a high probability of extreme returns.

# Correlation between amzn and S&P
correlation = cor(amzn_hpr, gspc_hpr)
correlation

#### TESTS ### 

require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations

# Download historical data from Yahoo Finance
getSymbols(market_symbol, src = "yahoo", from = "2010-01-01", to = "2025-07-10")

# View the first few rows of the downloaded data
head(GSPC)

# Define the adjusted closing prices for GSPC
gspc_prices = GSPC[, "GSPC.Adjusted"]

# Calculate the daily holding period returns for  GSPC
# The Delt function calculates the percentage change between the closing prices on consecutive days, giving the daily return.
gspc_hpr = Delt(gspc_prices)

# Convert the risk-free rate to a daily rate (IRX is given in percentage points per annum)
rf_rates_daily = rf_rates / 100 / 252

# Remove NA values
gspc_hpr = na.omit(gspc_hpr)
return = gspc_hpr

summary (return)

plot(return, main = "S&P 600 daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions
## Location
mean (return) # calculate mean of the return
mean (return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 = c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 = c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(return) # calculate the median value of the return

## Estimates of Variability
# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(return)

# Variance
var(return)
# Standard deviation
sd(return)
# Median Absolute Deviation
mad (return)

# Estimates Based on Percentiles
quantile (return)
quantile (return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (return, tauseq)

IQR (return) # IQRx = Qx(0,75) - Qx(0.25)

# boxplot
boxplot(return, horizontal=TRUE, main="SP 600 Return")

hist(return) # bild a histograme of the return

# kernel density
return.density = density(return) # estimate kernel density
plot (return.density) # plot the estimated density

#QQ
qqnorm(return)
qqline(return)

## Tests for normality
#skewness
library(e1071) # the library to calculate skewness
skewness (return) # The skewness of any symmetric distribution is 0.
# If a distribution has a long right tail, its skewness is positive.
# If a distribution has a long left tail, its skewness is negative.
kurtosis(return) #The kurtosis of a distribution is a measure of how much mass is in its
#tails and, therefore, is a measure of how much of the variance of Y arises from
#extreme values. The kurtosis of a normally distributed random variable is 3.

# D'AGOSTINO TEST OF SKEWNESS
library (moments)
agostino.test(return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

# Kolmogorov-Smirnov
library (fBasics)
ks.test(return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(return)

## Multivariate analysis
## I test correlation between SP600 (from teh previous examples) and Bitcoin

##Data preparation ########################################################################################################
getSymbols(stock_symbol, src = "yahoo", from = "2010-01-01", to = "2025-07-11")

# Define the adjusted closing prices for AMZN, GSPC, and IRX
amzn_prices = AMZN[, "AMZN.Adjusted"]

# Calculate the daily holding period returns for AMZN 
amzn_hpr = Delt(amzn_prices)

return2=amzn_hpr

plot (return)
plot (return2)

# for further simplicity let's assign returns to X and Y
X= return
Y= return2

length(X)
length(Y)

combined = merge(X, Y, join = "inner")
colnames(combined)=c("SP600", "AMZN")

# Correlation 
require (fBasics) # attache the required package

#Pearson's product moment correlation coefficient t-test
cor.test (combined$SP600,combined$AMZN, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(combined$SP600,combined$AMZN)

#spearman rank correlation test
# Remove rows with any NA
combined_clean <- na.omit(combined)
cor.test(as.numeric(combined_clean$SP600), as.numeric(combined_clean$AMZN), 
         method = "spearman", alternative = "two.sided")

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(as.numeric(combined_clean$SP600), as.numeric(combined_clean$AMZN), 
         method ="kendal",alternative="two.sided")


