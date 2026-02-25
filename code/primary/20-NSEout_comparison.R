library(dtw)
library(forecast)
library(here)
library(lmtest)
library(tidyverse)
library(tseries)
library(urca)

# set loc
here::i_am("code/primary/20-NSEout_comparison.R")
options(max.print=2000)

# load in ssE (from 05E)
ssNSE <- readRDS(file=here("data", "clean", "ssNSE.rds"))

# fitted data - Indian River and NSE(out)
states <- ssNSE$states
df <- data.frame(states)
df <- data.frame(t(df))

names(df)[names(df) == "X1"] <- "NS_e"
names(df)[names(df) == "X2"] <- "IR_e"
names(df)[names(df) == "X3"] <- "NS_o"
names(df)[names(df) == "X4"] <- "IR_o"

df_E <- df[-c(3,4)]
df_O <- df[-c(1,2)]

NS_e <- ts(df_E$NS_e,
           start = 1, end = 32,
           frequency = 1)
plot.ts(NS_e)

IR_e <- ts(df_E$IR_e,
           start = 1, end = 32,
           frequency = 1)

NS_o <- ts(df_O$NS_o,
           start = 1, end = 32,
           frequency = 1)

IR_o <- ts(df_O$IR_o,
           start = 1, end = 32,
           frequency = 1)

# stationarity tests - confirm non-stationary
adf.test(NS_e)
  # non-stationary
adf.test(IR_e)
  # non-stationary
adf.test(NS_o)
  # non-stationary
adf.test(IR_o)
  # non-stationary

kpss.test(NS_e)
kpss.test(NS_e, null = "Trend")
  # suggests trend-stationary
kpss.test(IR_e)
kpss.test(IR_e, null = "Trend")
  # suggests trend-stationary
kpss.test(NS_o)
kpss.test(NS_o, null = "Trend")
  # suggests trend-stationary
kpss.test(IR_o)
kpss.test(IR_o, null = "Trend")
  # suggests trend-stationary

# auto.ARIMA to identify necessary amount of differencing
ARIMA.NS_e <- auto.arima(NS_e)
ARIMA.IR_e <- auto.arima(IR_e)
ARIMA.NS_o <- auto.arima(NS_o)
ARIMA.IR_o <- auto.arima(IR_o)

summary(ARIMA.NS_e)
summary(ARIMA.IR_e)
summary(ARIMA.NS_o)
summary(ARIMA.IR_o)
  # all four models are ARIMA(0,1,0) -> random walk model, 1st order differencing

# differencing data to remove linear trends - per ARIMA models
diffNS_e <- diff(NS_e,
                 lag = 1,
                 differences = 1)
diffIR_e <- diff(IR_e,
                 lag = 1,
                 differences = 1)
diffNS_o <- diff(NS_o,
                 lag = 1,
                 differences = 1)
diffIR_o <- diff(IR_o,
                 lag = 1,
                 differences = 1)

# stationarity tests for differenced data - confirm
adf.test(diffNS_e)
  # stationary
adf.test(diffIR_e)
  # non-stationary
adf.test(diffNS_o)
  # non-stationary
adf.test(diffIR_o)
  # non-stationary

kpss.test(diffNS_e)
  # stationary
kpss.test(diffIR_e)
  # stationary
kpss.test(diffNS_o)
  # stationary
kpss.test(diffIR_o)
  # stationary

# more differences needed? let's look at forecast::ndiff
forecast::ndiffs(NS_e, test= "adf")
forecast::ndiffs(IR_e, test= "adf")
forecast::ndiffs(NS_o, test= "adf")
forecast::ndiffs(IR_e, test= "adf")

forecast::ndiffs(NS_e, test= "kpss")
forecast::ndiffs(IR_e, test= "kpss")
forecast::ndiffs(NS_o, test= "kpss")
forecast::ndiffs(IR_e, test= "kpss")

# applied time series analysis textbook suggests that "The null hypothesis of a 
# random walk is now rejected so you might think that a 2nd difference is needed 
# for the anchovy data. However the actual problem is that the default for 
# adf.test() includes a trend but we removed the trend with our first difference. 
# Thus we included an unneeded trend parameter in our test. Our data are not that 
# long and this affects the result.
# 
# Let’s repeat without the trend and we’ll see that the null hypothesis is 
# rejected. The number of lags is set to be what would be used by adf.test(). 
# See ?adf.test."
#
# let's try and apply this...

k <- trunc((length(diffNS_e) - 1)^(1/3))
testNS_e <- urca::ur.df(diffNS_e, type = "drift", lags = k)

k <- trunc((length(diffIR_e) - 1)^(1/3))
testIR_e <- urca::ur.df(diffIR_e, type = "drift", lags = k)

k <- trunc((length(diffNS_e) - 1)^(1/3))
testNS_o <- urca::ur.df(diffNS_o, type = "drift", lags = k)

k <- trunc((length(diffIR_e) - 1)^(1/3))
testIR_o <- urca::ur.df(diffIR_o, type = "drift", lags = k)

summary(testNS_e)
  # stationary
summary(testIR_e)
  # stationary
summary(testNS_o)
  # non-stationary (close)
summary(testIR_o)
  # non-stationary (close)




# Box-Jenkins method suggests taking a second difference...
dif2NS_e <- diff(diffNS_e)
dif2IR_e <- diff(diffIR_e)
dif2NS_o <- diff(diffNS_o)
dif2IR_o <- diff(diffIR_o)

# stationarity tests for double-differenced data - confirm
adf.test(dif2NS_e)
  # stationary
adf.test(dif2IR_e)
  # stationary
adf.test(dif2NS_o)
  # non-stationary
adf.test(dif2IR_o)
  # non-stationary

kpss.test(dif2NS_e)
  # stationary
kpss.test(dif2IR_e)
  # stationary
kpss.test(dif2NS_o)
  # stationary
kpss.test(dif2IR_o)
  # stationary



# differenced time series are 'trend stationary' (i.e. passing kpss but not adf)
# except differenced NS_e which is stationary

# what about detrending data?
time_index <- seq_along(diffNS_e)
lmNS_e <- lm(diffNS_e ~ time_index)
lmIR_e <- lm(diffIR_e ~ time_index)
lmNS_o <- lm(diffNS_o ~ time_index)
lmIR_o <- lm(diffIR_o ~ time_index)

detrendedNS_e <- residuals(lmNS_e)
detrendedIR_e <- residuals(lmIR_e)
detrendedNS_o <- residuals(lmNS_o)
detrendedIR_o <- residuals(lmIR_o)

# stationarity tests for differenced data - confirm
adf.test(detrendedNS_e)
   # stationary
adf.test(detrendedIR_e)
   # non-stationary
adf.test(detrendedNS_o)
  # non-stationary
adf.test(detrendedIR_o)
  # non-stationary

kpss.test(detrendedNS_e)
   # stationary
kpss.test(detrendedIR_e)
   # stationary
kpss.test(detrendedNS_o)
  # stationary
kpss.test(detrendedIR_o)
  # stationary

# why would detrended data have the exact same result in an adf or kpss test as before detrending?
# possible because of high-order autocorrelation

# autocorrelation
acf(NS_e)
acf(IR_e)
acf(NS_o)
acf(IR_o)
  # high-order for sure, all time series have at least 4 significant lags
  # this makes sense given that these are generations of fish















# if stationary, let's check mean and variance
mean(NS_e) # -0.2301661
mean(IR_e) # -0.4252932
var(NS_e) # 0.8847487
var(IR_e) # 1.057261

mean(NS_o) # 0.2877304
mean(IR_o) # -0.3054424
var(NS_o) # 0.456703
var(IR_o) # 1.115295

# autocorrelation
acfNS_e <- acf(NS_e, plot = TRUE) # 5 sig lags
acfIR_e <- acf(IR_e, plot = TRUE) # 7 sig lags
acfNS_o <- acf(NS_o, plot = TRUE) # 3 sig lags
acfIR_o <- acf(IR_o, plot = TRUE) # 4 sig lags

# t.tests - means
t.test(NS_e, IR_e, var.equal = FALSE) # NO SIGNIFICANT DIFFERENCE
t.test(NS_o, IR_o, var.equal = FALSE) # SIGNIFICANT DIFFERENCE

# F-test - variance
var.test(NS_e, IR_e) # NO SIGNIFICANT DIFFERENCE
var.test(NS_o, IR_o) # SIGNIFICANT DIFFERENCE

# # partial autocorrelation
# pacf(NS_e)
# pacf(IR_e)
# pacf(NS_o)
# pacf(IR_o)
# 
# # differences?
# diffNS_e <- diff(NS_e,
#                  lag = 1,
#                  differences = 1)
# diffIR_e <- diff(IR_e,
#                  lag = 1,
#                  differences = 1)
# diffNS_o <- diff(NS_o,
#                  lag = 1,
#                  differences = 1)
# diffIR_o <- diff(IR_o,
#                  lag = 1,
#                  differences = 1)
# plot(diffNS_e)
# plot(diffIR_e)
# plot(diffNS_o)
# plot(diffIR_o)
# 
# # autocorrelation
# acf(diffNS_e)
# acf(diffIR_e)
# acf(diffNS_o)
# acf(diffIR_o)
# 
# # partial autocorrelation
# pacf(diffNS_e)
# pacf(diffIR_e)
# pacf(diffNS_o)
# pacf(diffIR_o)
# 
# ## cross-correlation function
# ccf(NS_e, IR_e)
# ccf(NS_o, IR_o)
# 
# ## arima models
# ARIMA.NS_e <- auto.arima(NS_e)
# summary(ARIMA.NS_e)
# ARIMA.IR_e <- auto.arima(IR_e)
# summary(ARIMA.IR_e)
# 
# ARIMA.NS_o <- auto.arima(NS_o)
# summary(ARIMA.NS_o)
# ARIMA.IR_o <- auto.arima(IR_o)
# summary(ARIMA.IR_o)
#   # not sure what this tells me
# 
# ## dynamic time warping
# # Compute the DTW alignment
# alignmentE <- dtw(NS_e, IR_e, dist.method = "Euclidean")
# alignmentO <- dtw(NS_o, IR_o, dist.method = "Euclidean")
# 
# # Plot the results
# plot(alignmentE)
# abline(a = 0, b = 1, lty = "dotted", col = "red")
# plot(alignmentO)
# abline(a = 0, b = 1, lty = "dotted", col = "red")
