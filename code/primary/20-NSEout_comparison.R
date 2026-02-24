library(dtw)
library(forecast)
library(here)
library(lmtest)
library(tidyverse)
library(tseries)

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
           start = 1960, end = 2022,
           frequency = 0.5)
plot.ts(NS_e)

IR_e <- ts(df_E$IR_e,
           start = 1960, end = 2022,
           frequency = 0.5)

NS_o <- ts(df_O$NS_o,
           start = 1961, end = 2023,
           frequency = 0.5)

IR_o <- ts(df_O$IR_o,
           start = 1961, end = 2023,
           frequency = 0.5)

# stationarity tests - looking good
adf.test(NS_e)
  # stationary
adf.test(IR_e)
  # stationary
adf.test(NS_o)
  # stationary
adf.test(IR_o)
  # stationary

kpss.test(NS_e)
  # stationary
kpss.test(IR_e)
  # stationary
kpss.test(NS_o)
  # stationary
kpss.test(IR_o)
  # stationary

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
