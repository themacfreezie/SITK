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

k1 <- trunc((length(diffNS_e) - 1)^(1/3)) # all same length
testNS_e <- urca::ur.df(diffNS_e, type = "drift", lags = k1)
testIR_e <- urca::ur.df(diffIR_e, type = "drift", lags = k1)
testNS_o <- urca::ur.df(diffNS_o, type = "drift", lags = k1)
testIR_o <- urca::ur.df(diffIR_o, type = "drift", lags = k1)

summary(testNS_e)
  # stationary
summary(testIR_e)
  # stationary
summary(testNS_o)
  # non-stationary (close)
summary(testIR_o)
  # non-stationary (close)

# p-values for failed odd-year runs?
test_statistic <- testNS_o@teststat[1]
critical_values <- testNS_o@cval[1,]
print(test_statistic)
print(critical_values)
  # test stat within 15% of critical value

test_statistic <- testIR_o@teststat[1]
critical_values <- testIR_o@cval[1,]
print(test_statistic)
print(critical_values)
  # test stat within 17% of critical value

# test diff2 for odd years?
dif2NS_o <- diff(diffNS_o)
dif2IR_o <- diff(diffIR_o)

k2 <- trunc((length(dif2NS_o) - 1)^(1/3)) # all same length
tst2NS_o <- urca::ur.df(dif2NS_o, type = "drift", lags = k2)
tst2IR_o <- urca::ur.df(dif2IR_o, type = "drift", lags = k2)

summary(tst2NS_o)
  # non-stationary (closer)
summary(tst2IR_o)
  # non-stationary (closer)

# test diff3 for odd years?
dif3NS_o <- diff(dif2NS_o)
dif3IR_o <- diff(dif2IR_o)

k3 <- trunc((length(dif3NS_o) - 1)^(1/3)) # all same length
tst3NS_o <- urca::ur.df(dif3NS_o, type = "drift", lags = k3)
tst3IR_o <- urca::ur.df(dif3IR_o, type = "drift", lags = k3)

summary(tst3NS_o)
  # stationary
summary(tst3IR_o)
  # stationary

## MUST TALK TO MARK ABOUT THIS...

# so, diffNS_e & diffIR_e are stationary
# and dif3NS_o & dif3IR_o are stationary
# AND diffNS_o & diffIR_o are close to stationary

# EVEN RUNS
# let's check mean and variance
mean(diffNS_e) # 0.06072951
mean(diffIR_e) # 0.0725145
var(diffNS_e) # 0.1257533
var(diffIR_e) # 0.0873107

# check autocorrelation
acfNS_e <- acf(diffNS_e, plot = TRUE) # 0 sig lags
acfIR_e <- acf(diffIR_e, plot = TRUE) # 1 sig lags (@ lag4, seems random)

# F-test - variance
var.test(diffNS_e, diffIR_e) # NO SIGNIFICANT DIFFERENCE IN VARIANCE

# t.test - means
t.test(diffNS_e, diffIR_e, var.equal = TRUE) # NO SIGNIFICANT DIFFERENCE in MEANS
  # var.equal = TRUE specified due to F-test finding no difference
  # var.equal = FALSE also finds no significant difference

# trend (that was removed?)
t <- 1:length(NS_e)
modNS_e <- lm(NS_e ~ t)
summary(modNS_e)

modIR_e <- lm(IR_e ~ t)
summary(modIR_e)

# do these trends differ? Welch's t-test
b_NS <- summary(modNS_e)$coefficients["t", "Estimate"]
se_NS <- summary(modNS_e)$coefficients["t", "Std. Error"]

b_IR <- summary(modIR_e)$coefficients["t", "Estimate"]
se_IR <- summary(modIR_e)$coefficients["t", "Std. Error"]

b_diff <- b_IR - b_NS
se_diff <- sqrt(se_IR^2 + se_NS^2)

# calculate t-stat and p value
t_stat <- b_diff / se_diff
p_value <- 2 * (1 - pnorm(abs(t_stat)))
p_value  # 0.3593233 - # NO SIGNIFICANT DIFFERENCE IN TRENDS



# ODD RUNS - 3rd order differencing?
# let's check mean and variance
mean(dif3NS_o) # 0.02877715
mean(dif3IR_o) # -0.001083714
var(dif3NS_o) # 1.144709
var(dif3IR_o) # 0.9271075

# check autocorrelation
acfNS_o <- acf(dif3NS_o, plot = TRUE) # 1st lag is signif
acfIR_o <- acf(dif3IR_o, plot = TRUE) # 1st lag is signif, 2 more significant

# F-test - variance
var.test(dif3NS_o, dif3IR_o) # NO SIGNIFICANT DIFFERENCE IN VARIANCE

# t.test - means
t.test(dif3NS_o, dif3IR_o, var.equal = TRUE) # NO SIGNIFICANT DIFFERENCE IN MEANS
  # var.equal = TRUE specified due to F-test finding no difference
  # var.equal = FALSE also finds no significant difference

# trend (that was removed?)
t <- 1:length(NS_o)
modNS_o <- lm(NS_o ~ poly(t,3))
summary(modNS_o)

modIR_o <- lm(IR_o ~ poly(t,3))
summary(modIR_o)

# do these trends differ? Welch's t-test
b_NS <- summary(modNS_o)$coefficients["poly(t, 3)1", "Estimate"]
se_NS <- summary(modNS_o)$coefficients["poly(t, 3)1", "Std. Error"]

b_IR <- summary(modIR_o)$coefficients["poly(t, 3)1", "Estimate"]
se_IR <- summary(modIR_o)$coefficients["poly(t, 3)1", "Std. Error"]

b_diff <- b_IR - b_NS
se_diff <- sqrt(se_IR^2 + se_NS^2)

# calculate t-stat and p value
t_stat <- b_diff / se_diff
p_value <- 2 * (1 - pnorm(abs(t_stat)))
p_value  # 0.005148745 - # SIGNIFICANT DIFFERENCE IN TRENDS


# ODD RUN - 1st order differencing (close to being stationary)
# let's check mean and variance
mean(diffNS_o) # 0.05492173
mean(diffIR_o) # 0.07704178
var(diffNS_o) # 0.1615338
var(diffIR_o) # 0.200944

# check autocorrelation
acfNS_o <- acf(diffNS_o, plot = TRUE) # 1 sig lags (@ lag5, seems random)
acfIR_o <- acf(diffIR_o, plot = TRUE) # 1 sig lags (@ lag5, seems random)

# F-test - variance
var.test(diffNS_o, diffIR_o) # NO SIGNIFICANT DIFFERENCE IN VARIANCE

# t.test - means
t.test(diffNS_o, diffIR_o, var.equal = TRUE) # NO SIGNIFICANT DIFFERENCE IN MEANS
  # var.equal = TRUE specified due to F-test finding no difference
  # var.equal = FALSE also finds no significant difference

# trend (that was removed?)
t <- 1:length(NS_o)
modNS_o <- lm(NS_o ~ t)
summary(modNS_o)

modIR_o <- lm(IR_o ~ t)
summary(modIR_o)

# do these trends differ? Welch's t-test
b_NS <- summary(modNS_o)$coefficients["t", "Estimate"]
se_NS <- summary(modNS_o)$coefficients["t", "Std. Error"]

b_IR <- summary(modIR_o)$coefficients["t", "Estimate"]
se_IR <- summary(modIR_o)$coefficients["t", "Std. Error"]

b_diff <- b_IR - b_NS
se_diff <- sqrt(se_IR^2 + se_NS^2)

# calculate t-stat and p value
t_stat <- b_diff / se_diff
p_value <- 2 * (1 - pnorm(abs(t_stat)))
p_value  # 0.007193675 - # SIGNIFICANT DIFFERENCE IN TRENDS