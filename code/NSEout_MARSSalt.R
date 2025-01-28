## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
library(data.table)
library(dplyr)
library(gghighlight)
library(ggplot2)
# library(gplots)
library(here)
# library(lmtest)
library(MARSS)
library(marssTMB)
library(openxlsx)
# library (panelr)
# library(plm)
# library(readxl)
library(tidyverse)
library(TMB)
# library(tseries)

# set working dir
here::i_am("code/NSEout_MARSSalt.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "NSEout_wpinksE_scst.Rda"))

# load observer data - from adfgobserver_mini
load(here("data", "NSEout_wobserverE.Rda"))

# grab year lists, # of observations, & pure count data
yearsE <- names(wpinksE_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
nE <- nrow(wpinksE_scst.df)

# convert counts to matrix
datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])

# drop observer streams missing from peak count data
## why are these missing?
wobserverE.df <- wobserverE.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

# replace missing values 
wobserverE.df$ID_1960  <- wobserverE.df$ID_1960 %>% replace_na(11)
wobserverE.df$ID_1962  <- wobserverE.df$ID_1962 %>% replace_na(11)
wobserverE.df$ID_1964  <- wobserverE.df$ID_1964 %>% replace_na(11)
wobserverE.df$ID_1966  <- wobserverE.df$ID_1966 %>% replace_na(11)
wobserverE.df$ID_1968  <- wobserverE.df$ID_1968 %>% replace_na(11)
wobserverE.df$ID_1970  <- wobserverE.df$ID_1970 %>% replace_na(11)
wobserverE.df$ID_1972  <- wobserverE.df$ID_1972 %>% replace_na(11)
wobserverE.df$ID_1974  <- wobserverE.df$ID_1974 %>% replace_na(11)
wobserverE.df$ID_1976  <- wobserverE.df$ID_1976 %>% replace_na(11)
wobserverE.df$ID_1978  <- wobserverE.df$ID_1978 %>% replace_na(11)
wobserverE.df$ID_1980  <- wobserverE.df$ID_1980 %>% replace_na(11)
wobserverE.df$ID_1982  <- wobserverE.df$ID_1982 %>% replace_na(11)
wobserverE.df$ID_1984  <- wobserverE.df$ID_1984 %>% replace_na(11)
wobserverE.df$ID_1986  <- wobserverE.df$ID_1986 %>% replace_na(11)
wobserverE.df$ID_1988  <- wobserverE.df$ID_1988 %>% replace_na(11)
wobserverE.df$ID_1990  <- wobserverE.df$ID_1990 %>% replace_na(11)
wobserverE.df$ID_1992  <- wobserverE.df$ID_1992 %>% replace_na(11)
wobserverE.df$ID_1994  <- wobserverE.df$ID_1994 %>% replace_na(11)
wobserverE.df$ID_1996  <- wobserverE.df$ID_1996 %>% replace_na(11)
wobserverE.df$ID_1998  <- wobserverE.df$ID_1998 %>% replace_na(11)
wobserverE.df$ID_2000  <- wobserverE.df$ID_2000 %>% replace_na(11)
wobserverE.df$ID_2002  <- wobserverE.df$ID_2002 %>% replace_na(11)
wobserverE.df$ID_2004  <- wobserverE.df$ID_2004 %>% replace_na(11)
wobserverE.df$ID_2006  <- wobserverE.df$ID_2006 %>% replace_na(11)
wobserverE.df$ID_2008  <- wobserverE.df$ID_2008 %>% replace_na(11)
wobserverE.df$ID_2010  <- wobserverE.df$ID_2010 %>% replace_na(11)
wobserverE.df$ID_2012  <- wobserverE.df$ID_2012 %>% replace_na(11)
wobserverE.df$ID_2014  <- wobserverE.df$ID_2014 %>% replace_na(11)
wobserverE.df$ID_2016  <- wobserverE.df$ID_2016 %>% replace_na(11)
wobserverE.df$ID_2018  <- wobserverE.df$ID_2018 %>% replace_na(11)
wobserverE.df$ID_2020  <- wobserverE.df$ID_2020 %>% replace_na(11)
wobserverE.df$ID_2022  <- wobserverE.df$ID_2022 %>% replace_na(11)
# stupid brute force solution, there's got to be a better way to do this

# convert observer ID to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])

# specify matrices for MARSS models
bE.model <- "identity"

# uE.model <- matrix(
#   c("u1", "u2"),
#   nrow = 2,
#   ncol = 1,
#   byrow = TRUE
# )
uE.model <- "zero"

qE.model <- "diagonal and equal"

# zE.model <- "identity"
zE.model <- matrix(
  c(1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    0, 1,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0),
  nrow = 36,
  ncol = 2,
  byrow = TRUE
)
# zE.model <- matrix(1, nE, 1)

aE.model <- "zero"

# rE.model <- "diagonal and equal"
rE.model <- "equalvarcov" 
  # will compare with AIC

dE.model <- matrix(list(0), nE, nE)
diag(dE.model) <- paste0("d", seq(nE))

x0E.model <- "unequal"

v0E.model <- "zero"

model.listE <- list(
  B = bE.model, U = uE.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  D = dE.model, d = obsE)

# specify MARSS model
# ptm <- proc.time()
# ssE <- MARSS(datE, model = model.listE, method = "TMB")
# proc.time()[3] - ptm

ptm <- proc.time()
ssE <- MARSS(datE, model = model.listE, method = "kem")
proc.time()[3] - ptm

# let's see those estimates
fitted(ssE)
# autoplot(ssE)

#grabbing data for figures
statesE.est <- ssE$states
statesEse.est <- ssE$states.se
statesE.est
statesEse.est

# plot variance terms
statesE.df <- as.data.frame(statesE.est)
statesEse.df <- as.data.frame(statesEse.est)

statesLong <- statesE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")

statesLong <- statesLong %>% 
  mutate(Year = c(1960, 1962, 1964, 1966, 1968,
                  1970, 1972, 1974, 1976, 1978,
                  1980, 1982, 1984, 1986, 1988,
                  1990, 1992, 1994, 1996, 1998,
                  2000, 2002, 2004, 2006, 2008,
                  2010, 2012, 2014, 2016, 2018,
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
                  1970, 1972, 1974, 1976, 1978,
                  1980, 1982, 1984, 1986, 1988,
                  1990, 1992, 1994, 1996, 1998,
                  2000, 2002, 2004, 2006, 2008,
                  2010, 2012, 2014, 2016, 2018,
                  2020, 2022))

statesLong <- statesLong %>% 
  mutate(state = c("Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River",
                   "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer",
                   "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer",
                   "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer",
                   "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer",
                   "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer",
                   "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer", "NSE Outer",
                   "NSE Outer", "NSE Outer"))

statesseLong <- statesEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLong <- statesLong %>% 
  mutate(se = statesseLong$se)

statesLong$ub = statesLong$fitted + statesLong$se
statesLong$lb = statesLong$fitted - statesLong$se

# plotting
ggplot(data = statesLong, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.4) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = TRUE, unhighlighted_params = list(color="gray70")) +
  xlab("") +
  ylab("State Estimate (standardized)") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
