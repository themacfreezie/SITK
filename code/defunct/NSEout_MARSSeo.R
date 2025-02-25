## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
library(data.table)
library(dplyr)
library(gghighlight)
# library(ggplot2)
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
here::i_am("code/NSEout_MARSSeo.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "NSEout_wpinksE_scst.Rda"))
load(here("data", "NSEout_wpinksO_scst.Rda"))

# load observer data - from adfgobserver_mini
load(here("data", "NSEout_wobserverE.Rda"))
load(here("data", "NSEout_wobserverO.Rda"))

# grab year lists, # of observations, & pure count data
yearsE <- names(wpinksE_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
nE <- nrow(wpinksE_scst.df)

yearsO <- names(wpinksO_scst.df)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=13, last=16)
nO <- nrow(wpinksO_scst.df)

# convert counts to matrix
datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])
datO <- data.matrix(wpinksO_scst.df[2:ncol(wpinksO_scst.df)])

# drop observer streams missing from peak count data
## why are these missing?
wobserverE.df <- wobserverE.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]
wobserverO.df <- wobserverO.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

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

wobserverO.df$ID_1961  <- wobserverO.df$ID_1961 %>% replace_na(11)
wobserverO.df$ID_1963  <- wobserverO.df$ID_1963 %>% replace_na(11)
wobserverO.df$ID_1965  <- wobserverO.df$ID_1965 %>% replace_na(11)
wobserverO.df$ID_1967  <- wobserverO.df$ID_1967 %>% replace_na(11)
wobserverO.df$ID_1969  <- wobserverO.df$ID_1969 %>% replace_na(11)
wobserverO.df$ID_1971  <- wobserverO.df$ID_1971 %>% replace_na(11)
wobserverO.df$ID_1973  <- wobserverO.df$ID_1973 %>% replace_na(11)
wobserverO.df$ID_1975  <- wobserverO.df$ID_1975 %>% replace_na(11)
wobserverO.df$ID_1977  <- wobserverO.df$ID_1977 %>% replace_na(11)
wobserverO.df$ID_1979  <- wobserverO.df$ID_1979 %>% replace_na(11)
wobserverO.df$ID_1981  <- wobserverO.df$ID_1981 %>% replace_na(11)
wobserverO.df$ID_1983  <- wobserverO.df$ID_1983 %>% replace_na(11)
wobserverO.df$ID_1985  <- wobserverO.df$ID_1985 %>% replace_na(11)
wobserverO.df$ID_1987  <- wobserverO.df$ID_1987 %>% replace_na(11)
wobserverO.df$ID_1989  <- wobserverO.df$ID_1989 %>% replace_na(11)
wobserverO.df$ID_1991  <- wobserverO.df$ID_1991 %>% replace_na(11)
wobserverO.df$ID_1993  <- wobserverO.df$ID_1993 %>% replace_na(11)
wobserverO.df$ID_1995  <- wobserverO.df$ID_1995 %>% replace_na(11)
wobserverO.df$ID_1997  <- wobserverO.df$ID_1997 %>% replace_na(11)
wobserverO.df$ID_1999  <- wobserverO.df$ID_1999 %>% replace_na(11)
wobserverO.df$ID_2001  <- wobserverO.df$ID_2001 %>% replace_na(11)
wobserverO.df$ID_2003  <- wobserverO.df$ID_2003 %>% replace_na(11)
wobserverO.df$ID_2005  <- wobserverO.df$ID_2005 %>% replace_na(11)
wobserverO.df$ID_2007  <- wobserverO.df$ID_2007 %>% replace_na(11)
wobserverO.df$ID_2009  <- wobserverO.df$ID_2009 %>% replace_na(11)
wobserverO.df$ID_2011  <- wobserverO.df$ID_2011 %>% replace_na(11)
wobserverO.df$ID_2013  <- wobserverO.df$ID_2013 %>% replace_na(11)
wobserverO.df$ID_2015  <- wobserverO.df$ID_2015 %>% replace_na(11)
wobserverO.df$ID_2017  <- wobserverO.df$ID_2017 %>% replace_na(11)
wobserverO.df$ID_2019  <- wobserverO.df$ID_2019 %>% replace_na(11)
wobserverO.df$ID_2021  <- wobserverO.df$ID_2021 %>% replace_na(11)
wobserverO.df$ID_2023  <- wobserverO.df$ID_2023 %>% replace_na(11)
# stupid brute force solution, there's got to be a better way to do this

# convert observer ID to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
obsO <- data.matrix(wobserverO.df[2:ncol(wobserverO.df)])

# specify matrices for MARSS models
bE.model <- "identity"
bO.model <- "identity"

uE.model <- matrix(c(paste0("u", seq(nE))))
uO.model <- matrix(c(paste0("u", seq(nO))))

qE.model <- "diagonal and equal"
qO.model <- "diagonal and equal"

zE.model <- "identity"
zO.model <- "identity"

aE.model <- "zero"
aO.model <- "zero"

rE.model <- "diagonal and equal"
rO.model <- "diagonal and equal"
  # "equalvarcov" 

dE.model <- matrix(list(0), nE, nE)
diag(dE.model) <- paste0("d", seq(nE))
dO.model <- matrix(list(0), nO, nO)
diag(dO.model) <- paste0("d", seq(nO))

x0E.model <- "unequal"
x0O.model <- "unequal"

v0E.model <- "zero"
v0O.model <- "zero"

model.listE <- list(
  B = bE.model, U = uE.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  D = dE.model, d = obsE)

model.listO <- list(
  B = bO.model, U = uO.model, Q = qO.model,
  Z = zO.model, A = aO.model, R = rO.model,
  x0 = x0O.model, V0 = v0O.model, tinitx = 0,
  D = dO.model, d = obsO)

# specify MARSS model
ptm <- proc.time()
ssE <- MARSS(datE, model = model.listE, method = "TMB")
proc.time()[3] - ptm

ptm <- proc.time()
ssO <- MARSS(datO, model = model.listO, method = "TMB")
proc.time()[3] - ptm

# let's see those estimates
varE.est <- ssE$par[[6]] # even year variance terms
statesE.est <- ssE$states
varE.est
statesE.est

varO.est <- ssO$par[[6]] # odd year variance terms
statesO.est <- ssO$states
varO.est
statesO.est

# plot variance terms
varE.df <- as.data.frame(varE.est)
statesE.df <- as.data.frame(statesE.est)

statesE.df <- statesE.df %>% 
  mutate(STREAMID = wpinksE_scst.df$STREAMID)
statesEv.df <- statesE.df %>% 
  mutate(var = varE.df$V1)

statesEv.df <- statesEv.df[order(statesEv.df$var, decreasing = FALSE),]
statesEv.df <- statesEv.df %>% 
  mutate(obnum = c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                   10, 11, 12, 13, 14, 15, 16, 17, 18,
                   19, 20, 21, 22, 23, 24, 25, 26, 27,
                   28, 29, 30, 31, 32, 33, 34, 35, 36))

ggplot(data = statesEv.df, aes(y=var, x=obnum, color = STREAMID)) +
  geom_point(show.legend = FALSE) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x =element_blank())+
  gghighlight(STREAMID == "113-41-019", use_direct_label = FALSE, unhighlighted_params = list(color="gray70")) +
  ylab("Variance, ln(peak Count per kilometer)")

statesLong <- statesE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="state")
statesLong <-  statesLong %>% 
  mutate(year = c(1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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
                  2020, 2022,
                  1960, 1962, 1964, 1966, 1968,
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

ggplot(data = statesLong, aes(y=state, x=year, color = STREAMID)) +
  geom_line(show.legend = FALSE, linewidth = 1.4) +
  theme_classic() +
  gghighlight(STREAMID == "113-41-019", use_direct_label = FALSE, unhighlighted_params = list(color="gray70", linewidth = 0.5)) +
  xlab("Year") +
  ylab("State Estimate") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
