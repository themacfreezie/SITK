## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(dplyr)
# library(gghighlight)
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
here::i_am("code/adfg_pinkMARSS_NSE.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "NSEwpinksE_scst.Rda"))
load(here("data", "NSEwpinksO_scst.Rda"))

# load observer data - from adfgobserver_mini
load(here("data", "NSEwobserverE.Rda"))
load(here("data", "NSEwobserverO.Rda"))

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
wobserverE.df <- wobserverE.df[c(2:17, 20:24, 26:38, 40, 42:65, 67:73, 75:92, 94:97, 99:102, 104, 106:120, 122:127, 129:143, 145:150, 152:155, 157:162, 164:165, 167:175, 177:178, 180:184, 186:192, 194:198, 201:204, 206:227, 230:233, 235:240, 242:294), ]
wobserverO.df <- wobserverO.df[c(2:17, 20:24, 26:38, 40, 42:65, 67:73, 75:92, 94:97, 99:102, 104, 106:120, 122:127, 129:143, 145:150, 152:155, 157:162, 164:165, 167:175, 177:178, 180:184, 186:192, 194:198, 201:204, 206:227, 230:233, 235:240, 242:294), ]

# replace missing values 
wobserverE.df$ID_1960  <- wobserverE.df$ID_1960 %>% replace_na(48)
wobserverE.df$ID_1962  <- wobserverE.df$ID_1962 %>% replace_na(48)
wobserverE.df$ID_1964  <- wobserverE.df$ID_1964 %>% replace_na(48)
wobserverE.df$ID_1966  <- wobserverE.df$ID_1966 %>% replace_na(48)
wobserverE.df$ID_1968  <- wobserverE.df$ID_1968 %>% replace_na(48)
wobserverE.df$ID_1970  <- wobserverE.df$ID_1970 %>% replace_na(48)
wobserverE.df$ID_1972  <- wobserverE.df$ID_1972 %>% replace_na(48)
wobserverE.df$ID_1974  <- wobserverE.df$ID_1974 %>% replace_na(48)
wobserverE.df$ID_1976  <- wobserverE.df$ID_1976 %>% replace_na(48)
wobserverE.df$ID_1978  <- wobserverE.df$ID_1978 %>% replace_na(48)
wobserverE.df$ID_1980  <- wobserverE.df$ID_1980 %>% replace_na(48)
wobserverE.df$ID_1982  <- wobserverE.df$ID_1982 %>% replace_na(48)
wobserverE.df$ID_1984  <- wobserverE.df$ID_1984 %>% replace_na(48)
wobserverE.df$ID_1986  <- wobserverE.df$ID_1986 %>% replace_na(48)
wobserverE.df$ID_1988  <- wobserverE.df$ID_1988 %>% replace_na(48)
wobserverE.df$ID_1990  <- wobserverE.df$ID_1990 %>% replace_na(48)
wobserverE.df$ID_1992  <- wobserverE.df$ID_1992 %>% replace_na(48)
wobserverE.df$ID_1994  <- wobserverE.df$ID_1994 %>% replace_na(48)
wobserverE.df$ID_1996  <- wobserverE.df$ID_1996 %>% replace_na(48)
wobserverE.df$ID_1998  <- wobserverE.df$ID_1998 %>% replace_na(48)
wobserverE.df$ID_2000  <- wobserverE.df$ID_2000 %>% replace_na(48)
wobserverE.df$ID_2002  <- wobserverE.df$ID_2002 %>% replace_na(48)
wobserverE.df$ID_2004  <- wobserverE.df$ID_2004 %>% replace_na(48)
wobserverE.df$ID_2006  <- wobserverE.df$ID_2006 %>% replace_na(48)
wobserverE.df$ID_2008  <- wobserverE.df$ID_2008 %>% replace_na(48)
wobserverE.df$ID_2010  <- wobserverE.df$ID_2010 %>% replace_na(48)
wobserverE.df$ID_2012  <- wobserverE.df$ID_2012 %>% replace_na(48)
wobserverE.df$ID_2014  <- wobserverE.df$ID_2014 %>% replace_na(48)
wobserverE.df$ID_2016  <- wobserverE.df$ID_2016 %>% replace_na(48)
wobserverE.df$ID_2018  <- wobserverE.df$ID_2018 %>% replace_na(48)
wobserverE.df$ID_2020  <- wobserverE.df$ID_2020 %>% replace_na(48)
wobserverE.df$ID_2022  <- wobserverE.df$ID_2022 %>% replace_na(48)

wobserverO.df$ID_1961  <- wobserverO.df$ID_1961 %>% replace_na(48)
wobserverO.df$ID_1963  <- wobserverO.df$ID_1963 %>% replace_na(48)
wobserverO.df$ID_1965  <- wobserverO.df$ID_1965 %>% replace_na(48)
wobserverO.df$ID_1967  <- wobserverO.df$ID_1967 %>% replace_na(48)
wobserverO.df$ID_1969  <- wobserverO.df$ID_1969 %>% replace_na(48)
wobserverO.df$ID_1971  <- wobserverO.df$ID_1971 %>% replace_na(48)
wobserverO.df$ID_1973  <- wobserverO.df$ID_1973 %>% replace_na(48)
wobserverO.df$ID_1975  <- wobserverO.df$ID_1975 %>% replace_na(48)
wobserverO.df$ID_1977  <- wobserverO.df$ID_1977 %>% replace_na(48)
wobserverO.df$ID_1979  <- wobserverO.df$ID_1979 %>% replace_na(48)
wobserverO.df$ID_1981  <- wobserverO.df$ID_1981 %>% replace_na(48)
wobserverO.df$ID_1983  <- wobserverO.df$ID_1983 %>% replace_na(48)
wobserverO.df$ID_1985  <- wobserverO.df$ID_1985 %>% replace_na(48)
wobserverO.df$ID_1987  <- wobserverO.df$ID_1987 %>% replace_na(48)
wobserverO.df$ID_1989  <- wobserverO.df$ID_1989 %>% replace_na(48)
wobserverO.df$ID_1991  <- wobserverO.df$ID_1991 %>% replace_na(48)
wobserverO.df$ID_1993  <- wobserverO.df$ID_1993 %>% replace_na(48)
wobserverO.df$ID_1995  <- wobserverO.df$ID_1995 %>% replace_na(48)
wobserverO.df$ID_1997  <- wobserverO.df$ID_1997 %>% replace_na(48)
wobserverO.df$ID_1999  <- wobserverO.df$ID_1999 %>% replace_na(48)
wobserverO.df$ID_2001  <- wobserverO.df$ID_2001 %>% replace_na(48)
wobserverO.df$ID_2003  <- wobserverO.df$ID_2003 %>% replace_na(48)
wobserverO.df$ID_2005  <- wobserverO.df$ID_2005 %>% replace_na(48)
wobserverO.df$ID_2007  <- wobserverO.df$ID_2007 %>% replace_na(48)
wobserverO.df$ID_2009  <- wobserverO.df$ID_2009 %>% replace_na(48)
wobserverO.df$ID_2011  <- wobserverO.df$ID_2011 %>% replace_na(48)
wobserverO.df$ID_2013  <- wobserverO.df$ID_2013 %>% replace_na(48)
wobserverO.df$ID_2015  <- wobserverO.df$ID_2015 %>% replace_na(48)
wobserverO.df$ID_2017  <- wobserverO.df$ID_2017 %>% replace_na(48)
wobserverO.df$ID_2019  <- wobserverO.df$ID_2019 %>% replace_na(48)
wobserverO.df$ID_2021  <- wobserverO.df$ID_2021 %>% replace_na(48)
wobserverO.df$ID_2023  <- wobserverO.df$ID_2023 %>% replace_na(48)
# stupid brute force solution, there's got to be a better way to do this

# convert observer ID to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
obsO <- data.matrix(wobserverO.df[2:ncol(wobserverO.df)])

# specify matrices for MARSS models
bE.model <- "identity"
bO.model <- "identity"

uE.model <- matrix(c(paste0("u", seq(nE))))
uO.model <- matrix(c(paste0("u", seq(nO))))

qE.model <- "diagonal and unequal"
qO.model <- "diagonal and unequal"

zE.model <- "identity"
zO.model <- "identity"

aE.model <- "zero"
aO.model <- "zero"

rE.model <- "diagonal and equal"
rO.model <- "diagonal and equal"

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
  #almost 1.5 hours to run

ptm <- proc.time()
ssO <- MARSS(datO, model = model.listO, method = "TMB")
proc.time()[3] - ptm

# let's see those estimates
varE.est <- ssE$par[[6]] # variance terms
varE.est

varO.est <- ssO$par[[6]] # variance terms
varO.est

strvarEO.df <- data.frame(
  strID <- wpinksE_scst.df$STREAMID,
  varE <- varE.est,
  varO <- varO.est
)
colnames(strvarEO.df) <- c("strID","varE", "varO")

strvarEO.df$sclvarE <- scale(strvarEO.df$varE)
strvarEO.df$sclvarO <- scale(strvarEO.df$varO)

write.csv(strvarEO.df, here("data", "strvarEO_NSE.csv"), row.names = TRUE)