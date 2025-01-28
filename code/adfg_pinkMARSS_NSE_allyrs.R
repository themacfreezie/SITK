## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
library(data.table)
library(dplyr)
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
here::i_am("code/adfg_pinkMARSS_NSE_allyrs.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "NSEwpinksE_scst.Rda"))
load(here("data", "NSEwpinksO_scst.Rda"))

# combine years
newnames <- colnames(wpinksE_scst.df)
oldnames <- colnames(wpinksO_scst.df)

setnames(wpinksO_scst.df, old = oldnames, new = newnames)
wpinks_scst.df <- rbind(wpinksE_scst.df, wpinksO_scst.df)

# load observer data - from adfgobserver_mini
load(here("data", "wobserverE_NSE.Rda"))
load(here("data", "wobserverO_NSE.Rda"))

# grab year lists, # of observations, & pure count data
years <- names(wpinks_scst.df)
years <- years[-1]
years <- substring(years, first=13, last=16)
n <- nrow(wpinks_scst.df)

# convert counts to matrix
dat <- data.matrix(wpinks_scst.df[2:ncol(wpinks_scst.df)])

# drop observer streams missing from peak count data
## why are these missing?
wobserverE.df <- wobserverE.df[c(2:17, 20:24, 26:38, 40, 42:65, 67:73, 75:92, 94:97, 99:102, 104, 106:120, 122:127, 129:143, 145:150, 152:155, 157:162, 164:165, 167:175, 177:178, 180:184, 186:192, 194:198, 201:204, 206:227, 230:233, 235:240, 242:294), ]
wobserverO.df <- wobserverO.df[c(2:17, 20:24, 26:38, 40, 42:65, 67:73, 75:92, 94:97, 99:102, 104, 106:120, 122:127, 129:143, 145:150, 152:155, 157:162, 164:165, 167:175, 177:178, 180:184, 186:192, 194:198, 201:204, 206:227, 230:233, 235:240, 242:294), ]

# combine years - observers
newnames <- colnames(wobserverE.df)
oldnames <- colnames(wobserverO.df)

setnames(wobserverO.df, old = oldnames, new = newnames)
wobserver.df <- rbind(wobserverE.df, wobserverO.df)

# replace missing values 
wobserver.df$ID_1960  <- wobserver.df$ID_1960 %>% replace_na(48)
wobserver.df$ID_1962  <- wobserver.df$ID_1962 %>% replace_na(48)
wobserver.df$ID_1964  <- wobserver.df$ID_1964 %>% replace_na(48)
wobserver.df$ID_1966  <- wobserver.df$ID_1966 %>% replace_na(48)
wobserver.df$ID_1968  <- wobserver.df$ID_1968 %>% replace_na(48)
wobserver.df$ID_1970  <- wobserver.df$ID_1970 %>% replace_na(48)
wobserver.df$ID_1972  <- wobserver.df$ID_1972 %>% replace_na(48)
wobserver.df$ID_1974  <- wobserver.df$ID_1974 %>% replace_na(48)
wobserver.df$ID_1976  <- wobserver.df$ID_1976 %>% replace_na(48)
wobserver.df$ID_1978  <- wobserver.df$ID_1978 %>% replace_na(48)
wobserver.df$ID_1980  <- wobserver.df$ID_1980 %>% replace_na(48)
wobserver.df$ID_1982  <- wobserver.df$ID_1982 %>% replace_na(48)
wobserver.df$ID_1984  <- wobserver.df$ID_1984 %>% replace_na(48)
wobserver.df$ID_1986  <- wobserver.df$ID_1986 %>% replace_na(48)
wobserver.df$ID_1988  <- wobserver.df$ID_1988 %>% replace_na(48)
wobserver.df$ID_1990  <- wobserver.df$ID_1990 %>% replace_na(48)
wobserver.df$ID_1992  <- wobserver.df$ID_1992 %>% replace_na(48)
wobserver.df$ID_1994  <- wobserver.df$ID_1994 %>% replace_na(48)
wobserver.df$ID_1996  <- wobserver.df$ID_1996 %>% replace_na(48)
wobserver.df$ID_1998  <- wobserver.df$ID_1998 %>% replace_na(48)
wobserver.df$ID_2000  <- wobserver.df$ID_2000 %>% replace_na(48)
wobserver.df$ID_2002  <- wobserver.df$ID_2002 %>% replace_na(48)
wobserver.df$ID_2004  <- wobserver.df$ID_2004 %>% replace_na(48)
wobserver.df$ID_2006  <- wobserver.df$ID_2006 %>% replace_na(48)
wobserver.df$ID_2008  <- wobserver.df$ID_2008 %>% replace_na(48)
wobserver.df$ID_2010  <- wobserver.df$ID_2010 %>% replace_na(48)
wobserver.df$ID_2012  <- wobserver.df$ID_2012 %>% replace_na(48)
wobserver.df$ID_2014  <- wobserver.df$ID_2014 %>% replace_na(48)
wobserver.df$ID_2016  <- wobserver.df$ID_2016 %>% replace_na(48)
wobserver.df$ID_2018  <- wobserver.df$ID_2018 %>% replace_na(48)
wobserver.df$ID_2020  <- wobserver.df$ID_2020 %>% replace_na(48)
wobserver.df$ID_2022  <- wobserver.df$ID_2022 %>% replace_na(48)
# stupid brute force solution, there's got to be a better way to do this

# convert observer ID to matrix
obs <- data.matrix(wobserver.df[2:ncol(wobserver.df)])

# specify matrices for MARSS models
b.model <- "identity"
u.model <- matrix(c(paste0("u", seq(n/2))))
q.model <- "diagonal and unequal"
z.model <- rbind(diag(n/2), diag(n/2))
a.model <- "zero"
r.model <- "diagonal and equal"
d.model <- matrix(list(0), n, n)
  diag(d.model) <- paste0("d", seq(n))
x0.model <- "unequal"
v0.model <- "zero"

model.list <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x0.model, V0 = v0.model, tinitx = 0,
  D = d.model, d = obs)

# specify MARSS model
ptm <- proc.time()
ssm <- MARSS(dat, model = model.list, method = "TMB")
proc.time()[3] - ptm
# almost 4 hours!

# let's see those estimates
var.est <- ssm$par[[6]] # variance terms
var.est

strvarALL.df <- data.frame(
  strID <- wpinksE_scst.df$STREAMID,
  var <- var.est
)
colnames(strvarALL.df) <- c("strID","var")

write.csv(strvarALL.df, here("data", "strvarALL_NSE.csv"), row.names = TRUE)

# unrelated to above script
test <- readRDS(here("data", "ssmALL.rds"))
