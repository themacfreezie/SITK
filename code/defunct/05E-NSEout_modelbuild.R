library(here) # set workind directory
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/05E-NSEout_modelbuild.R")
options(max.print=2000)

# load data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinksE_scst.Rda"))
# observer data - from NSEout_observer
load(here("data", "clean", "NSEout_wobserverE.Rda"))
# pdo data - from pdo_clean
load(here("data", "clean", "WpdoE.Rda"))

# grab year lists, # of observations, & pure count data
yearsE <- names(wpinksE_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
nE <- nrow(wpinksE_scst.df)

# convert counts to matrix
datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])

# grab IR data for output
IRdata <- as.numeric(as.vector(datE[6,]))
saveRDS(IRdata, file=here("data", "clean", "IRdataE.rds"))

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

# convert observer ID and pdo data to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
pdoE <- data.matrix(WpdoE)

# specify matrices for MARSS models
bE.model <- "identity"
# uE.model <- matrix(
#   c("u1", "u2"),
#   nrow = 2,
#   ncol = 1,
#   byrow = TRUE
# )
uEa.model <- "zero"
uEb.model <- matrix(paste0("u", seq(nE)))
qE.model <- "diagonal and equal"
zE.model <- "identity"
cE.model <- matrix("pdo", nE, 1)
aE.model <- "zero"
# rE.model <- "diagonal and equal"
rE.model <- "equalvarcov" 
# will compare with AIC
dE.model <- matrix(list(0), nE, nE)
diag(dE.model) <- paste0("d", seq(nE))
x0E.model <- "unequal"
v0E.model <- "zero"

model.listEa <- list(
  B = bE.model, U = uEa.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  C= cE.model, c = pdoE, D = dE.model, d = obsE)

model.listEb <- list(
  B = bE.model, U = uEb.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  C= cE.model, c = pdoE, D = dE.model, d = obsE)

# specify MARSS model
ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEa.rds"))){
  ssEa <- MARSS(datE, model = model.listEa, method = "kem")
  saveRDS(ssEa, file=here("data", "clean", "ssEa.rds"))
}
if(!file.exists(here("data", "clean", "ssEb.rds"))){
  ssEb <- MARSS(datE, model = model.listEb, method = "kem")
  saveRDS(ssEb, file=here("data", "clean", "ssEb.rds"))
}
proc.time()[3] - ptm

zEa.model <- "identity"
uEbb.model <- matrix(paste0("u", seq(2)))
cEb.model <- matrix("pdo", 2, 1)
zEb.model <- matrix(
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

model.listEba <- list(
  B = bE.model, U = uEb.model, Q = qE.model,
  Z = zEa.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  C= cE.model, c = pdoE, D = dE.model, d = obsE)

model.listEbb <- list(
  B = bE.model, U = uEbb.model, Q = qE.model,
  Z = zEb.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  C= cEb.model, c = pdoE, D = dE.model, d = obsE)

# specify MARSS model
ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEba.rds"))){
  ssEba <- MARSS(datE, model = model.listEba, method = "kem")
  saveRDS(ssEba, file=here("data", "clean", "ssEba.rds"))
}
if(!file.exists(here("data", "clean", "ssEbb.rds"))){
  ssEbb <- MARSS(datE, model = model.listEbb, method = "kem")
  saveRDS(ssEbb, file=here("data", "clean", "ssEbb.rds"))
}
proc.time()[3] - ptm
