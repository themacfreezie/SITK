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
# library (MARSS)
library (marssTMB)
# library (panelr)
# library(plm)
# library(readxl)
# library(tidyverse)
library(TMB)
# library(tseries)

# set working dir
here::i_am("code/adfg_pinkMARSS.R")
options(max.print=2000)

# load data
load(here("data", "wpinksE_scst.Rda"))
load(here("data", "wpinksO_scst.Rda"))

# grab year lists, # of observations, & pure count data
yearsE <- names(wpinksE_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
nE <- nrow(wpinksE_scst.df)

yearsO <- names(wpinksO_scst.df)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=13, last=16)
nO <- nrow(wpinksO_scst.df)

datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])
datO <- data.matrix(wpinksO_scst.df[2:ncol(wpinksO_scst.df)])

# specify matrices for MARSS models

bE.model <- matrix(list(0), nE, nE)
diag(bE.model) <- paste0("b", seq(nE))
bO.model <- matrix(list(0), nO, nO)
diag(bO.model) <- paste0("b", seq(nO))

uE.model <- "zero"
uO.model <- "zero"

qE.model <- "diagonal and equal"
qO.model <- "diagonal and equal"

zE.model <- matrix(list(0), nE, nE)
diag(zE.model) <- paste0("z", seq(nE))
zO.model <- matrix(list(0), nO, nO)
diag(zO.model) <- paste0("z", seq(nO))

aE.model <- "zero"
aO.model <- "zero"

rE.model <- "diagonal and equal"
rO.model <- "diagonal and equal"

model.listE <- list(
  B = bE.model, U = uE.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  tinitx = 0)

model.listO <- list(
  B = bO.model, U = uO.model, Q = qO.model,
  Z = zO.model, A = aO.model, R = rO.model,
  tinitx = 0)

# specify MARSS model
ptm <- proc.time()
foo=MARSS(datE, model = model.listE, fit=FALSE)
summary(foo$model)
proc.time()[3] - ptm

ptm <- proc.time()
ssE <- MARSS(datE, model = model.listE, method = "TMB")
summary(ssE$model)
saveRDS(ssE, file = here("data", "ssE_model.rda"))
proc.time()[3] - ptm
