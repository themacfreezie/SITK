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
library (MARSS)
library (marssTMB)
library(openxlsx)
# library (panelr)
# library(plm)
library(readxl)
# library(tidyverse)
library(TMB)
# library(tseries)

# set working dir
here::i_am("code/adfg_pinkMARSS_miniDUMMYCOV.R")
options(max.print=2000)

# 1) AWC stream length data
# pull in data
dummy.df <- read_excel(here("data", "dummy_data.xlsx"), col_names = FALSE)

# load data - from adfg_pinkWIDE
load(here("data", "wpinksE_scst.Rda"))
load(here("data", "wpinksO_scst.Rda"))

# # and standardized data - from adfg_pinkWIDEstandardize
# load(here("data", "wpinksEst.Rda"))
# load(here("data", "wpinksOst.Rda"))

wpinksE_scst.df <- wpinksE_scst.df[542:595, ]
wpinksO_scst.df <- wpinksO_scst.df[542:595, ]

# wpinksEst.df <- wpinksEst.df[236:296, ]
# wpinksOst.df <- wpinksOst.df[236:296, ]

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
dum <- data.matrix(dummy.df[1:ncol(dummy.df)])

# specify matrices for MARSS models
bE.model <- matrix(list(0), nE, nE)
diag(bE.model) <- paste0("b", seq(nE))
bO.model <- matrix(list(0), nO, nO)
diag(bO.model) <- paste0("b", seq(nO))

uE.model <- "zero"
uO.model <- "zero"

qE.model <- "diagonal and unequal"
qO.model <- "diagonal and unequal"

zE.model <- "identity"
zO.model <- "identity"

aE.model <- "zero"
aO.model <- "zero"

rE.model <- "diagonal and equal"
rO.model <- "diagonal and equal"

x0E.model <- "unequal"
x0O.model <- "unequal"

v0E.model <- "zero"
v0O.model <- "zero"

cE.model <- dum
CE.model <- matrix(list(0), nE, nE)
diag(CE.model) <- paste0("c", seq(nE))

model.listE <- list(
  B = bE.model, U = uE.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  C = CE.model, c = cE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0)

model.listO <- list(
  B = bO.model, U = uO.model, Q = qO.model,
  Z = zO.model, A = aO.model, R = rO.model,
  x0 = x0O.model, V0 = v0O.model, tinitx = 0)

# specify MARSS model - ln(counts)
ptm <- proc.time()
ssE <- MARSS(datE, model = model.listE, method = "TMB")
proc.time()[3] - ptm

# specify MARSS model - standardized ln(counts)
# ptm <- proc.time()
# ssE <- MARSS(datEst, model = model.listE, method = "TMB")
# proc.time()[3] - ptm

# let's see those estimates
# ssE$par
# ssE$par[[4]] # betas
# variance estimates
var.est <- ssE$par[[6]] # variance terms
var.est
# Zvar.est <- scale(var.est)
# hist(Zvar.est)

tidy(ssE)