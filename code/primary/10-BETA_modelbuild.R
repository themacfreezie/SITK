## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(tidyverse)
library(zoo)

# set loc
here::i_am("code/primary/BETA_modelexplore.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "WbetaE_DFGob.Rda"))
load(here("data", "clean", "WbetaO_DFGob.Rda"))

# pdo data - from pdo_clean
load(here("data", "clean", "WpdoE.Rda"))
load(here("data", "clean", "WpdoO.Rda"))

load(here("data", "clean", "WsmoltsE.Rda"))
load(here("data", "clean", "WsmoltsO.Rda"))

WpdoE <- WpdoE[-c(1:9)]
WpdoO <- WpdoO[-c(1:8)]

WsmoltsE <- WsmoltsE[-c(25)]
WsmoltsO <- WsmoltsO[-c(25)]

# grab year lists, # of observations, & pure count data
yearsE <- names(WbetaE_DFGob.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=4, last=7)

yearsO <- names(WbetaO_DFGob.df)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=4, last=7)

# test
# WbetaO_DFGob.df[is.na(WbetaO_DFGob.df)] <- 7.5

# convert counts to matrix
datE_DFGob <- data.matrix(WbetaE_DFGob.df[2:ncol(WbetaE_DFGob.df)])
datO_DFGob <- data.matrix(WbetaO_DFGob.df[2:ncol(WbetaO_DFGob.df)])

# convert pdo data to matrix
pdoE <- data.matrix(WpdoE)
pdoO <- data.matrix(WpdoO)

# convert smolt data to matrix
smoltsE <- data.matrix(WsmoltsE)
smoltsO <- data.matrix(WsmoltsO)

# combining datasets into one model
# dat
new_names <- c("site_run", 
               "ct_1", "ct_2", "ct_3", "ct_4", "ct_5", "ct_6",
               "ct_7", "ct_8", "ct_9", "ct_10", "ct_11", "ct_12",
               "ct_13", "ct_14", "ct_15", "ct_16", "ct_17", "ct_18",
               "ct_19", "ct_20", "ct_21", "ct_22", "ct_23", "ct_24")
names(WbetaE_DFGob.df) <- new_names
names(WbetaO_DFGob.df) <- new_names
WbetaE_DFGob.df$site_run <- c("SJ_E", "IR_E")
WbetaO_DFGob.df$site_run <- c("SJ_O", "IR_O")
beta_DFGob.df <- bind_rows(WbetaE_DFGob.df, WbetaO_DFGob.df)
dat_DFGob <- data.matrix(beta_DFGob.df[2:ncol(beta_DFGob.df)])

# smolts
new_names <- c("ct_1", "ct_2", "ct_3", "ct_4", "ct_5", "ct_6",
               "ct_7", "ct_8", "ct_9", "ct_10", "ct_11", "ct_12",
               "ct_13", "ct_14", "ct_15", "ct_16", "ct_17", "ct_18",
               "ct_19", "ct_20", "ct_21", "ct_22", "ct_23", "ct_24")
names(WsmoltsE) <- new_names
names(WsmoltsO) <- new_names
smolts <- bind_rows(WsmoltsE, WsmoltsO)
smolts <- data.matrix(smolts)

# model list
z.model <- "identity"
u.model <- "zero"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"

# site level observation error
r.model <- matrix(list(0), 4, 4)
r.model[1,1] <- "rH"
r.model[2,2] <- "rR"
r.model[3,3] <- "rH"
r.model[4,4] <- "rR"

# autoregressive character - beta matrix
b.model <- matrix(list(0), 4, 4)
b.model[1,2] <- "bE_Ht<Rt-1"
b.model[2,1] <- "bE_Rt<Ht-1"
b.model[2,2] <- "bE_RR"
b.model[3,4] <- "bO_Ht<Rt-1"
b.model[4,3] <- "bO_Rt<Ht-1"
b.model[4,4] <- "bO_RR"

# c matrix - for them smolts
c.model <- matrix(list(0), 4, 2)
c.model[1,1] <- "sE"
c.model[3,2] <- "sO"

# run level observation error
q.model <- matrix(list(0), 4, 4)
q.model[1,1] <- "qE"
q.model[2,2] <- "qE"
q.model[3,3] <- "qO"
q.model[4,4] <- "qO"

model.list <- list(
  B = b.model 
  , U = u.model
  , Q = q.model
  , Z = z.model 
  , A = a.model 
  , R = r.model
  , x0 = x.model 
  , V0 = v.model 
  , tinitx = 1
  , C = c.model
  , c = smolts
)

# run modelos
if(!file.exists(here("data", "clean", "ssbeta_DFGob.rds"))){
  ssbeta_DFGob <- MARSS(dat_DFGob,
                        model = model.list,
                        method = "kem")
  saveRDS(ssbeta_DFGob, file=here("data", "clean", "ssbeta_DFGob.rds"))
}