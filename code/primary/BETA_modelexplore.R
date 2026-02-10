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
# needs more data for larger IR time series
# ignoring for now

WsmoltsE <- WsmoltsE[-c(25)]
WsmoltsO <- WsmoltsO[-c(25)]

# grab year lists, # of observations, & pure count data
yearsE <- names(WbetaE_DFGob.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=4, last=7)

yearsO <- names(WbetaO_DFGob.df)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=4, last=7)

# convert counts to matrix
datE_DFGob <- data.matrix(WbetaE_DFGob.df[2:ncol(WbetaE_DFGob.df)])
datO_DFGob <- data.matrix(WbetaO_DFGob.df[2:ncol(WbetaO_DFGob.df)])

# convert pdo data to matrix
pdoE <- data.matrix(WpdoE)
pdoO <- data.matrix(WpdoO)

# convert smolt data to matrix
smoltsE <- data.matrix(WsmoltsE)
smoltsO <- data.matrix(WsmoltsO)

# specify matrices for MARSS models
q.model <- "diagonal and equal"
z.model <- "identity"
a.model <- "zero"
r.model <- "diagonal and unequal"
x.model <- "unequal"
v.model <- "zero"

# autoregressive character - beta matrix
b.model <- matrix(list(0), 2, 2)
b.model[1,2] <- "bHt<Rt-1"
b.model[2,1] <- "bRt<Ht-1"
b.model[2,2] <- "bRR"

# state trend - u
# u.model <- matrix(list(0), 2, 1)
# u.model[2,1] <- "u"
u.model <- "zero"

# c matrix - for them smolts
c.model <- matrix(list(0), 2, 1)
c.model[1,1] <- "smolts"

# c could also include pdo for both 
# d could include observers for IR (no such info for hatchery)

# specify MARSS model
model.listE <- list(
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
  , c = smoltsE
  # , D = d.model
  # , d = IRobserversE
)

model.listO <- list(
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
  , c = smoltsO
  # , D = d.model
  # , d = IRobserversO
)

# # specify initial conditions
# inits.E <- list(x0 = matrix(c(9.938505, 8.5749326), nrow = 2))
# inits.O <- list(x0 = matrix(c(11.7200578, 9.4813332), nrow = 2))
# ##  average of first 5 obs? - ask mark

# run modelos
ssEbeta_DFGob <- MARSS(datE_DFGob, 
                         model = model.listE, 
                         method = "kem")
saveRDS(ssEbeta_DFGob, file=here("data", "clean", "ssEbeta_DFGob.rds"))


ssObeta_DFGob <- MARSS(datO_DFGob, 
                         model = model.listO, 
                         control = list(maxit = 1000),
                         method = "kem")
saveRDS(ssObeta_DFGob, file=here("data", "clean", "ssObeta_DFGob.rds"))