## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/Betas/03-modelbuild.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "WbetaE_DFGob.Rda"))
load(here("data", "clean", "WbetaO_DFGob.Rda"))

# pdo data - from pdo_clean
load(here("data", "clean", "WpdoE.Rda"))
load(here("data", "clean", "WpdoO.Rda"))

WpdoE <- WpdoE[-c(1:9)]
WpdoO <- WpdoO[-c(1:8)]
  # needs more data for larger IR time series
  # ignoring for now

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

# specify matrices for MARSS models - no betas
# u.model <- matrix(paste0("u", seq(2)))
# u.model <- "zero"
  # what I maybe want is similar to b? a 0 for the hatchery and a parameter for the river?
# u.model <- matrix(c(0, "Ur"), 2, 1)
q.model <- "diagonal and unequal"
z.model <- "identity"
a.model <- "zero"
r.model <- "diagonal and unequal"
x.model <- "unequal"
v.model <- "zero"

# beta matrix
b.model <- matrix(list(0), 2, 2)
b.model[1,2] <- "b12"
b.model[2,1] <- "b21"
b.model[2,2] <- "b22"

# state trend
u.model <- matrix(list(0), 2, 1)
u.model[2,1] <- "u"

### NEEDS SMOLT RELEASES FROM t-1 in cC_t

# # c matrix
# c.model <- matrix("pdo", 2, 1)

# could also include observers for IR (no such info for hatchery)

# specify MARSS model
model.list <- list(
  B = b.model 
  , U = u.model
  , Q = q.model
  , Z = z.model 
  , A = a.model 
  , R = r.model
  , x0 = x.model 
  , V0 = v.model 
  , tinitx = 0
  # , C = c.model 
  # , c = pdoE
  # , D = d.model
  # , d = gobbers
)

# specify initial conditions
inits.E <- list(x0 = matrix(c(9.938505, 8.5749326), nrow = 2))
inits.O <- list(x0 = matrix(c(11.7200578, 9.4813332), nrow = 2))
  ##  average of first 5 obs? - ask mark

# run modelos
if(!file.exists(here("data", "clean", "ssEbeta_DFGob.rds"))){
  ssEbeta_DFGob <- MARSS(datE_DFGob, model = model.list, inits = inits.E, method = "kem")
  saveRDS(ssEbeta_DFGob, file=here("data", "clean", "ssEbeta_DFGob.rds"))
}

if(!file.exists(here("data", "clean", "ssObeta_DFGob.rds"))){
  ssObeta_DFGob <- MARSS(datO_DFGob, model = model.list, inits = inits.O, method = "kem")
  saveRDS(ssObeta_DFGob, file=here("data", "clean", "ssObeta_DFGob.rds"))
}
