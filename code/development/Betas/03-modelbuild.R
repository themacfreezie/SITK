## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/Betas/03-modelbuild.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "WbetaE_AMPr.Rda"))
load(here("data", "clean", "WbetaO_AMPr.Rda"))
load(here("data", "clean", "WbetaE_DFGr.Rda"))
load(here("data", "clean", "WbetaO_DFGr.Rda"))
load(here("data", "clean", "WbetaE_DFGe.Rda"))
load(here("data", "clean", "WbetaO_DFGe.Rda"))

# pdo data - from pdo_clean
load(here("data", "clean", "WpdoE.Rda"))
load(here("data", "clean", "WpdoO.Rda"))

WpdoE <- WpdoE[-c(1:9)]
WpdoO <- WpdoO[-c(1:8)]
  # needs more data for larger IR time series

# grab year lists, # of observations, & pure count data
yearsE <- names(WbetaE_AMPr.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=4, last=7)

yearsO <- names(WbetaO_AMPr.df)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=4, last=7)

# convert counts to matrix
datE_AMPr <- data.matrix(WbetaE_AMPr.df[2:ncol(WbetaE_AMPr.df)])
datO_AMPr <- data.matrix(WbetaO_AMPr.df[2:ncol(WbetaO_AMPr.df)])
datE_DFGr <- data.matrix(WbetaE_DFGr.df[2:ncol(WbetaE_DFGr.df)])
datO_DFGr <- data.matrix(WbetaO_DFGr.df[2:ncol(WbetaO_DFGr.df)])
datE_DFGe <- data.matrix(WbetaE_DFGe.df[2:ncol(WbetaE_DFGe.df)])
datO_DFGe <- data.matrix(WbetaO_DFGe.df[2:ncol(WbetaO_DFGe.df)])

# convert pdo data to matrix
pdoE <- data.matrix(WpdoE)
pdoO <- data.matrix(WpdoO)

# specify matrices for MARSS models
b.model <- "unconstrained"
u.model <- matrix(paste0("u", seq(2)))
q.model <- "diagonal and unequal"
z.model <- "identity"
a.model <- "zero"
r1.model <- "diagonal and unequal"
r2.model <- "equalvarcov" 
x.model <- "unequal"
v.model <- "zero"
c.model <- matrix("pdo", 2, 1)

model.list_Ebetar1 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r1.model,
  x0 = x.model, V0 = v.model, tinitx = 0
  # , C= c.model, c = pdoE
  )

model.list_Obetar1 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r1.model,
  x0 = x.model, V0 = v.model, tinitx = 0
  # , C= c.model, c = pdoO
  )

model.list_Ebetar2 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r2.model,
  x0 = x.model, V0 = v.model, tinitx = 0
  # , C= c.model, c = pdoE
)

model.list_Obetar2 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r2.model,
  x0 = x.model, V0 = v.model, tinitx = 0
  # , C= c.model, c = pdoO
)

# specify MARSS models
ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEbeta_AMPr_r1.rds"))){
  ssEbeta_AMPr_r1 <- MARSS(datE_AMPr, model = model.list_Ebetar1, method = "kem")
  saveRDS(ssEbeta_AMPr_r1, file=here("data", "clean", "ssEbeta_AMPr_r1.rds"))
}
if(!file.exists(here("data", "clean", "ssEbeta_AMPr_r2.rds"))){
  ssEbeta_AMPr_r2 <- MARSS(datE_AMPr, model = model.list_Ebetar2, method = "kem")
  saveRDS(ssEbeta_AMPr_r2, file=here("data", "clean", "ssEbeta_AMPr_r2.rds"))
}
proc.time()[3] - ptm

ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEbeta_DFGr_r1.rds"))){
  ssEbeta_DFGr_r1 <- MARSS(datE_DFGr, model = model.list_Ebetar1, method = "kem")
  saveRDS(ssEbeta_DFGr_r1, file=here("data", "clean", "ssEbeta_DFGr_r1.rds"))
}
if(!file.exists(here("data", "clean", "ssEbeta_DFGr_r2.rds"))){
  ssEbeta_DFGr_r2 <- MARSS(datE_DFGr, model = model.list_Ebetar2, method = "kem")
  saveRDS(ssEbeta_DFGr_r2, file=here("data", "clean", "ssEbeta_DFGr_r2.rds"))
}
proc.time()[3] - ptm

ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEbeta_DFGe_r1.rds"))){
  ssEbeta_DFGe_r1 <- MARSS(datE_DFGe, model = model.list_Ebetar1, method = "kem")
  saveRDS(ssEbeta_DFGe_r1, file=here("data", "clean", "ssEbeta_DFGe_r1.rds"))
}
if(!file.exists(here("data", "clean", "ssEbeta_DFGe_r2.rds"))){
  ssEbeta_DFGe_r2 <- MARSS(datE_DFGe, model = model.list_Ebetar2, method = "kem")
  saveRDS(ssEbeta_DFGe_r2, file=here("data", "clean", "ssEbeta_DFGe_r2.rds"))
}
proc.time()[3] - ptm

ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssObeta_AMPr_r1.rds"))){
  ssObeta_AMPr_r1 <- MARSS(datO_AMPr, model = model.list_Obetar1, method = "kem")
  saveRDS(ssObeta_AMPr_r1, file=here("data", "clean", "ssObeta_AMPr_r1.rds"))
}
if(!file.exists(here("data", "clean", "ssObeta_AMPr_r2.rds"))){
  ssObeta_AMPr_r2 <- MARSS(datO_AMPr, model = model.list_Obetar2, method = "kem")
  saveRDS(ssObeta_AMPr_r2, file=here("data", "clean", "ssObeta_AMPr_r2.rds"))
}
proc.time()[3] - ptm

ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssObeta_DFGr_r1.rds"))){
  ssObeta_DFGr_r1 <- MARSS(datO_DFGr, model = model.list_Obetar1, method = "kem")
  saveRDS(ssObeta_DFGr_r1, file=here("data", "clean", "ssObeta_DFGr_r1.rds"))
}
if(!file.exists(here("data", "clean", "ssObeta_DFGr_r2.rds"))){
  ssObeta_DFGr_r2 <- MARSS(datO_DFGr, model = model.list_Obetar2, method = "kem")
  saveRDS(ssObeta_DFGr_r2, file=here("data", "clean", "ssObeta_DFGr_r2.rds"))
}
proc.time()[3] - ptm

ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssObeta_DFGe_r1.rds"))){
  ssObeta_DFGe_r1 <- MARSS(datO_DFGe, model = model.list_Obetar1, method = "kem")
  saveRDS(ssObeta_DFGe_r1, file=here("data", "clean", "ssObeta_DFGe_r1.rds"))
}
if(!file.exists(here("data", "clean", "ssObeta_DFGe_r2.rds"))){
  ssObeta_DFGe_r2 <- MARSS(datO_DFGe, model = model.list_Obetar2, method = "kem")
  saveRDS(ssObeta_DFGe_r2, file=here("data", "clean", "ssObeta_DFGe_r2.rds"))
}
proc.time()[3] - ptm