library(here)
library(MARSS)

# set loc
here::i_am("code/primary/06-NSEout_modeleval.R")
options(max.print=2000)

# load models
ssNSE_u1q1r1 <- readRDS(here("data", "clean", "ssNSE_u1q1r1.rds"))
ssNSE_u1q1r2 <- readRDS(here("data", "clean", "ssNSE_u1q1r2.rds"))
ssNSE_u2q1r1 <- readRDS(here("data", "clean", "ssNSE_u2q1r1.rds"))
ssNSE_u2q1r2 <- readRDS(here("data", "clean", "ssNSE_u2q1r2.rds"))
ssNSE_u1q2r1 <- readRDS(here("data", "clean", "ssNSE_u1q2r1.rds"))
ssNSE_u1q2r2 <- readRDS(here("data", "clean", "ssNSE_u1q2r2.rds"))
ssNSE_u2q2r1 <- readRDS(here("data", "clean", "ssNSE_u2q2r1.rds"))
ssNSE_u2q2r2 <- readRDS(here("data", "clean", "ssNSE_u2q2r2.rds"))

# AICc evaluation of competing models
MARSSaic(ssNSE_u1q1r1, output = c("AIC"))
  # 3788.686
MARSSaic(ssNSE_u1q1r2, output = c("AIC"))
  # 3841.958
MARSSaic(ssNSE_u2q1r1, output = c("AIC"))
  # 3794.538
MARSSaic(ssNSE_u2q1r2, output = c("AIC"))
  # 3820.157
MARSSaic(ssNSE_u1q2r1, output = c("AIC"))
  # 3785.795 <- best model
MARSSaic(ssNSE_u1q2r2, output = c("AIC"))
  # 3838.619
MARSSaic(ssNSE_u2q2r1, output = c("AIC"))
  # 3791.588
MARSSaic(ssNSE_u2q2r2, output = c("AIC"))
  # 3835.989

ssNSE <- ssNSE_u1q2r1
  # No process trend (U = "zero")
  # Run level process variance
  # Site level observation variance
saveRDS(ssNSE, file=here("data", "clean", "ssNSE.rds"))
