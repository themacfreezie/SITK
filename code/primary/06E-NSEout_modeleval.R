library(here) # set workind directory
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/06E-NSEout_modeleval.R")
options(max.print=2000)

# aic evaluation of competing models
ssEa <- readRDS(here("data", "clean", "ssEa.rds"))
MARSSaic(ssEa, output = c("AIC"))
ssEb <- readRDS(here("data", "clean", "ssEb.rds"))
MARSSaic(ssEb, output = c("AIC"))
  # ssEb is better model

# aic evaluation of competing models
ssEba <- readRDS(here("data", "clean", "ssEba.rds"))
MARSSaic(ssEba, output = c("AIC"))
ssEbb <- readRDS(here("data", "clean", "ssEbb.rds"))
MARSSaic(ssEbb, output = c("AIC"))
  # ssEbb is better model

ssE <- ssEbb
saveRDS(ssE, file=here("data", "clean", "ssE.rds"))
