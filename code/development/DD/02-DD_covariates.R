library(here) # set workind directory

# set loc
here::i_am("code/development/DD/02-DD_covariates.R")
options(max.print=2000)

# load data 
# pinkies
load(here("data", "clean", "DD_pinksE_scst.Rda"))
load(here("data", "clean", "DD_pinksO_scst.Rda"))

# pdo
load(here("data", "clean", "pdoE.Rda"))
load(here("data", "clean", "pdoO.Rda"))

# observers
load(here("data", "clean", "NSEout_observerE.Rda"))
load(here("data", "clean", "NSEout_observerO.Rda"))
