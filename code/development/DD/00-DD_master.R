library(here)

# set loc
here::i_am("code/development/DD/00-DD_master.R")

# nse outside modeling
source(here("code", "development", "DD", "01-DD_dataclean.R"))
source(here("code", "development", "DD", "02-DD_covariates.R"))
source(here("code", "development", "DD", "03-DD_regression.R"))