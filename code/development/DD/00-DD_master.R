library(here)

# set loc
here::i_am("00-DD_master.R")

# nse outside modeling
source(here("01-DD_dataclean.R"))
source(here("02-DD_covariates.R"))
source(here("03-DD_regression.R"))