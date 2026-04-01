update.packages(ask = FALSE, checkBuilt = TRUE)

library(here)

# set loc
here::i_am("code/primary/00-master.R")

# descriptive stats and figures
source(here("code", "primary", "a01-IRmultiline_scaled.R"))

# nse outside modeling
source(here("code", "primary", "a02-NSEout_adfgIR_scale.R"))
source(here("code", "primary", "a03-NSEout_WIDEstandardize.R"))
source(here("code", "primary", "a04-NSEout_observer.R"))
source(here("code", "primary", "a05-pdo_clean.R"))
source(here("code", "primary", "a06-NSEout_modelbuild.R"))
source(here("code", "primary", "a07-NSEout_modeleval.R"))
source(here("code", "primary", "a08-NSEout_output.R"))
source(here("code", "primary", "a09-NSEout_1statemodelcompare.R"))

# diff-in-diff modeling
source(here("code", "primary", "b01-DD_dataclean.R"))
source(here("code", "primary", "b02-DD_covariates.R"))
source(here("code", "primary", "b03-DD_regression.R"))
source(here("code", "primary", "b04-DD_modeleval.R"))
source(here("code", "primary", "b05-DD_eventstudies.R"))
source(here("code", "primary", "b06-DD_syntheticcontrols.R"))
source(here("code", "primary", "b07-DD_screg.R"))
source(here("code", "primary", "b08-DD_output.R"))
source(here("code", "primary", "b09-DD_marssBACI.R"))

# betas modeling
source(here("code", "primary", "c01-BETA_data_clean.R"))
source(here("code", "primary", "c02-BETA_WIDEstandardize.R"))
source(here("code", "primary", "c03-BETA_modelbuild.R"))
source(here("code", "primary", "c04-BETA_modeleval.R"))
source(here("code", "primary", "c05-BETA_bootstrap.R"))
source(here("code", "primary", "c06-BETA_boot_output.R"))