library(here)

# set loc
here::i_am("code/primary/00-master.R")

# set preferences
autoplot_06 <- "no"

# nse outside modeling
source(here("code", "primary", "01-NSEout_adfgIR_scale.R"))
source(here("code", "primary", "02-NSEout_WIDEstandardize.R"))
source(here("code", "primary", "03-NSEout_observer.R"))
source(here("code", "primary", "04-pdo_clean.R"))
source(here("code", "primary", "05E-NSEout_modelbuild.R"))
source(here("code", "primary", "06E-NSEout_modeleval.R"))
source(here("code", "primary", "07E-NSEout_output.R"))

# betas modeling
source(here("code", "primary", "08-BETA_data_clean.R"))
source(here("code", "primary", "09-BETA_WIDEstandardize.R"))
source(here("code", "primary", "10-BETA_modelbuild.R"))
source(here("code", "primary", "11-BETA_modeleval.R"))
source(here("code", "primary", "12-BETA_bootstrap.R"))
source(here("code", "primary", "13-BETA_boot_output.R"))

# diff-in-diff modeling
