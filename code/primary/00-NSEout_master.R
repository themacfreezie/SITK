library(here)

here::i_am("code/primary/00-NSEout_master.R")

# nse outside modeling
source(here("code", "primary", "01-adfgIR_scale.R"))
source(here("code", "primary", "02-NSEout_WIDEstandardize.R"))
source(here("code", "primary", "03-NSEout_observer.R"))
source(here("code", "primary", "04-pdo_clean.R"))
source(here("code", "primary", "05-NSEout_modeleval.R"))
source(here("code", "primary", "06-NSEout_output.R"))
