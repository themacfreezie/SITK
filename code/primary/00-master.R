library(here)

# set loc
here::i_am("code/primary/00-master.R")

# set preferences
autoplot_06 <- "no"

# nse outside modeling
source(here("code", "primary", "01-adfgIR_scale.R"))
source(here("code", "primary", "02-NSEout_WIDEstandardize.R"))
source(here("code", "primary", "03-NSEout_observer.R"))
source(here("code", "primary", "04-pdo_clean.R"))
source(here("code", "primary", "05E-NSEout_modelbuild.R"))
source(here("code", "primary", "06E-NSEout_modeleval.R"))
source(here("code", "primary", "07E-NSEout_output.R"))

# ir modeling
source(here("code", "primary", "08a-strayrates_clean.R"))
source(here("code", "primary", "08b-IR_statetransform.R"))
source(here("code", "primary", "08c-strayrates_compare.R"))
source(here("code", "primary", "08d-strayrates_timing.R"))
source(here("code", "primary", "09a-precip_clean.R"))
source(here("code", "primary", "09b-temp_clean.R"))
