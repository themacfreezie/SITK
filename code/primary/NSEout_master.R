library(here)

here::i_am("code/primary/NSEout_master.R")

# nse outside modeling
source(here("code", "01-adfgIR_scale.R"))
source(here("code", "02-NSEout_WIDEstandardize.R"))
source(here("code", "03-NSEout_observer.R"))
source(here("code", "NSEout_MARSSeo.R"))
source(here("code", "NSEout_MARSSall.R"))
source(here("code", "NSEout_output.R"))
