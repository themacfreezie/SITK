library(here)

here::i_am("code/NSEout_master.R")

# nse outside modeling
source(here("code", "adfgIR_scale.R"))
source(here("code", "NSEout_WIDEstandardize.R"))
source(here("code", "NSEout_observer.R"))
source(here("code", "NSEout_MARSSeo.R"))
source(here("code", "NSEout_MARSSall.R"))
source(here("code", "NSEout_output.R"))
