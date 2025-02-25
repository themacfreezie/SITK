library(here)

here::i_am("code/MARSS_NSEmaster.R")

# nse modeling
source(here("code", "adfgIR_scale.R"))
source(here("code", "adfg_pinkWIDEstandardize.R"))
source(here("code", "adfgobserver_NSE.R"))
source(here("code", "adfg_pinkMARSS_NSE.R"))

# nse-inner modeling