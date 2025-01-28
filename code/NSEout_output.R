## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(data.table)
# library(dplyr)
# library(gghighlight)
# library(ggplot2)
# library(gplots)
library(here)
# library(lmtest)
# library(MARSS)
# library(marssTMB)
# library(openxlsx)
# library (panelr)
# library(plm)
# library(readxl)
# library(tidyverse)
# library(TMB)
# library(tseries)

# set working dir
here::i_am("code/NSEout_output.R")
options(max.print=2000)

strvarEOA.df <- data.frame(
  strID <- wpinksE_scst.df$STREAMID,
  varE <- varE.est,
  varO <- varO.est,
  varA <- varA.est
)
colnames(strvarEOA.df) <- c("strID","varE", "varO", "varA")

write.csv(strvarEOA.df, here("data", "NSEout_var.csv"), row.names = TRUE)
