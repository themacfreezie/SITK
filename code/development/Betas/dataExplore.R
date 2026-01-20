library(here) # set workind directory
library(readxl)
library(tidyverse)

# set loc
here::i_am("code/development/Betas/dataExplore.R")
options(max.print=2000)

# pull in data
SJHdfg.df <- read_excel(here("data", "raw", "SJHpinkdata.xlsx"), col_names = TRUE)

## Figure out what sums to total return
SJHdfg.df$TOTCOMM2 <- SJHdfg.df$SEINE + SJHdfg.df$GILLNET + SJHdfg.df$TROLL + SJHdfg.df$OTHERCOMM
  # TOTCOMM is recorded funky

SJHdfg.df$check <- rowSums(SJHdfg.df[, c("TOTCOMM2",
                                         "CR_CATCH",
                                         "SPORT",
                                         "OTHER",
                                         "BROOD",
                                         "ESCAPE",
                                         "SUBSIS",
                                         "OTHERNONCOMM")],
                           na.rm = TRUE)
SJHdfg.df$check_diff <- SJHdfg.df$`Total return` - SJHdfg.df$check
  # this covers everything comprising ADFG's `Total return``