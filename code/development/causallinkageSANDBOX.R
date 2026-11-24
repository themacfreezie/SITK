## SET WORKING DIR & PACKAGES

library(here)
library(readxl)
library(tidyverse)

# set loc
here::i_am("code/development/causallinkageSANDBOX.R")
options(max.print=2000)

## DATA FILTERING

# 1) SJH releases
# there are 3 possible sources for hatchery returns 
#   - AMP release * ocean survival rate
#   - ADFG report total returns (corresponds closely with AMP release * ocean survival rate)
#   - ADFG report ESCAPE (escapement to watershed - seems low/underreported)

# pull in data
SJHamp.df <- read_csv(here("data", "raw", "sjh_releases.csv"), col_names = TRUE)
SJHdfg.df <- read_excel(here("data", "raw", "SJHpinkdata.xlsx"), col_names = TRUE)

# manip amp data
SJHamp.df$releases <- SJHamp.df$`Number Released`
SJHamp.df$fwSurv <- (SJHamp.df$`Freshwater Survival %`/100)
SJHamp.df$ocSurv <- (SJHamp.df$`Ocean Survival %`/100)

SJHamp.df <- SJHamp.df[-c(4, 7, 8)]

SJHamp.df$test_RO <- SJHamp.df$releases*SJHamp.df$ocSurv
SJHamp.df$test_RFO <- SJHamp.df$releases*SJHamp.df$ocSurv*SJHamp.df$fwSurv

SJHamp.df$RETURN_YEAR <- SJHamp.df$`Brood Year` + 2

# dfg releases
SJHdfgRETURNS.df <- SJHdfg.df[-c(1, 2, 4:26)]
SJHdfgRETURNS.df$returns <- SJHdfgRETURNS.df$`Total return`
SJHdfgRETURNS.df <- SJHdfgRETURNS.df[-c(2)]
SJHdfgRETURNS.df$RETURN_YEAR <- as.numeric(SJHdfgRETURNS.df$RETURN_YEAR) 

SJHamp.df <- left_join(SJHamp.df, SJHdfgRETURNS.df, by = "RETURN_YEAR")

# check correlation b/w test estimates and DFG returns
SJHampCORTEST.df <- na.omit(SJHamp.df)
cor(SJHampCORTEST.df$returns, SJHampCORTEST.df$test_RO)
  # 0.96978
cor(SJHampCORTEST.df$returns, SJHampCORTEST.df$test_RFO)
  # 0.95467
  # so RO seems good...

# commercial harvest percentage
SJHdfg.df$TOTCOMM2 <- SJHdfg.df$SEINE + SJHdfg.df$GILLNET + SJHdfg.df$TROLL + SJHdfg.df$OTHERCOMM
  # TOTCOMM is recorded funky
SJHdfg.df$harvest <- rowSums(SJHdfg.df[, c( "TOTCOMM2",
                                            # "CR_CATCH", # cost recovery - known
                                            "SPORT",
                                            "OTHER",
                                            # "BROOD", # broodstock - known
                                            # "ESCAPE", # estimated escapement
                                            "SUBSIS",
                                            "OTHERNONCOMM")],
                              na.rm = TRUE)
SJHdfgHARVEST.df <- SJHdfg.df[-c(1, 2, 4:28)]
SJHdfgHARVEST.df$RETURN_YEAR <- as.numeric(SJHdfgHARVEST.df$RETURN_YEAR) 
SJHamp.df <- left_join(SJHamp.df, SJHdfgHARVEST.df, by = "RETURN_YEAR")

SJHamp.df$harvest_pct <- SJHamp.df$harvest / SJHamp.df$test_RO

# SJHamp.df$TEST <- SJHamp.df$test_RO*(1 - SJHamp.df$harvest_pct)
#   # this would be the same as just subtracting the harvest from test_RO

# Indian river data
indianr.df <- read_excel(here("data", "raw", "SITKA AREA HISTORIC PINK ESCAPEMENTS.xlsx"), sheet = "Sitka Sound", col_names = TRUE)

# drop other streams and extra rows from indianr.df
indianr.df <- indianr.df[-c(2:13, 15:20)]
indianr.df <- indianr.df[-c(1, 67:76), ]

# rename vars in indianr.df to match pinks.df
names(indianr.df)[names(indianr.df) == "11341019"] <- "IRpeak_count"
names(indianr.df)[names(indianr.df) == "...1"] <- "RETURN_YEAR"

indianr.df$IRpeak_count <- as.numeric(indianr.df$IRpeak_count)
indianr.df$RETURN_YEAR <- as.numeric(indianr.df$RETURN_YEAR)

# drop years prior to 1977
indianr.df <- indianr.df %>% filter(RETURN_YEAR >= 1977)

# merge in IR counts
SJHamp.df <- left_join(SJHamp.df, indianr.df, by = "RETURN_YEAR")

SJHamp.df$SJHeggs <- SJHamp.df$`Egg Take`
SJHamp.df$SJHfwSurv <- SJHamp.df$fwSurv
SJHamp.df$SJHocSurv <- SJHamp.df$ocSurv
SJHamp.df$SJHreleases <- SJHamp.df$releases
SJHamp.df$SJHreportedReturns <- SJHamp.df$returns
SJHamp.df$SJHcalculatedReturns <- round(SJHamp.df$SJHreleases * SJHamp.df$SJHocSurv)
SJHamp.df$SJHharvest <- SJHamp.df$harvest
SJHamp.df$SJHharvest_pct <- SJHamp.df$harvest_pct

SJHamp.df <- SJHamp.df[-c(2:10, 12:14)]

