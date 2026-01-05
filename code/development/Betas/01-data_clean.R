## SET WORKING DIR & PACKAGES

library(here)
library(readxl)
library(tidyverse)

# set loc
here::i_am("code/development/Betas/01-data_clean.R")
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

# dropping and renaming variables - amp
SJHamp.df <- SJHamp.df[-c(2, 3, 5:7)]
names(SJHamp.df)[names(SJHamp.df) == "Brood Year"] <- "brood_year"
names(SJHamp.df)[names(SJHamp.df) == "Number Released"] <- "released"
names(SJHamp.df)[names(SJHamp.df) == "Ocean Survival %"] <- "ocean_surv"

# dropping and renaming variables - adfg
SJHdfg.df <- SJHdfg.df[-c(1, 2, 4:17, 19:26)]
names(SJHdfg.df)[names(SJHdfg.df) == "RETURN_YEAR"] <- "return_year"
names(SJHdfg.df)[names(SJHdfg.df) == "ESCAPE"] <- "DFGescape"
names(SJHdfg.df)[names(SJHdfg.df) == "Total return"] <- "DFGreturners"

# finding returners for amp data
SJHamp.df$return_year <- SJHamp.df$brood_year + 2
SJHamp.df$AMPreturners <- SJHamp.df$released * (SJHamp.df$ocean_surv/100)
SJHamp.df <- SJHamp.df[-c(1:3)]

# merge all returns
returns.df <- merge(SJHamp.df, SJHdfg.df, by="return_year")

# save SJH releases as .rda
save(returns.df, file=here("data", "clean", "SJHreturns.Rda"))

# 2) Indian river data
indianr.df <- read_excel(here("data", "raw", "IndianRiver.xlsx"), sheet = "Stopha 2015 - Table 6", col_names = TRUE)

# rename vars in indianr.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "IRpeak_count"
names(indianr.df)[names(indianr.df) == "YEAR"] <- "return_year"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# drop years prior to 1977
indianr.df <- indianr.df %>% filter(return_year >= 1977)

# 3) merge all into one df
beta_data.df <- merge(returns.df, indianr.df, by="return_year")

# 4) split into even and odd year runs
beta_dataE.df <- beta_data.df %>% filter(return_year %% 2 == 0)
beta_dataO.df <- beta_data.df %>% filter(return_year %% 2 != 0)

# save scaled dataframes
save(beta_dataE.df, file=here("data", "clean", "beta_dataE.Rda"))
save(beta_dataO.df, file=here("data", "clean", "beta_dataO.Rda"))
