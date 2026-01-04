library(here) # set workind directory
library(tidyverse)

# set loc
here::i_am("code/development/data_clean.R")
options(max.print=2000)

## DATA FILTERING

# 1) SJH releases
# pull in data
releases.df <- read_csv(here("data", "raw", "sjh_releases.csv"), col_names = TRUE)

# save SJH releases as .rda
save(releases.df, file=here("data", "clean", "SJHreleases.Rda"))

# 2) Indian river data
indianr.df <- read_excel(here("data", "raw", "IndianRiver.xlsx"), sheet = "Stopha 2015 - Table 6", col_names = TRUE)
