## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
library(dplyr, warn.conflicts = FALSE)
# library(gghighlight)
# library(ggplot2)
# library(gplots)
library (here)
# library(lmtest)
library (MARSS)
library (panelr)
# library(plm)
library(readxl)
# library(tidyverse)
# library(tseries)

# create working dir and output folder
here::i_am("code/adfg_pinkWIDE.R")
options(max.print=10000)

# pull in data
pinks.df <- read_excel(here("data", "adfg_pink.xlsx"), col_names = TRUE)

## DATA FILTERING

# drop extraneous regions from pinks.df
pinks.df <- pinks.df %>% filter(SUB_REGION!="SSE")

# drop extraneous variables from pinks.df
pinks.df$ct <- pinks.df$PEAK_COUNT
pinks.df <- pinks.df[-c(2, 4:10)]

# data transform - split into odd/even runs
pinksE.df <- pinks.df %>% filter(YEAR %% 2 == 0)
pinksO.df <- pinks.df %>% filter(YEAR %% 2 != 0)

# natural log of count variable
pinksE.df$ct <- pinksE.df$ct + 1
pinksO.df$ct <- pinksO.df$ct + 1

pinksE.df$ct <- log(pinksE.df$ct)
pinksO.df$ct <- log(pinksO.df$ct)

# set data wide (rows = IDs, columns = year)
wpinksE.df <- panel_data(pinksE.df, id = STREAM_NO, wave = YEAR)
wpinksE.df <- widen_panel(wpinksE.df, separator = "_")

wpinksO.df <- panel_data(pinksO.df, id = STREAM_NO, wave = YEAR)
wpinksO.df <- widen_panel(wpinksO.df, separator = "_")

# save wide dataframes
# save wide dataframes
save(wpinksE.df, file=here("data", "wpinksE.Rda"))
save(wpinksO.df, file=here("data", "wpinksO.Rda"))
