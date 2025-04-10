## SET WORKING DIR & PACKAGES

# import packages
library(dplyr, warn.conflicts = FALSE)
library(here)
library(panelr)
library(readxl)

# create working dir and output folder
here::i_am("code/primary/02-NSEout_WIDEstandardize.R")
options(max.print=10000)

# pull in data
load(here("data", "clean", "pinksE_sc.Rda"))
load(here("data", "clean", "pinksO_sc.Rda"))

## DATA FILTERING

## if desired, drop extraneous regions from pinks.df
pinksE_sc.df <- pinksE_sc.df %>% filter(SUB_REGION!="SSE")
pinksE_sc.df <- pinksE_sc.df %>% filter(SUB_REGION!="NSE Inside")
pinksO_sc.df <- pinksO_sc.df %>% filter(SUB_REGION!="SSE")
pinksO_sc.df <- pinksO_sc.df %>% filter(SUB_REGION!="NSE Inside")

# drop extraneous variables from pinks.df
pinksE_sc.df <- pinksE_sc.df[-c(3:8)]
pinksO_sc.df <- pinksO_sc.df[-c(3:8)]

# natural log of count variable
pinksE_sc.df$ct <- pinksE_sc.df$ESCbyKM + 1
pinksO_sc.df$ct <- pinksO_sc.df$ESCbyKM + 1

pinksE_sc.df$ct <- log(pinksE_sc.df$ct)
pinksO_sc.df$ct <- log(pinksO_sc.df$ct)

# save mean and sd by stream
stream_mctE <- pinksE_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate_at(vars(ct),
            funs(mean_ct = mean(., na.rm = TRUE),
                 sd_ct = sd(., na.rm = TRUE)))
stream_mctE <- stream_mctE %>%
  group_by(STREAMID) %>%
  summarize(mean = mean(mean_ct),
            sd = mean(sd_ct))
stream_mctE

stream_mctO <- pinksO_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate_at(vars(ct),
            funs(mean_ct = mean(., na.rm = TRUE),
                 sd_ct = sd(., na.rm = TRUE)))
stream_mctO <- stream_mctO %>%
  group_by(STREAMID) %>%
  summarize(mean = mean(mean_ct),
            sd = mean(sd_ct))
stream_mctO

save(stream_mctE, file=here("data", "clean", "stream_mctE.Rda"))
save(stream_mctO, file=here("data", "clean", "stream_mctO.Rda"))

#standardize ln(ct)
pinksE_scst.df <- pinksE_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate(standard_ct=scale(ct))
pinksE_scst.df <- pinksE_scst.df[-c(3, 4)]

pinksO_scst.df <- pinksO_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate(standard_ct=scale(ct))
pinksO_scst.df <- pinksO_scst.df[-c(3, 4)]

# save wide dataframes
save(pinksE_scst.df, file=here("data", "clean", "NSEout_tpinksE_scst.Rda"))
save(pinksO_scst.df, file=here("data", "clean", "NSEout_tpinksO_scst.Rda"))

# set data wide (rows = IDs, columns = year)
wpinksE_scst.df <- panel_data(pinksE_scst.df, id = STREAMID, wave = YEAR)
wpinksE_scst.df <- widen_panel(wpinksE_scst.df, separator = "_")

wpinksO_scst.df <- panel_data(pinksO_scst.df, id = STREAMID, wave = YEAR)
wpinksO_scst.df <- widen_panel(wpinksO_scst.df, separator = "_")

# save wide dataframes
save(wpinksE_scst.df, file=here("data", "clean", "NSEout_wpinksE_scst.Rda"))
save(wpinksO_scst.df, file=here("data", "clean", "NSEout_wpinksO_scst.Rda"))
