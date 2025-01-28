## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
library(data.table)
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
here::i_am("code/NSEout_variance.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "pinksE_sc.Rda"))
load(here("data", "pinksO_sc.Rda"))

## DATA FILTERING

## if desired, drop extraneous regions from pinks.df
pinksE_sc.df <- pinksE_sc.df %>% filter(SUB_REGION!="SSE")
pinksE_sc.df <- pinksE_sc.df %>% filter(SUB_REGION!="NSE Inside")
pinksO_sc.df <- pinksO_sc.df %>% filter(SUB_REGION!="SSE")
pinksO_sc.df <- pinksO_sc.df %>% filter(SUB_REGION!="NSE Inside")

# drop extraneous variables from pinks.df
pinksE_sc.df <- pinksE_sc.df[-c(3:9)]
pinksO_sc.df <- pinksO_sc.df[-c(3:9)]

# drop indian river
pinksE_sc.df <- pinksE_sc.df %>% filter(STREAMID!="113-41-019")
pinksO_sc.df <- pinksO_sc.df %>% filter(STREAMID!="113-41-019")

# natural log of count variable
pinksE_sc.df$ct <- pinksE_sc.df$ESCbyKM + 1
pinksO_sc.df$ct <- pinksO_sc.df$ESCbyKM + 1

pinksE_sc.df$ct <- log(pinksE_sc.df$ct)
pinksO_sc.df$ct <- log(pinksO_sc.df$ct)

# standarize counts by stream
pinksE_scst.df <- pinksE_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate(standard_ct=scale(ct))
pinksE_scst.df <- pinksE_scst.df[-c(3, 4)]

pinksO_scst.df <- pinksO_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate(standard_ct=scale(ct))
pinksO_scst.df <- pinksO_scst.df[-c(3, 4)]

# create data sets for pre and post 1990
pinksE_scst1990post <- pinksE_scst.df %>%  filter(YEAR > 1990)
pinksO_scst1990post <- pinksO_scst.df %>%  filter(YEAR > 1990)

pinksE_scst1990pre <- pinksE_scst.df %>%  filter(YEAR <= 1990)
pinksO_scst1990pre <- pinksO_scst.df %>%  filter(YEAR <= 1990)

# variance by stream
varEpre1990 <- setDT(pinksE_scst1990pre)[, list(pre1990var=var(rep(standard_ct))), by = STREAMID]
varEpos1990 <- setDT(pinksE_scst1990post)[, list(pos1990var=var(rep(standard_ct))), by = STREAMID]

varOpre1990 <- setDT(pinksO_scst1990pre)[, list(pre1990var=var(rep(standard_ct))), by = STREAMID]
varOpos1990 <- setDT(pinksO_scst1990post)[, list(pos1990var=var(rep(standard_ct))), by = STREAMID]

varEprepos <- merge(varEpre1990, varEpos1990, by="STREAMID")
varOprepos <- merge(varOpre1990, varOpos1990, by="STREAMID")

varEprepos$diff <- (varEprepos$pos1990var - varEprepos$pre1990var)
varOprepos$diff <- (varOprepos$pos1990var - varOprepos$pre1990var)
