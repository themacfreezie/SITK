## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(dplyr)
# library(gghighlight)
# library(ggplot2)
# library(gplots)
library(here)
# library(lmtest)
# library(plm)
library(panelr)
library(readxl)
library(tidyverse)
# library(tseries)

# create working dir and output folder
here::i_am("code/adfgobserver_mini.R")
options(max.print=10000)

# escapement data (IR & ADFG)
# pull in data
pinks.df <- read_excel(here("data", "adfg_pink.xlsx"), col_names = TRUE)
indianr.df <- read_excel(here("data", "IndianRiver.xlsx"), sheet = "Stopha 2015 - Table 6", col_names = TRUE)

# rename vars in indianr.df to match pinks.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "PEAK_COUNT"
names(indianr.df)[names(indianr.df) == "PinkRUN"] <- "RUN"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# add stream and ID to indianr.df
indianr.df$STREAM <- "Indian River"
indianr.df$STREAM_NO <- "113-41-019"

# drop all districts in pinks.df besides 113
pinks.df <- pinks.df %>% filter(District=="113")

# drop extraneous variables from pinks.df
pinks.df <- pinks.df[-c(2, 5:9)]

# append pinks.df and indianr.df
pinks.df <- rbind(pinks.df, indianr.df)
pinks.df <- pinks.df[-c(3:4)]

# grab observer data
observer.df <- read_excel(here("data", "Pink Salmon Index Counts_from OceanAK.xlsx"), sheet = "Observers", col_names = TRUE)

# drop all districts in observer.df besides 113
observer.df <- observer.df %>% filter(District=="113")

# drop extraneous variables from observer.df
observer.df <- observer.df[-c(2, 4:9, 11)]

# rename stream_no to match pinks.df
names(pinks.df)[names(pinks.df) == "STREAM_NO"] <- "STREAMID"
names(observer.df)[names(observer.df) == "Stream Number"] <- "STREAMID"
names(observer.df)[names(observer.df) == "Year"] <- "YEAR"

# merge
merge.df <- merge(pinks.df, observer.df, by=c("YEAR","STREAMID"), all = "TRUE")
wobserverE.df <- wobserverE.df %>% replace_na("Unknown")

# create unique observer id
merge.df <- merge.df %>%
  group_by(Observer) %>%
  mutate(ID = cur_group_id())
mergeID.df <- merge.df[-c(3)]

# data transform - split into odd/even runs
observerE.df <- mergeID.df %>% filter(YEAR %% 2 == 0)
observerO.df <- mergeID.df %>% filter(YEAR %% 2 != 0)

# set data wide (rows = IDs, columns = year)
wobserverE.df <- panel_data(observerE.df, id = STREAMID, wave = YEAR)
wobserverE.df <- widen_panel(wobserverE.df, separator = "_")

wobserverO.df <- panel_data(observerO.df, id = STREAMID, wave = YEAR)
wobserverO.df <- widen_panel(wobserverO.df, separator = "_")

# save wide dataframes
save(wobserverE.df, file=here("data", "wobserverE_mini.Rda"))
save(wobserverO.df, file=here("data", "wobserverO_mini.Rda"))
