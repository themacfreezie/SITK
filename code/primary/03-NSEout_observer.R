## SET WORKING DIR & PACKAGES

# import packages
library(here)
library(panelr)
library(readxl)
library(tidyverse)

# create working dir and output folder
here::i_am("code/primary/03-NSEout_observer.R")
options(max.print=10000)

# escapement data (IR & ADFG)
# pull in data
pinks.df <- read_excel(here("data", "raw", "adfg_pink.xlsx"), col_names = TRUE)
indianr.df <- read_excel(here("data", "raw", "IndianRiver.xlsx"), sheet = "Stopha 2015 - Table 6", col_names = TRUE)

# rename vars in indianr.df to match pinks.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "PEAK_COUNT"
names(indianr.df)[names(indianr.df) == "PinkRUN"] <- "RUN"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# add stream and ID to indianr.df
indianr.df$STREAM <- "Indian River"
indianr.df$STREAM_NO <- "113-41-019"
indianr.df$District <- "113"
indianr.df$SUB_REGION <- "NSE Outside"

# keep only NSE outer
pinks.df <- pinks.df %>% filter(SUB_REGION!="SSE")
pinks.df <- pinks.df %>% filter(SUB_REGION!="NSE Inside")

# drop extraneous variables from pinks.df
pinks.df <- pinks.df[-c(2, 5:10)]
indianr.df <- indianr.df[-c(2, 5, 6)]

# append pinks.df and indianr.df
pinks.df <- rbind(pinks.df, indianr.df)

# grab observer data
observer.df <- read_excel(here("data", "raw", "Pink Salmon Index Counts_from OceanAK.xlsx"), sheet = "Observers", col_names = TRUE)

# keep only NSE outer
names(observer.df)[names(observer.df) == "Sub Region"] <- "SUB_REGION"
observer.df <- observer.df %>% filter(SUB_REGION!="SSE")
observer.df <- observer.df %>% filter(SUB_REGION!="NSE Inside")

# drop extraneous variables from observer.df
observer.df <- observer.df[-c(2, 4:9, 11)]

# rename stream_no to match pinks.df
names(pinks.df)[names(pinks.df) == "STREAM_NO"] <- "STREAMID"
names(observer.df)[names(observer.df) == "Stream Number"] <- "STREAMID"
names(observer.df)[names(observer.df) == "Year"] <- "YEAR"

# merge
merge.df <- merge(pinks.df, observer.df, by=c("YEAR","STREAMID"), all = "TRUE")
merge.df$Observer <- merge.df$Observer %>% replace_na("Unknown")

# create unique observer id
merge.df <- merge.df %>%
  group_by(Observer) %>%
  mutate(ID = cur_group_id())
mergeID.df <- merge.df[-c(3, 4)]

observer.df <- mergeID.df
save(observer.df, file=here("data", "clean", "NSEout_observer.Rda"))

# data transform - split into odd/even runs
observerE.df <- mergeID.df %>% filter(YEAR %% 2 == 0)
observerO.df <- mergeID.df %>% filter(YEAR %% 2 != 0)

# save dataframes
save(observerE.df, file=here("data", "clean", "NSEout_observerE.Rda"))
save(observerO.df, file=here("data", "clean", "NSEout_observerO.Rda"))

# set data wide (rows = IDs, columns = year)
wobserver.df <- panel_data(observer.df, id = STREAMID, wave = YEAR)
wobserver.df <- widen_panel(wobserver.df, separator = "_")

wobserverE.df <- panel_data(observerE.df, id = STREAMID, wave = YEAR)
wobserverE.df <- widen_panel(wobserverE.df, separator = "_")

wobserverO.df <- panel_data(observerO.df, id = STREAMID, wave = YEAR)
wobserverO.df <- widen_panel(wobserverO.df, separator = "_")

# save wide dataframes
save(wobserver.df, file=here("data", "clean", "NSEout_wobserver.Rda"))
save(wobserverE.df, file=here("data", "clean", "NSEout_wobserverE.Rda"))
save(wobserverO.df, file=here("data", "clean", "NSEout_wobserverO.Rda"))
