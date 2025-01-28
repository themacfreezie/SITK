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
library(readxl)
library(tidyverse)
# library(tseries)

# create working dir and output folder
here::i_am("code/adfgIR_scale.R")
options(max.print=10000)


## DATA FILTERING

# 1) AWC stream length data
# pull in data
streams.df <- read_excel(here("data", "awc_stream.xlsx"), col_names = TRUE)

# drop extraneous variables from streams.df
streams.df <- streams.df[-c(1, 4:7)]

# rename vars in streams.df 
names(streams.df)[names(streams.df) == "SHAPE_Length"] <- "LENGTH"

# create new var w/ streamID substring
streams.df$STREAMID <- substr(streams.df$AWC_CODE, 1, 12)

# drop extraneous variables from streams.df
streams.df <- streams.df[-c(1:2)]

# cut extrameous characters (8 and 12) out of strings for matching
streams.df$STREAMID <- substr(streams.df$STREAMID, 1, 11)

streams.df$STREAMIDstart <- substr(streams.df$STREAMID, 1, 7)
streams.df$STREAMIDend <- substr(streams.df$STREAMID, 9, 11)

streams.df$STREAMID <- paste(streams.df$STREAMIDstart, streams.df$STREAMIDend, sep="")

streams.df <- streams.df[-c(3:4)]

# collapse around streamID
streams.df <- streams.df %>%
  group_by(STREAMID) %>%
  summarize(LENGTH = sum(LENGTH))

# adjust to km
names(streams.df)[names(streams.df) == "LENGTH"] <- "LENGTHm"
streams.df$LENGTHkm <- (streams.df$LENGTHm/1000)

save(streams.df, file=here("data", "stream_length.Rda"))


# 2) escapement data (IR & ADFG)
# pull in data
pinks.df <- read_excel(here("data", "adfg_pink.xlsx"), col_names = TRUE)
indianr.df <- read_excel(here("data", "IndianRiver.xlsx"), sheet = "Stopha 2015 - Table 6", col_names = TRUE)

# rename vars in indianr.df to match pinks.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "PEAK_COUNT"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# add stream and ID to indianr.df
indianr.df$STREAM <- "Indian River"
indianr.df$STREAM_NO <- "113-41-019"
indianr.df$District <- "113"
indianr.df$SUB_REGION <- "NSE Outside"

# drop extraneous variables from pinks.df
pinks.df <- pinks.df[-c(5:7, 9)]

# append pinks.df and indianr.df
pinks.df <- rbind(pinks.df, indianr.df)


# 3) merge stream length data into escapement data by streamID
# rename stream_no to match streams.df
names(pinks.df)[names(pinks.df) == "STREAM_NO"] <- "STREAMID"

# merge
merge.df <- merge(pinks.df, streams.df, by="STREAMID")


# 4) create esc/km and split into two runs
# esc/km
merge.df$ESCbyKM <- (merge.df$PEAK_COUNT/merge.df$LENGTHkm)
pinks.df <- merge.df

# data transform - split into odd/even runs
pinksE_sc.df <- pinks.df %>% filter(YEAR %% 2 == 0)
pinksO_sc.df <- pinks.df %>% filter(YEAR %% 2 != 0)

# save scaled dataframes
save(pinksE_sc.df, file=here("data", "pinksE_sc.Rda"))
save(pinksO_sc.df, file=here("data", "pinksO_sc.Rda"))
