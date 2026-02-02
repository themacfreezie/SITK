## SET WORKING DIR & PACKAGES

# import packages
library(here)
library(readxl)
library(tidyverse)

# create working dir and output folder
here::i_am("code/primary/01-NSEout_adfgIR_scale.R")
options(max.print=10000)


## DATA FILTERING

# 1) AWC stream length data
# pull in data
streams.df <- read_excel(here("data", "raw", "awc_stream.xlsx"), col_names = TRUE)

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

save(streams.df, file=here("data", "clean", "stream_length.Rda"))


# 2) escapement data (IR & ADFG)
# pull in data
pinks.df <- read_excel(here("data", "raw", "adfg_pink.xlsx"), col_names = TRUE)
indianr.df <- read_excel(here("data", "raw", "SITKA AREA HISTORIC PINK ESCAPEMENTS.xlsx"), sheet = "Sitka Sound", col_names = TRUE)

# drop other streams and extra rows from indianr.df
indianr.df <- indianr.df[-c(2:13, 15:20)]
indianr.df <- indianr.df[-c(1, 67:76), ]

# rename vars in indianr.df to match pinks.df
names(indianr.df)[names(indianr.df) == "11341019"] <- "PEAK_COUNT"
names(indianr.df)[names(indianr.df) == "...1"] <- "YEAR"

indianr.df$PEAK_COUNT <- as.numeric(indianr.df$PEAK_COUNT)
indianr.df$YEAR <- as.numeric(indianr.df$YEAR)

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
save(pinksE_sc.df, file=here("data", "clean", "pinksE_sc.Rda"))
save(pinksO_sc.df, file=here("data", "clean", "pinksO_sc.Rda"))
