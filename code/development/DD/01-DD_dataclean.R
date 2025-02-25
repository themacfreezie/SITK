## SET WORKING DIR & PACKAGES

# import packages
library(here)
library(readxl)
library(tidyverse)

# create working dir and output folder
here::i_am("code/development/DD/01-DD_dataclean.R")
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
indianr.df <- read_excel(here("data", "raw", "IndianRiver.xlsx"), sheet = "Stopha 2015 - Table 6", col_names = TRUE)

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

## if desired, drop extraneous regions from pinks.df
pinksE_sc.df <- pinksE_sc.df %>% filter(SUB_REGION!="SSE")
pinksE_sc.df <- pinksE_sc.df %>% filter(SUB_REGION!="NSE Inside")
pinksO_sc.df <- pinksO_sc.df %>% filter(SUB_REGION!="SSE")
pinksO_sc.df <- pinksO_sc.df %>% filter(SUB_REGION!="NSE Inside")

# retaining Indian River and seven nearby streams
DD_pinksE_sc.df <- pinksE_sc.df %>% filter(STREAMID=="113-41-042" |  
                                           STREAMID=="113-41-032" |
                                           STREAMID=="113-44-003" |
                                           STREAMID=="113-41-019" |
                                           STREAMID=="113-41-034" |  
                                           STREAMID=="113-42-002" |
                                           STREAMID=="113-42-003" |
                                           STREAMID=="113-43-002" |
                                           STREAMID=="113-43-005" |
                                           STREAMID=="113-44-005" )

DD_pinksO_sc.df <- pinksO_sc.df %>% filter(STREAMID=="113-41-042" |  
                                           STREAMID=="113-41-032" |
                                           STREAMID=="113-44-003" |
                                           STREAMID=="113-41-019" |
                                           STREAMID=="113-41-034" |  
                                           STREAMID=="113-42-002" |
                                           STREAMID=="113-42-003" |
                                           STREAMID=="113-43-002" |
                                           STREAMID=="113-43-005" |
                                           STREAMID=="113-44-005" )

# drop extraneous variables from pinks.df
DD_pinksE_sc.df <- DD_pinksE_sc.df[-c(3:8)]
DD_pinksO_sc.df <- DD_pinksO_sc.df[-c(3:8)]

# natural log of count variable
DD_pinksE_sc.df$ct <- DD_pinksE_sc.df$ESCbyKM + 1
DD_pinksO_sc.df$ct <- DD_pinksO_sc.df$ESCbyKM + 1

DD_pinksE_sc.df$ct <- log(DD_pinksE_sc.df$ct)
DD_pinksO_sc.df$ct <- log(DD_pinksO_sc.df$ct)

# save mean and sd by stream
DD_stream_mctE <- DD_pinksE_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate_at(vars(ct),
            funs(mean_ct = mean(., na.rm = TRUE),
                 sd_ct = sd(., na.rm = TRUE)))
DD_stream_mctE <- DD_stream_mctE %>%
  group_by(STREAMID) %>%
  summarize(mean = mean(mean_ct),
            sd = mean(sd_ct))
DD_stream_mctE

DD_stream_mctO <- DD_pinksO_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate_at(vars(ct),
            funs(mean_ct = mean(., na.rm = TRUE),
                 sd_ct = sd(., na.rm = TRUE)))
DD_stream_mctO <- DD_stream_mctO %>%
  group_by(STREAMID) %>%
  summarize(mean = mean(mean_ct),
            sd = mean(sd_ct))
DD_stream_mctO

save(DD_stream_mctE, file=here("data", "clean", "DD_stream_mctE.Rda"))
save(DD_stream_mctO, file=here("data", "clean", "DD_stream_mctO.Rda"))

#standardize ln(ct)
DD_pinksE_scst.df <- DD_pinksE_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate(standard_ct=scale(ct))
DD_pinksE_scst.df <- DD_pinksE_scst.df[-c(3, 4)]

DD_pinksO_scst.df <- DD_pinksO_sc.df %>% 
  group_by(STREAMID) %>% 
  mutate(standard_ct=scale(ct))
DD_pinksO_scst.df <- DD_pinksO_scst.df[-c(3, 4)]

#generating dummy variable for IR
DD_pinksE_scst.df$dIR <- ifelse(DD_pinksE_scst.df$STREAMID=="113-41-019", 1, 0)
DD_pinksO_scst.df$dIR <- ifelse(DD_pinksO_scst.df$STREAMID=="113-41-019", 1, 0)

#generating dummy variables for treatment
DD_pinksE_scst.df$dPost <- ifelse(DD_pinksE_scst.df$YEAR>1980, 1, 0)
DD_pinksO_scst.df$dPost <- ifelse(DD_pinksO_scst.df$YEAR>1980, 1, 0)

#drop obs with no count (IR)
DD_pinksE_scst.df <- na.omit(DD_pinksE_scst.df)
DD_pinksO_scst.df <- na.omit(DD_pinksO_scst.df)

#drop obs after last IR observation
# DD_pinksE_scst.df <- DD_pinksE_scst.df %>% filter(YEAR<=2016)
# DD_pinksO_scst.df <- DD_pinksO_scst.df %>% filter(YEAR<=2015)
  # not sure if this is necessary

# save scaled dataframes
save(DD_pinksE_scst.df, file=here("data", "clean", "DD_pinksE_scst.Rda"))
save(DD_pinksO_scst.df, file=here("data", "clean", "DD_pinksO_scst.Rda"))