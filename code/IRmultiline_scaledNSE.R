## SET WORKING DIR & PACKAGES

# import packages
library(car)
library(cowplot)
library(dplyr)
library(gghighlight)
library(ggplot2)
library(gplots)
library(lmtest)
library(plm)
library(readxl)
library(tidyverse)
library(tseries)

# create working dir and output folder
WD1 <- "C:/Users/thema/OneDrive/Documents/dissertation/ch1/data"
setwd(WD1)

options(max.print=10000)


## DATA FILTERING

# 1) AWC stream length data
# pull in data
streams.df <- read_excel("awc_stream.xlsx", col_names = TRUE)

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


# 2) escapement data (IR & ADFG)
# pull in data
nPinks.df <- read_excel("adfg_pink.xlsx", col_names = TRUE)
indianr.df <- read_excel("IndianRiver.xlsx", sheet = "Stopha 2015 - Table 6", col_names = TRUE)

# rename vars in indianr.df to match nPinks.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "PEAK_COUNT"
names(indianr.df)[names(indianr.df) == "PinkRUN"] <- "RUN"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# add stream and ID to indianr.df
indianr.df$STREAM <- "Indian River"
indianr.df$STREAM_NO <- "113-41-019"

# drop extraneous regions from nPinks.df
nPinks.df <- nPinks.df %>% filter(SUB_REGION!="SSE")

# drop extraneous variables from nPinks.df
nPinks.df <- nPinks.df[-c(2, 5:9)]

# append nPinks.df and indianr.df
nPinks.df <- rbind(nPinks.df, indianr.df)

# data transform - create study year
nPinks.df <- transform(nPinks.df, studyYEAR = (YEAR - 1959))


# 3) merge stream length data into escapement data by streamID
# rename stream_no to match streams.df
names(nPinks.df)[names(nPinks.df) == "STREAM_NO"] <- "STREAMID"

# merge
merge.df <- merge(nPinks.df, streams.df, by="STREAMID")


# 4) create esc/km and split into two runs
# esc/km
names(merge.df)[names(merge.df) == "LENGTH"] <- "LENGTHm"
merge.df$LENGTHkm <- (merge.df$LENGTHm/1000)
merge.df$ESCbyKM <- (merge.df$PEAK_COUNT/merge.df$LENGTHkm)
nPinks.df <- merge.df

# data transform - split into odd/even runs
nPinksE.df <- nPinks.df %>% filter(YEAR %% 2 == 0)
nPinksO.df <- nPinks.df %>% filter(YEAR %% 2 != 0)

# clean up studyYEAR for new datasets
nPinksE.df$studyYEAR <- ((nPinksE.df$studyYEAR + 1)/2)
nPinksO.df$studyYEAR <- (nPinksO.df$studyYEAR/2)


## CHARTING

# Highlight Indian River on charts of regional escapement
print(ggplot(nPinksE.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("N. SEAK - Even Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

print(ggplot(nPinksO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("N. SEAK - Odd Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

# Dismissing some strange outliers and recharting
nPinksEr.df <- nPinksE.df %>% filter(STREAMID!="113-22-015")
## Whale Bay Grt Arm Hd - "heads" may be measured weird: look into it
nPinksEr.df <- nPinksEr.df %>% filter(STREAMID!="113-81-003")
## Goulding Harbor Hd - "heads" may be measured weird: look into it
nPinksEr.df <- nPinksEr.df %>% filter(STREAMID!="113-43-002")
## Nakwasina River - I believe 113-43-002 & -001 are the same system and should be combined
nPinksEr.df <- nPinksEr.df %>% filter(STREAMID!="113-73-010")
## Slocum Arm Head - "heads" may be measured weird: look into it
nPinksEr.df <- nPinksEr.df %>% filter(STREAMID!="112-73-020")
## Hood Bay S Arm Head - "heads" may be measured weird: look into it
nPinksEr.df <- nPinksEr.df %>% filter(STREAMID!="111-32-080")
## Turner Lake Outlet - measures outlet from a lake: could pinks be laying eggs on streams running into lake?
nPinksEr.df <- nPinksEr.df %>% filter(STREAMID!="109-10-009")
## Lovers Cove Creek - inconsistent listing b/w AWC (LLC = 109-10-012) and escapement numbers

nPinksOr.df <- nPinksO.df %>% filter(STREAMID!="113-22-015")
nPinksOr.df <- nPinksOr.df %>% filter(STREAMID!="113-81-003")
nPinksOr.df <- nPinksOr.df %>% filter(STREAMID!="113-43-002")
nPinksOr.df <- nPinksOr.df %>% filter(STREAMID!="113-73-010")
nPinksOr.df <- nPinksOr.df %>% filter(STREAMID!="112-73-020")
nPinksOr.df <- nPinksOr.df %>% filter(STREAMID!="111-32-080")
nPinksOr.df <- nPinksOr.df %>% filter(STREAMID!="109-10-009")

# Recharting regional escapement w/ selected omissions
print(ggplot(nPinksEr.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("N. SEAK - Even Year Runs (no suspicious data)") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

print(ggplot(nPinksOr.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("N. SEAK - Odd Year Runs (no suspicious data)") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

# Dismissing additional potentially outlying systems and recharting
nPinksEr2.df <- nPinksEr.df %>% filter(STREAMID!="113-66-003")
## Marine Cove - no obvious issue here
nPinksEr2.df <- nPinksEr2.df %>% filter(STREAMID!="112-21-006")
## Ralph's Creek Kelp B - no obvious issue here
nPinksEr2.df <- nPinksEr2.df %>% filter(STREAMID!="115-31-038")
## Sullivan Creek - no obvious issue here

nPinksOr2.df <- nPinksOr.df %>% filter(STREAMID!="113-66-003")
nPinksOr2.df <- nPinksOr2.df %>% filter(STREAMID!="112-21-006")
nPinksOr2.df <- nPinksOr2.df %>% filter(STREAMID!="115-31-038")

# Recharting regional escapement w/ selected omissions
print(ggplot(nPinksEr2.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("N. SEAK - Even Year Runs (no 'outliers')") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

print(ggplot(nPinksOr2.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("N. SEAK - Odd Year Runs (no 'outliers')") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

# charting just indian river
irPinksE.df <- nPinksE.df %>% filter(STREAMID=="113-41-019")
irPinksO.df <- nPinksO.df %>% filter(STREAMID=="113-41-019")

print(ggplot(irPinksE.df, aes(x=YEAR, y=ESCbyKM)) +
        ggtitle("Indian River - Even Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line())

print(ggplot(irPinksO.df, aes(x=YEAR, y=ESCbyKM)) +
        ggtitle("Indian River - Odd Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line())