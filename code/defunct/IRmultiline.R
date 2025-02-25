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

# pull in data
pinks.df <- read_excel("adfg_pink.xlsx", col_names = TRUE)
indianr.df <- read_excel("IndianRiver.xlsx", sheet = "Stopha 2015 - Table 6", col_names = TRUE)

# rename vars in indianr.df to match pinks.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "PEAK_COUNT"
names(indianr.df)[names(indianr.df) == "PinkRUN"] <- "RUN"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# add stream to indianr.df
indianr.df$STREAM <- "Indian River"

# drop extraneous regions from pinks.df
pinks.df <- pinks.df %>% filter(SUB_REGION=="NSE Outside")

# drop extraneous variables from pinks.df
pinks.df <- pinks.df[-c(2:3, 5:9)]

# append pinks.df and indianr.df
pinks.df <- rbind(pinks.df, indianr.df)

# data transform - create study year
pinks.df <- transform(pinks.df, studyYEAR = (YEAR - 1959))

# data transform - split into odd/even runs
pinksE.df <- pinks.df %>% filter(YEAR %% 2 == 0)
pinksO.df <- pinks.df %>% filter(YEAR %% 2 != 0)

# clean up studyYEAR for new datasets
pinksE.df$studyYEAR <- ((pinksE.df$studyYEAR + 1)/2)
pinksO.df$studyYEAR <- (pinksO.df$studyYEAR/2)


## CHARTING

# Highlight Indian River on charts of regional escapement
print(ggplot(pinksE.df, aes(x=YEAR, y=PEAK_COUNT, group=STREAM)) +
        ggtitle("N. SEAK Outer - Even Year Runs") +
        xlab("Year") + ylab("Peak Count") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

print(ggplot(pinksO.df, aes(x=YEAR, y=PEAK_COUNT, group=STREAM)) +
        ggtitle("N. SEAK Outer - Odd Year Runs") +
        xlab("Year") + ylab("Peak Count") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

