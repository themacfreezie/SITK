## SET WORKING DIR & PACKAGES

# import packages
library(car)
library(cowplot)
library(data.table)
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

# rename vars in indianr.df to match nPinks.df
names(indianr.df)[names(indianr.df) == "Indian River Peak Escapement"] <- "PEAK_COUNT"
names(indianr.df)[names(indianr.df) == "PinkRUN"] <- "RUN"

# drop run and sitka sound index from indianr.df
indianr.df <- indianr.df[-c(2, 4)]

# add stream and ID to indianr.df
indianr.df$STREAM <- "Indian River"
indianr.df$STREAM_NO <- "113-41-019"

# drop extraneous regions from nPinks.df
pinks.df <- pinks.df %>% filter(SUB_REGION!="SSE")

# drop extraneous variables from nPinks.df
pinks.df <- pinks.df[-c(2, 5:9)]

# append nPinks.df and indianr.df
pinks.df <- rbind(pinks.df, indianr.df)

# data transform - create study year
pinks.df <- transform(pinks.df, studyYEAR = (YEAR - 1959))

# data transform - split into odd/even runs
pinksE.df <- pinks.df %>% filter(YEAR %% 2 == 0)
pinksO.df <- pinks.df %>% filter(YEAR %% 2 != 0)

# clean up studyYEAR for new datasets
pinksE.df$studyYEAR <- ((pinksE.df$studyYEAR + 1)/2)
pinksO.df$studyYEAR <- (pinksO.df$studyYEAR/2)

## THIS IS ROUGH AND READY - NOT A COMPLETIST WAY TO SET UP STOCK/RECRUIT VARS
   # I need to figure out how to lag by group

# create lag variable for stock/recruitment
pinksE.df$RECRUIT <- sapply(1:nrow(pinksE.df), function(x) pinksE.df$PEAK_COUNT[x+1])
pinksE.df$STOCK <- pinksE.df$PEAK_COUNT

pinksO.df$RECRUIT <- sapply(1:nrow(pinksO.df), function(x) pinksO.df$PEAK_COUNT[x+1])
pinksO.df$STOCK <- pinksO.df$PEAK_COUNT

# drop 2022 from dataset as last 'generation' available 
pinksE.df <- pinksE.df %>% filter(studyYEAR!=32)

pinksO.df <- pinksO.df %>% filter(studyYEAR!=32)

# let's try charting
streams <- c(unique(pinksE.df$STREAM))
for(i in streams){
  plot_data.df <- subset(pinksE.df, STREAM == i)
  print(ggplot(plot_data.df, aes(STOCK, RECRUIT)) +
    ggtitle(i, "Even Year Run") +
    xlab("Esc, Year t - 'Stock'") + ylab("Esc, Year t+1 - 'Recruits'") +
    geom_smooth(method="lm", se=FALSE) +
    geom_point())
}

streams <- c(unique(pinksE.df$STREAM))
for(i in streams){
  plot_data.df <- subset(pinksO.df, STREAM == i)
  print(ggplot(plot_data.df, aes(STOCK, RECRUIT)) +
          ggtitle(i, "Odd Year Run") +
          xlab("Esc, Year t - 'Stock'") + ylab("Esc, Year t+1 - 'Recruits'") +
          geom_smooth(method="lm", se=FALSE) +
          geom_point())
}

# regression analysis by stream?
for(i in streams){
  plot_data.df <- subset(pinksE.df, STREAM == i)
  print(i)
  print("Even Year Run")
  print(summary(model <- lm(RECRUIT ~ STOCK, data = plot_data.df)))
}

for(i in streams){
  plot_data.df <- subset(pinksO.df, STREAM == i)
  print(i)
  print("Odd Year Run")
  print(summary(model <- lm(RECRUIT ~ STOCK, data = plot_data.df)))
}

plot_data.df <- subset(pinksE.df, STREAM != "Indian River")

print(ggplot(plot_data.df, aes(STOCK, RECRUIT)) +
        ggtitle("All N Streams EXCEPT Indian River - Even Year Runs") +
        xlab("Esc, Year t - 'Stock'") + ylab("Esc, Year t+1 - 'Recruits'") +
        geom_smooth(method="lm", se=FALSE) +
        geom_point())

print("All N Streams EXCEPT Indian River - Even Year Runs")
print(summary(model <- lm(RECRUIT ~ STOCK, data = plot_data.df)))

plot_data.df <- subset(pinksO.df, STREAM != "Indian River")

print(ggplot(plot_data.df, aes(STOCK, RECRUIT)) +
        ggtitle("All N Streams EXCEPT Indian River - Odd Year Runs") +
        xlab("Esc, Year t - 'Stock'") + ylab("Esc, Year t+1 - 'Recruits'") +
        geom_smooth(method="lm", se=FALSE) +
        geom_point())

print("All N Streams EXCEPT Indian River - Odd Year Runs")
print(summary(model <- lm(RECRUIT ~ STOCK, data = plot_data.df)))
