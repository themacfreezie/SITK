## SET WORKING DIR & PACKAGES

# import packages
library(car)
library(cowplot)
library(datasets)
library(data.table)
library(dplyr)
library(gghighlight)
library(ggplot2)
library(gplots)
library(lmtest)
library(plm)
library(readxl)
library(statespacer)
library(tidyverse)
library(tseries)

# create working dir and output folder
WD1 <- "C:/Users/thema/OneDrive/Documents/dissertation/ch1/data"
setwd(WD1)

options(max.print=10000)

## DATA FILTERING

# pull in data
pinks.df <- read_excel("adfg_pink.xlsx", col_names = TRUE)

# data transform - create study year
pinks.df <- transform(pinks.df, studyYEAR = (YEAR - 1959))

# data transform - split into odd/even runs
pinksE.df <- pinks.df %>% filter(YEAR %% 2 == 0)
pinksO.df <- pinks.df %>% filter(YEAR %% 2 != 0)

# clean up studyYEAR for new datasets
pinksE.df$studyYEAR <- ((pinksE.df$studyYEAR + 1)/2)
pinksO.df$studyYEAR <- (pinksO.df$studyYEAR/2)

# data transform - log transform peak count
pinksE.df$lnPEAK_COUNT <- log(pinksE.df$PEAK_COUNT)
pinksO.df$lnPEAK_COUNT <- log(pinksO.df$PEAK_COUNT)

# add run variable for each even/odd dataset
pinksE.df <- pinksE.df %>%
  add_column(RUN = "EVEN")
pinksO.df <- pinksO.df %>%
  add_column(RUN = "ODD")

# drop SSE - northern inner and outer
NpinksE.df <- pinksE.df %>% filter(SUB_REGION!="SSE")
NpinksO.df <- pinksO.df %>% filter(SUB_REGION!="SSE")

# drop inner - northern outer only
ouNpinksE.df <- NpinksE.df %>% filter(SUB_REGION!="NSE Inside")
ouNpinksO.df <- NpinksO.df %>% filter(SUB_REGION!="NSE Inside")
# outside is the smallest region by a lot

# create matrix for statespacer
ouNpinksE.m <- data.matrix(ouNpinksE.df, rownames.force=NA)
