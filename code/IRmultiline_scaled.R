## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(dplyr)
library(gghighlight)
# library(ggplot2)
# library(gplots)
library(here)
# library(lmtest)
# library(plm)
library(readxl)
library(tidyverse)
# library(tseries)

# create working dir and output folder
here::i_am("code/IRmultiline_scaled.R")
options(max.print=10000)


## CHARTING

# pull in data
# load data
load(here("data", "pinksE_sc.Rda"))
load(here("data", "pinksO_sc.Rda"))

# highlight Indian River on charts of escapement
print(ggplot(pinksE_sc.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("SEAK, Even Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

print(ggplot(pinksO_sc.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("SEAK - Odd Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        gghighlight(STREAM == "Indian River"))

# # Dismissing some strange outliers and recharting
# pinksEr.df <- pinksE_sc.df %>% filter(STREAMID!="113-22-015")
#   ## Whale Bay Grt Arm Hd - "heads" may be measured weird: look into it
# pinksEr.df <- pinksEr.df %>% filter(STREAMID!="113-81-003")
#   ## Goulding Harbor Hd - "heads" may be measured weird: look into it
# pinksEr.df <- pinksEr.df %>% filter(STREAMID!="113-43-002")
#   ## Nakwasina River - I believe 113-43-002 & -001 are the same system and should be combined
# pinksEr.df <- pinksEr.df %>% filter(STREAMID!="113-73-010")
#   ## Slocum Arm Head - "heads" may be measured weird: look into it
# 
# pinksOr.df <- pinksO_sc.df %>% filter(STREAMID!="113-22-015")
# pinksOr.df <- pinksOr.df %>% filter(STREAMID!="113-81-003")
# pinksOr.df <- pinksOr.df %>% filter(STREAMID!="113-43-002")
# pinksOr.df <- pinksOr.df %>% filter(STREAMID!="113-73-010")
# 
# # Recharting escapement w/ selected omissions
# print(ggplot(pinksEr.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
#         ggtitle("N. SEAK, Outer - Even Year Runs (no suspicious data)") +
#         xlab("Year") + ylab("Peak Count/km") +
#         geom_line() +
#         gghighlight(STREAM == "Indian River"))
# 
# print(ggplot(pinksOr.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
#         ggtitle("N. SEAK, Outer - Odd Year Runs (no suspicious data)") +
#         xlab("Year") + ylab("Peak Count/km") +
#         geom_line() +
#         gghighlight(STREAM == "Indian River"))
# 
# # Dismissing additional potentially outlying systems and recharting
# pinksEr2.df <- pinksEr.df %>% filter(STREAMID!="113-66-003")
# ## Marine Cove - no obvious issue here
# 
# pinksOr2.df <- pinksOr.df %>% filter(STREAMID!="113-66-003")
# 
# # Recharting escapement w/ selected omissions
# print(ggplot(pinksEr2.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
#         ggtitle("N. SEAK, Outer - Even Year Runs (no 'outliers')") +
#         xlab("Year") + ylab("Peak Count/km") +
#         geom_line() +
#         gghighlight(STREAM == "Indian River"))
# 
# print(ggplot(pinksOr2.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
#         ggtitle("N. SEAK, Outer - Odd Year Runs (no 'outliers')") +
#         xlab("Year") + ylab("Peak Count/km") +
#         geom_line() +
#         gghighlight(STREAM == "Indian River"))
