## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
library(dplyr)
# library(gghighlight)
library(ggplot2)
# library(gplots)
library(here)
# library(lmtest)
# library(MARSS)
# library(marssTMB)
# library(openxlsx)
# library (panelr)
# library(plm)
# library(readxl)
# library(tidyverse)
# library(TMB)
# library(tseries)

# create working dir and output folder
here::i_am("code/var_escperkm_vis.R")
options(max.print=10000)

# load data
load(here("data", "pinksE_sc.Rda"))
var.df <- read_csv(here("data", "strvarEu_NSE.csv"), col_names = TRUE)

# average escbykm by stream across years
pinksE_sc.df <- pinksE_sc.df %>% group_by(STREAMID) %>% mutate(ave = mean(ESCbyKM, na.rm = TRUE)) %>% ungroup()
pinksE_scAGG.df <- aggregate(pinksE_sc.df$ave, list(pinksE_sc.df$STREAMID, pinksE_sc.df$STREAM, pinksE_sc.df$District), mean)
pinksE_scAGG.df <- pinksE_scAGG.df %>% 
  rename(
    strID = Group.1,
    name = Group.2,
    District = Group.3,
    avgESCbyKM = x
  )

# drop southern districts
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="101")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="102")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="103")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="104")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="105")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="106")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="107")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="108")
pinksE_scAGG.df <- pinksE_scAGG.df %>% filter(District!="111")

# drop dist variable
pinksE_scAGG.df <- pinksE_scAGG.df[-c(3)]

# merge variance w/ esc
merge.df <- merge(pinksE_scAGG.df, var.df, by="strID")

# plot escapement against variance
plot(merge.df$avgESCbyKM, merge.df$sclvar, col=ifelse(merge.df$name == "Indian River", 'red', 'blue'))

ggplot(data = merge.df, aes(avgESCbyKM, sclvar)) +
  geom_point() +
  geom_point(color = "red", x = 2044.89782, y = -0.639795848)

write.csv(merge.df, here("data", "NSEstr_escvar.csv"), row.names = TRUE)
