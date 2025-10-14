## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(dplyr)
library(geomtextpath)
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
here::i_am("code/defunct/IRmultiline_scaled.R")
options(max.print=10000)


## CHARTING

# pull in data
# load data
load(here("data", "clean", "pinksE_sc.Rda"))
load(here("data", "clean", "pinksO_sc.Rda"))

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

# dismissing NSE inner and SSE
pinksE_nseO.df <- pinksE_sc.df %>% filter(SUB_REGION=="NSE Outside")
pinksO_nseO.df <- pinksO_sc.df %>% filter(SUB_REGION=="NSE Outside")

ggplot(pinksE_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside - Even Year Runs") +
  xlab("Year") + ylab("Peak Count/km") +
  geom_line() +
  theme_minimal()

ggplot(pinksO_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
        ggtitle("Northern Southeast Alaska, Outside - Odd Year Runs") +
        xlab("Year") + ylab("Peak Count/km") +
        geom_line() +
        theme_minimal()

# dismissing outliers
pinksE_nseO.df <- pinksE_nseO.df %>% filter(STREAMID!="113-22-015")
# pinksE_nseO.df <- pinksE_nseO.df %>% filter(STREAMID!="113-81-003")
# pinksE_nseO.df <- pinksE_nseO.df %>% filter(STREAMID!="113-43-002")
# pinksE_nseO.df <- pinksE_nseO.df %>% filter(STREAMID!="113-73-010")

pinksO_nseO.df <- pinksO_nseO.df %>% filter(STREAMID!="113-22-015")
pinksO_nseO.df <- pinksO_nseO.df %>% filter(STREAMID!="113-81-003")
pinksO_nseO.df <- pinksO_nseO.df %>% filter(STREAMID!="113-43-002")
# pinksO_nseO.df <- pinksO_nseO.df %>% filter(STREAMID!="113-73-010")

# dropping IR for comparison
pinksE_nseO_noIR.df <- pinksE_nseO.df %>% filter(STREAMID!="113-41-019")
pinksO_nseO_noIR.df <- pinksO_nseO.df %>% filter(STREAMID!="113-41-019")

ggplot(pinksE_nseO_noIR.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside - Even Year Runs") +
  xlab("Year") + ylab("Peak count per kilometer") +
  geom_line() +
  theme_classic()

ggplot(pinksO_nseO_noIR.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside - Odd Year Runs") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_line() +
  theme_classic()

ggplot(pinksO_nseO_noIR.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_line() +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

ggplot(pinksO_nseO_noIR.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_area() +
  facet_wrap(~STREAM) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

ggplot(pinksO_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM, color = STREAM)) +
  ggtitle("Indian River among Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_line(show.legend = FALSE, linewidth = 1.4) +
  theme_classic() +
  gghighlight(STREAM == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(linewidth = 0.5, color ="gray50")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

ggplot(pinksO_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Indian River among Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_area() +
  facet_wrap(~STREAM) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

ggplot(pinksO_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Indian River among Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_line() +
  facet_wrap(~STREAM) +
  theme_classic() +
  geom_smooth(method="lm", level=0.95) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


nseO_rawE <- ggplot(pinksE_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak escapement per km") +
  geom_line(color = "grey50") +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

ggsave(here("output", "figures", "nseO_rawE.png"), plot=nseO_rawE, device="png", dpi=300)

ggplot(pinksE_nseO.df, aes(x=YEAR, y=ESCbyKM, group=STREAM, color = STREAM)) +
  ggtitle("Indian River among Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Peak Count per kilometer") +
  geom_line(show.legend = FALSE, linewidth = 1.4) +
  theme_classic() +
  gghighlight(STREAM == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(linewidth = 0.5, color ="gray50")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# log transform count data and scale by stream
pinksE_nseO.df$c <- pinksE_nseO.df$ESCbyKM + 1
pinksE_nseO.df$d <- log(pinksE_nseO.df$c)
pinksE_nseO.df <- pinksE_nseO.df %>% 
  group_by(STREAMID) %>% 
  mutate(e=scale(d))

nseOsc_noIR <- ggplot(pinksE_nseO.df, aes(x=YEAR, y=e, group=STREAM)) +
  ggtitle("Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Ln peak esc. per km (scaled)") +
  geom_line(color = "grey50") +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
nseOsc_noIR

nseOsc_IR <- ggplot(pinksE_nseO.df, aes(x=YEAR, y=e, group=STREAM, color = STREAM)) +
  ggtitle("Indian River among Northern Southeast Alaska, Outside") +
  xlab("Year") + ylab("Ln peak esc. per km (scaled)") +
  geom_line(show.legend = FALSE, linewidth = 1.4) +
  theme_classic() +
  gghighlight(STREAM == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(linewidth = 0.5, color ="gray50")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
nseOsc_IR

# svae output
ggsave(here("output", "figures", "nseOsc_noIR.png"), plot=nseOsc_noIR, device="png", dpi=300)
ggsave(here("output", "figures", "nseOsc_IR.png"), plot=nseOsc_IR, device="png", dpi=300)

