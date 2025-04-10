## SET WORKING DIR & PACKAGES

# import packages
library(here)
library(tidyverse)

# create working dir and output folder
here::i_am("code/development/AKscience_plot.R")
options(max.print=10000)

# pull in data
load(here("data", "clean", "NSEout_tpinksE_scst.Rda"))
load(here("data", "clean", "NSEout_tpinksO_scst.Rda"))

# rename count var for ease
pinksE_scst.df$count <- pinksE_scst.df$standard_ct

# plotting
NSEout_popE <- ggplot(data = pinksE_scst.df, aes(y=count, x=YEAR, fill=STREAMID)) +
  geom_line(color = "grey40", show.legend = FALSE) +
  theme_classic() +  
  labs(x = "", 
       y="Relative change in average population (ln)",
       title='Estimates of even year pink salmon peak returns in 36 Southeast Alaska streams') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
NSEout_popE

# save 
ggsave(here("output", "figures", "AKScience_NSEout.png"), plot=NSEout_popE, device="png", dpi=300)