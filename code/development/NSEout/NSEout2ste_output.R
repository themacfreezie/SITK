library(gghighlight)
library(grid)
library(here)
library(patchwork)
library(tidyverse)

# set loc
here::i_am("code/primary/a08-NSEout_output.R")
options(max.print=2000)

# load in ssE (from 05E)
ssNSE <- readRDS(file=here("data", "clean", "ssNSE_2state.rds"))

#grabbing data for figures
statesNSE.est <- ssNSE$states
statesNSEse.est <- ssNSE$states.se
statesNSE.est
statesNSEse.est

# plot variance terms
statesNSE.df <- as.data.frame(statesNSE.est)
statesNSEse.df <- as.data.frame(statesNSEse.est)

statesLongNSE <- statesNSE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")

yearsNSEe <- c(1960, 1962, 1964, 1966, 1968,
               1970, 1972, 1974, 1976, 1978,
               1980, 1982, 1984, 1986, 1988,
               1990, 1992, 1994, 1996, 1998,
               2000, 2002, 2004, 2006, 2008,
               2010, 2012, 2014, 2016, 2018,
               2020, 2022)
yearsNSEo <- c(1961, 1963, 1965, 1967, 1969,
               1971, 1973, 1975, 1977, 1979,
               1981, 1983, 1985, 1987, 1989,
               1991, 1993, 1995, 1997, 1999,
               2001, 2003, 2005, 2007, 2009,
               2011, 2013, 2015, 2017, 2019,
               2021, 2023)

statesLongNSE <- statesLongNSE %>% 
  mutate(Year = c(rep(yearsNSEe, 1), rep(yearsNSEo, 1)))

runNSE <- c("Even", "Odd")
statesLongNSE <- statesLongNSE %>% 
  mutate(run= c(rep(runNSE[1], 32),
                rep(runNSE[2], 32)))

statesseLongNSE <- statesNSEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLongNSE <- statesLongNSE %>% 
  mutate(se = statesseLongNSE$se)

statesLongNSE$ub = statesLongNSE$fitted + statesLongNSE$se
statesLongNSE$lb = statesLongNSE$fitted - statesLongNSE$se

# save long state estimates
save(statesLongNSE, file=here("data", "clean", "STK2ste_statesLongNSE.Rda"))

# split into odd and even runs
statesLongNSE_E <- statesLongNSE[statesLongNSE$run == 'Even', ]
statesLongNSE_O <- statesLongNSE[statesLongNSE$run != 'Even', ]

# plotting
STK2steNSE_E <- ggplot(data = statesLongNSE_E, aes(y=fitted, x=Year, color = run )) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill = run), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  theme(plot.subtitle = element_text(size = 16)) +
  theme(axis.title.y = element_text(margin=margin(r = 15, unit = "pt"))) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  # gghighlight(state == "Indian River", 
  #             use_direct_label = FALSE, 
  #             unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title=NULL,
       subtitle="Even") +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  geom_hline(yintercept = 0, linetype = "dashed")
STK2steNSE_E

STK2steNSE_O <- ggplot(data = statesLongNSE_O, aes(y=fitted, x=Year, color = run)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=run), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  theme(plot.subtitle = element_text(size = 16)) +
  theme(axis.title.y = element_text(margin=margin(r = 15, unit = "pt"))) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  # gghighlight(state == "Indian River", 
  #             use_direct_label = FALSE, 
  #             unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title=NULL,
       subtitle="Odd")+
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  geom_hline(yintercept = 0, linetype = "dashed")
STK2steNSE_O

ggsave(here("output", "figures", "STK2steNSE_E.png"), plot=STK2steNSE_E, device="png", dpi=300)
ggsave(here("output", "figures", "STK2steNSE_O.png"), plot=STK2steNSE_O, device="png", dpi=300)

# stacking figs
# Shared label
STK2steNSE_O$labels$y <- STK2steNSE_E$labels$y <- " "
ylab <- wrap_elements(
  full = textGrob(
    "State Estimate (standardized)",
    rot = 90,
    gp = gpar(fontsize = 18)
  )
)

# combine
STK2steNSE <- (ylab | (STK2steNSE_E / STK2steNSE_O)) +
  plot_layout(widths = c(0.01, 1))
STK2steNSE

ggsave(here("output", "figures", "STK2steNSE.png"), plot=STK2steNSE, device="png", dpi=300)