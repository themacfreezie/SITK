library(gghighlight)
library(here) # set workind directory
# library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/06E-NSEout_output.R")
options(max.print=2000)

# load in ssE (from 05E)
ssE <- readRDS(here("data", "clean", "ssE.rds"))

# let's see those estimates
fitted(ssE)

#grabbing data for figures
statesE.est <- ssE$states
statesEse.est <- ssE$states.se
statesE.est
statesEse.est

# plot variance terms
statesE.df <- as.data.frame(statesE.est)
statesEse.df <- as.data.frame(statesEse.est)

statesLongE <- statesE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")

yearsE <- c(1960, 1962, 1964, 1966, 1968,
            1970, 1972, 1974, 1976, 1978,
            1980, 1982, 1984, 1986, 1988,
            1990, 1992, 1994, 1996, 1998,
            2000, 2002, 2004, 2006, 2008,
            2010, 2012, 2014, 2016, 2018,
            2020, 2022)

statesLongE <- statesLongE %>% 
  mutate(Year = rep(yearsE, 2))

statesE <- c("NSE Outer", "Indian River")

statesLongE <- statesLongE %>% 
  mutate(state = c(rep(statesE[1], 32),
                   rep(statesE[2], 32)))

statesseLongE <- statesEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLongE<- statesLongE %>% 
  mutate(se = statesseLongE$se)

statesLongE$ub = statesLongE$fitted + statesLongE$se
statesLongE$lb = statesLongE$fitted - statesLongE$se

# save long state estimates
save(statesLongE, file=here("data", "clean", "STKste_statesLongE.Rda"))

# plotting
STKsteE_pdo <- ggplot(data = statesLongE, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estimates of even year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteE_pdo

ggsave(here("output", "figures", "STKsteE_pdo.png"), plot=STKsteE_pdo, device="png", dpi=300)

IRstatesLongE <- statesLongE %>% filter(state == "Indian River")
# load in IRdata
IRdata <- readRDS(here("data", "clean", "IRdataE.rds"))
IRstatesLongE$data <- IRdata
IRstatesLongE$predict <- ssE$ytT[6,]

# save long state estimates
save(IRstatesLongE, file=here("data", "clean", "IRstatesLongE.Rda"))

IR_resid <- ggplot(data = IRstatesLongE, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(y=data, x=Year, fill = state))+
  theme_classic()
IR_resid

if(autoplot_06 == "yes")autoplot(ssE)

