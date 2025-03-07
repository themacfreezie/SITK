library(gghighlight)
library(here) # set workind directory
# library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/07E-NSEout_output.R")
options(max.print=2000)

# load in ssE (from 05E)
ssE <- readRDS(here("data", "clean", "ssE.rds"))
ssE_i <- readRDS(here("data", "clean", "ssE_identity.rds"))

# let's see those estimates
fitted(ssE)
fitted(ssE_i)

#grabbing data for figures
statesE.est <- ssE$states
statesEse.est <- ssE$states.se
statesE.est
statesEse.est
statesE_i.est <- ssE_i$states
statesEse_i.est <- ssE_i$states.se
statesE_i.est
statesEse_i.est

# plot variance terms
statesE.df <- as.data.frame(statesE.est)
statesEse.df <- as.data.frame(statesEse.est)
statesE_i.df <- as.data.frame(statesE_i.est)
statesEse_i.df <- as.data.frame(statesEse_i.est)

statesLongE <- statesE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")
statesLongE_i <- statesE_i.df %>% 
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
statesLongE_i <- statesLongE_i %>% 
  mutate(Year = rep(yearsE, 36))

statesE <- c("NSE Outer", "Indian River")
statesE_i <- c("a", "b", "c", "d", "e", "Indian River",
               "f", "g", "h", "i", "j", "k",
               "l", "m", "n", "o", "p", "q",
               "r", "s", "t", "u", "v", "w",
               "x", "y", "z", "aa", "ab", "ac",
               "ad", "ae", "af", "ag", "ah", "ai")

statesLongE <- statesLongE %>% 
  mutate(state = c(rep(statesE[1], 32),
                   rep(statesE[2], 32)))
statesLongE_i <- statesLongE_i %>% 
  mutate(state = c(rep(statesE_i[1], 32),
                   rep(statesE_i[2], 32),
                   rep(statesE_i[3], 32),
                   rep(statesE_i[4], 32),
                   rep(statesE_i[5], 32),
                   rep(statesE_i[6], 32),
                   rep(statesE_i[7], 32),
                   rep(statesE_i[8], 32),
                   rep(statesE_i[9], 32),
                   rep(statesE_i[10], 32),
                   rep(statesE_i[11], 32),
                   rep(statesE_i[12], 32),
                   rep(statesE_i[13], 32),
                   rep(statesE_i[14], 32),
                   rep(statesE_i[15], 32),
                   rep(statesE_i[16], 32),
                   rep(statesE_i[17], 32),
                   rep(statesE_i[18], 32),
                   rep(statesE_i[19], 32),
                   rep(statesE_i[20], 32),
                   rep(statesE_i[21], 32),
                   rep(statesE_i[22], 32),
                   rep(statesE_i[23], 32),
                   rep(statesE_i[24], 32),
                   rep(statesE_i[25], 32),
                   rep(statesE_i[26], 32),
                   rep(statesE_i[27], 32),
                   rep(statesE_i[28], 32),
                   rep(statesE_i[29], 32),
                   rep(statesE_i[30], 32),
                   rep(statesE_i[31], 32),
                   rep(statesE_i[32], 32),
                   rep(statesE_i[33], 32),
                   rep(statesE_i[34], 32),
                   rep(statesE_i[35], 32),
                   rep(statesE_i[36], 32)))

statesseLongE <- statesEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")
statesseLongE_i <- statesEse_i.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLongE<- statesLongE %>% 
  mutate(se = statesseLongE$se)
statesLongE_i<- statesLongE_i %>% 
  mutate(se = statesseLongE_i$se)

statesLongE$ub = statesLongE$fitted + statesLongE$se
statesLongE$lb = statesLongE$fitted - statesLongE$se
statesLongE_i$ub = statesLongE_i$fitted + statesLongE_i$se
statesLongE_i$lb = statesLongE_i$fitted - statesLongE_i$se

# save long state estimates
save(statesLongE, file=here("data", "clean", "STKste_statesLongE.Rda"))
save(statesLongE_i, file=here("data", "clean", "STKste_statesLongE_i.Rda"))

# plotting
STKsteE <- ggplot(data = statesLongE, aes(y=fitted, x=Year, color = state)) +
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
STKsteE
STKsteE_i <- ggplot(data = statesLongE_i, aes(y=fitted, x=Year, color = state)) +
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
STKsteE_i

ggsave(here("output", "figures", "STKsteE.png"), plot=STKsteE, device="png", dpi=300)
ggsave(here("output", "figures", "STKsteE_i.png"), plot=STKsteE_i, device="png", dpi=300)

# effort to plot indian river residuals - I'm not sure if this makes sense..
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

