library(gghighlight)
library(here)
library(tidyverse)

# set loc
here::i_am("code/primary/07-NSEout_output.R")
options(max.print=2000)

# load in ssE (from 05E)
ssNSE <- readRDS(file=here("data", "clean", "ssNSE.rds"))

# some plots - Indian River (data v. fitted)
states <- ssNSE$states
ytT <- ssNSE$call$data
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

df <- df[-c(1, 3, 5:9, 11:45, 47:76)]

names(df)[names(df) == "X2"] <- "IRe.x"
names(df)[names(df) == "X4"] <- "IRo.x"
names(df)[names(df) == "X.5"] <- "IRe.y"
names(df)[names(df) == "X.41"] <- "IRo.y"

df_E <- df[-c(2,4)]
df_O <- df[-c(1,3)]

df_E$index <- 1:nrow(df_E)
df_E <- df_E %>% 
  pivot_longer(
    cols = c("IRe.x", "IRe.y"),
    names_to = "measure",
    values_to = "value"
  )

df_O$index <- 1:nrow(df_O)
df_O <- df_O %>% 
  pivot_longer(
    cols = c("IRo.x", "IRo.y"),
    names_to = "measure",
    values_to = "value"
  )

ggplot(df_E, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="Even year runs",
       y=NULL) +
  theme_classic()

ggplot(df_O, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="Odd year runs",
       y=NULL) +
  theme_classic()

## some plots - all streams
# let's see those estimates
fitted(ssNSE)

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
  mutate(Year = c(rep(yearsNSEe, 2), rep(yearsNSEo, 2)))

statesNSE <- c("NSE Outer", "Indian River")
statesLongNSE <- statesLongNSE %>% 
  mutate(state = c(rep(statesNSE[1], 32),
                   rep(statesNSE[2], 32),
                   rep(statesNSE[1], 32),
                   rep(statesNSE[2], 32)))

runNSE <- c("Even", "Odd")
statesLongNSE <- statesLongNSE %>% 
  mutate(run= c(rep(runNSE[1], 64),
                rep(runNSE[2], 64)))

statesseLongNSE <- statesNSEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLongNSE <- statesLongNSE %>% 
  mutate(se = statesseLongNSE$se)

statesLongNSE$ub = statesLongNSE$fitted + statesLongNSE$se
statesLongNSE$lb = statesLongNSE$fitted - statesLongNSE$se

# save long state estimates
save(statesLongNSE, file=here("data", "clean", "STKste_statesLongNSE.Rda"))

# split into odd and even runs
statesLongNSE_E <- statesLongNSE[statesLongNSE$run == 'Even', ]
statesLongNSE_O <- statesLongNSE[statesLongNSE$run != 'Even', ]

# plotting
STKsteNSE_E <- ggplot(data = statesLongNSE_E, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estimates of even-year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteNSE_E

STKsteNSE_O <- ggplot(data = statesLongNSE_O, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estimates of odd-year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteNSE_O

ggsave(here("output", "figures", "STKsteNSE_E.png"), plot=STKsteNSE_E, device="png", dpi=300)
ggsave(here("output", "figures", "STKsteNSE_O.png"), plot=STKsteNSE_O, device="png", dpi=300)

# effort to plot indian river residuals
IRstatesLongE <- statesLongNSE_E %>% filter(state == "Indian River")
IRstatesLongO <- statesLongNSE_O %>% filter(state == "Indian River")

# load in IRdata
IRdata_E <- readRDS(here("data", "clean", "eIRdata.rds"))
IRstatesLongE$data <- IRdata_E
IRstatesLongE$predict <- ssNSE$ytT[6,]

IRdata_O <- readRDS(here("data", "clean", "oIRdata.rds"))
IRstatesLongO$data <- IRdata_O
IRstatesLongO$predict <- ssNSE$ytT[42,]

# save long state estimates
save(IRstatesLongE, file=here("data", "clean", "IRstatesLongE.Rda"))
save(IRstatesLongO, file=here("data", "clean", "IRstatesLongO.Rda"))

IRresid_E <- ggplot(data = IRstatesLongE, aes(y=fitted, x=Year)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(y=data, x=Year)) +
  theme_classic() +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Recorded pink salmon returns compared to estimated returns at Indian River',
       subtitle='Even year runs') +
  theme(legend.position="none")
IRresid_E

IRresid_O <- ggplot(data = IRstatesLongO, aes(y=fitted, x=Year)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(y=data, x=Year)) +
  theme_classic() +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Recorded pink salmon returns compared to estimated returns at Indian River',
       subtitle='Odd year runs') +
  theme(legend.position="none")
IRresid_O

ggsave(here("output", "figures", "IRresid_E.png"), plot=IRresid_E, device="png", dpi=300)
ggsave(here("output", "figures", "IRresid_O.png"), plot=IRresid_O, device="png", dpi=300)
