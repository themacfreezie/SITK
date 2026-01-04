library(here) # set workind directory
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/05E-NSEout_modelbuild.R")
options(max.print=2000)

# load data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinksE_scst.Rda"))
# observer data - from NSEout_observer
load(here("data", "clean", "NSEout_wobserverE.Rda"))
# pdo data - from pdo_clean
load(here("data", "clean", "WpdoE.Rda"))

# grab year lists, # of observations, & pure count data
yearsE <- names(wpinksE_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
nE <- nrow(wpinksE_scst.df)

# convert counts to matrix
datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])

# grab IR data for output
IRdata <- as.numeric(as.vector(datE[6,]))
saveRDS(IRdata, file=here("data", "clean", "IRdataE.rds"))

# drop observer streams missing from peak count data
## why are these missing?
wobserverE.df <- wobserverE.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

# replace missing values 
wobserverE.df$ID_1960  <- wobserverE.df$ID_1960 %>% replace_na(11)
wobserverE.df$ID_1962  <- wobserverE.df$ID_1962 %>% replace_na(11)
wobserverE.df$ID_1964  <- wobserverE.df$ID_1964 %>% replace_na(11)
wobserverE.df$ID_1966  <- wobserverE.df$ID_1966 %>% replace_na(11)
wobserverE.df$ID_1968  <- wobserverE.df$ID_1968 %>% replace_na(11)
wobserverE.df$ID_1970  <- wobserverE.df$ID_1970 %>% replace_na(11)
wobserverE.df$ID_1972  <- wobserverE.df$ID_1972 %>% replace_na(11)
wobserverE.df$ID_1974  <- wobserverE.df$ID_1974 %>% replace_na(11)
wobserverE.df$ID_1976  <- wobserverE.df$ID_1976 %>% replace_na(11)
wobserverE.df$ID_1978  <- wobserverE.df$ID_1978 %>% replace_na(11)
wobserverE.df$ID_1980  <- wobserverE.df$ID_1980 %>% replace_na(11)
wobserverE.df$ID_1982  <- wobserverE.df$ID_1982 %>% replace_na(11)
wobserverE.df$ID_1984  <- wobserverE.df$ID_1984 %>% replace_na(11)
wobserverE.df$ID_1986  <- wobserverE.df$ID_1986 %>% replace_na(11)
wobserverE.df$ID_1988  <- wobserverE.df$ID_1988 %>% replace_na(11)
wobserverE.df$ID_1990  <- wobserverE.df$ID_1990 %>% replace_na(11)
wobserverE.df$ID_1992  <- wobserverE.df$ID_1992 %>% replace_na(11)
wobserverE.df$ID_1994  <- wobserverE.df$ID_1994 %>% replace_na(11)
wobserverE.df$ID_1996  <- wobserverE.df$ID_1996 %>% replace_na(11)
wobserverE.df$ID_1998  <- wobserverE.df$ID_1998 %>% replace_na(11)
wobserverE.df$ID_2000  <- wobserverE.df$ID_2000 %>% replace_na(11)
wobserverE.df$ID_2002  <- wobserverE.df$ID_2002 %>% replace_na(11)
wobserverE.df$ID_2004  <- wobserverE.df$ID_2004 %>% replace_na(11)
wobserverE.df$ID_2006  <- wobserverE.df$ID_2006 %>% replace_na(11)
wobserverE.df$ID_2008  <- wobserverE.df$ID_2008 %>% replace_na(11)
wobserverE.df$ID_2010  <- wobserverE.df$ID_2010 %>% replace_na(11)
wobserverE.df$ID_2012  <- wobserverE.df$ID_2012 %>% replace_na(11)
wobserverE.df$ID_2014  <- wobserverE.df$ID_2014 %>% replace_na(11)
wobserverE.df$ID_2016  <- wobserverE.df$ID_2016 %>% replace_na(11)
wobserverE.df$ID_2018  <- wobserverE.df$ID_2018 %>% replace_na(11)
wobserverE.df$ID_2020  <- wobserverE.df$ID_2020 %>% replace_na(11)
wobserverE.df$ID_2022  <- wobserverE.df$ID_2022 %>% replace_na(11)

# convert observer ID and pdo data to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
pdoE <- data.matrix(WpdoE)

# specify matrices for MARSS models
bE_i.model <- "identity"
bE.model <- "unconstrained"
uEbb.model <- matrix(paste0("u", seq(2)))
qE.model <- "diagonal and equal"
zEb.model <- matrix(
  c(1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    0, 1,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0),
  nrow = 36,
  ncol = 2,
  byrow = TRUE
)
aE.model <- "zero"
rE.model <- "equalvarcov" 
x0E.model <- "unequal"
v0E.model <- "zero"
cEb.model <- matrix("pdo", 2, 1)
dE.model <- matrix(list(0), nE, nE)

model.listEbeta_i <- list(
  B = bE_i.model, U = uEbb.model, Q = qE.model,
  Z = zEb.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  C= cEb.model, c = pdoE, D = dE.model, d = obsE)

model.listEbeta <- list(
  B = bE.model, U = uEbb.model, Q = qE.model,
  Z = zEb.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  C= cEb.model, c = pdoE, D = dE.model, d = obsE)

# specify MARSS model
ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEbeta_i.rds"))){
  ssEbeta_i <- MARSS(datE, model = model.listEbeta_i, method = "kem")
  saveRDS(ssEbeta_i, file=here("data", "clean", "ssEbeta_i.rds"))
}
proc.time()[3] - ptm

ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssEbeta.rds"))){
  ssEbeta <- MARSS(datE, model = model.listEbeta, method = "kem")
  saveRDS(ssEbeta, file=here("data", "clean", "ssEbeta.rds"))
}
proc.time()[3] - ptm

ssE <- ssEbeta

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