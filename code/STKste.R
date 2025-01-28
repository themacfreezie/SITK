library(gghighlight)
library(here) # set workind directory
library(MARSS)
library(marssTMB)
library(tidyverse)

# set loc
here::i_am("code/STKste.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "NSEout_wpinksE_scst.Rda"))
load(here("data", "NSEout_wpinksO_scst.Rda"))

# load observer data - from adfgobserver_mini
load(here("data", "NSEout_wobserverE.Rda"))
load(here("data", "NSEout_wobserverO.Rda"))

# grab year lists, # of observations, & pure count data
yearsE <- names(wpinksE_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
nE <- nrow(wpinksE_scst.df)

yearsO <- names(wpinksO_scst.df)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=13, last=16)
nO <- nrow(wpinksO_scst.df)

# convert counts to matrix
datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])
datO <- data.matrix(wpinksO_scst.df[2:ncol(wpinksO_scst.df)])

# drop observer streams missing from peak count data
## why are these missing?
wobserverE.df <- wobserverE.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]
wobserverO.df <- wobserverO.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

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

wobserverO.df$ID_1961  <- wobserverO.df$ID_1961 %>% replace_na(11)
wobserverO.df$ID_1963  <- wobserverO.df$ID_1963 %>% replace_na(11)
wobserverO.df$ID_1965  <- wobserverO.df$ID_1965 %>% replace_na(11)
wobserverO.df$ID_1967  <- wobserverO.df$ID_1967 %>% replace_na(11)
wobserverO.df$ID_1969  <- wobserverO.df$ID_1969 %>% replace_na(11)
wobserverO.df$ID_1971  <- wobserverO.df$ID_1971 %>% replace_na(11)
wobserverO.df$ID_1973  <- wobserverO.df$ID_1973 %>% replace_na(11)
wobserverO.df$ID_1975  <- wobserverO.df$ID_1975 %>% replace_na(11)
wobserverO.df$ID_1977  <- wobserverO.df$ID_1977 %>% replace_na(11)
wobserverO.df$ID_1979  <- wobserverO.df$ID_1979 %>% replace_na(11)
wobserverO.df$ID_1981  <- wobserverO.df$ID_1981 %>% replace_na(11)
wobserverO.df$ID_1983  <- wobserverO.df$ID_1983 %>% replace_na(11)
wobserverO.df$ID_1985  <- wobserverO.df$ID_1985 %>% replace_na(11)
wobserverO.df$ID_1987  <- wobserverO.df$ID_1987 %>% replace_na(11)
wobserverO.df$ID_1989  <- wobserverO.df$ID_1989 %>% replace_na(11)
wobserverO.df$ID_1991  <- wobserverO.df$ID_1991 %>% replace_na(11)
wobserverO.df$ID_1993  <- wobserverO.df$ID_1993 %>% replace_na(11)
wobserverO.df$ID_1995  <- wobserverO.df$ID_1995 %>% replace_na(11)
wobserverO.df$ID_1997  <- wobserverO.df$ID_1997 %>% replace_na(11)
wobserverO.df$ID_1999  <- wobserverO.df$ID_1999 %>% replace_na(11)
wobserverO.df$ID_2001  <- wobserverO.df$ID_2001 %>% replace_na(11)
wobserverO.df$ID_2003  <- wobserverO.df$ID_2003 %>% replace_na(11)
wobserverO.df$ID_2005  <- wobserverO.df$ID_2005 %>% replace_na(11)
wobserverO.df$ID_2007  <- wobserverO.df$ID_2007 %>% replace_na(11)
wobserverO.df$ID_2009  <- wobserverO.df$ID_2009 %>% replace_na(11)
wobserverO.df$ID_2011  <- wobserverO.df$ID_2011 %>% replace_na(11)
wobserverO.df$ID_2013  <- wobserverO.df$ID_2013 %>% replace_na(11)
wobserverO.df$ID_2015  <- wobserverO.df$ID_2015 %>% replace_na(11)
wobserverO.df$ID_2017  <- wobserverO.df$ID_2017 %>% replace_na(11)
wobserverO.df$ID_2019  <- wobserverO.df$ID_2019 %>% replace_na(11)
wobserverO.df$ID_2021  <- wobserverO.df$ID_2021 %>% replace_na(11)
wobserverO.df$ID_2023  <- wobserverO.df$ID_2023 %>% replace_na(11)
  # stupid brute force solution, there's got to be a better way to do this

# convert observer ID to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
obsO <- data.matrix(wobserverO.df[2:ncol(wobserverO.df)])

# specify matrices for MARSS models
bE.model <- "identity"
# uE.model <- matrix(
#   c("u1", "u2"),
#   nrow = 2,
#   ncol = 1,
#   byrow = TRUE
# )
uE.model <- "zero"
qE.model <- "diagonal and equal"
zE.model <- "identity"
aE.model <- "zero"
# rE.model <- "diagonal and equal"
rE.model <- "equalvarcov" 
  # will compare with AIC
dE.model <- matrix(list(0), nE, nE)
diag(dE.model) <- paste0("d", seq(nE))
x0E.model <- "unequal"
v0E.model <- "zero"

bO.model <- "identity"
# uO.model <- matrix(
#   c("u1", "u2"),
#   nrow = 2,
#   ncol = 1,
#   byrow = TRUE
# )
uO.model <- "zero"
qO.model <- "diagonal and equal"
zO.model <- "identity"
aO.model <- "zero"
# rO.model <- "diagonal and equal"
rO.model <- "equalvarcov" 
  # will compare with AIC
dO.model <- matrix(list(0), nO, nO)
diag(dO.model) <- paste0("d", seq(nO))
x0O.model <- "unequal"
v0O.model <- "zero"

model.listE <- list(
  B = bE.model, U = uE.model, Q = qE.model,
  Z = zE.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0,
  D = dE.model, d = obsE)

model.listO <- list(
  B = bO.model, U = uO.model, Q = qO.model,
  Z = zO.model, A = aO.model, R = rO.model,
  x0 = x0O.model, V0 = v0O.model, tinitx = 0,
  D = dO.model, d = obsO)

# specify MARSS model
ptm <- proc.time()
ssE <- MARSS(datE, model = model.listE, method = "kem")
proc.time()[3] - ptm

ptm <- proc.time()
ssO <- MARSS(datO, model = model.listO, method = "kem")
proc.time()[3] - ptm

# let's see those estimates
fitted(ssE)
fitted(ssO)

#grabbing data for figures
statesE.est <- ssE$states
statesEse.est <- ssE$states.se
statesE.est
statesEse.est

statesO.est <- ssO$states
statesOse.est <- ssO$states.se
statesO.est
statesOse.est

# plot variance terms
statesE.df <- as.data.frame(statesE.est)
statesEse.df <- as.data.frame(statesEse.est)

statesO.df <- as.data.frame(statesO.est)
statesOse.df <- as.data.frame(statesOse.est)

statesLongE <- statesE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")

statesLongO <- statesO.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")

yearsE <- c(1960, 1962, 1964, 1966, 1968,
            1970, 1972, 1974, 1976, 1978,
            1980, 1982, 1984, 1986, 1988,
            1990, 1992, 1994, 1996, 1998,
            2000, 2002, 2004, 2006, 2008,
            2010, 2012, 2014, 2016, 2018,
            2020, 2022)

yearsO <- c(1961, 1963, 1965, 1967, 1969,
            1971, 1973, 1975, 1977, 1979,
            1981, 1983, 1985, 1987, 1989,
            1991, 1993, 1995, 1997, 1999,
            2001, 2003, 2005, 2007, 2009,
            2011, 2013, 2015, 2017, 2019,
            2021, 2023)

statesLongE <- statesLongE %>% 
  mutate(Year = rep(yearsE, 36))

statesLongO <- statesLongO %>% 
  mutate(Year = rep(yearsO, 36))

statesE <- as.vector(wpinksE_scst.df$STREAMID)
statesO <- as.vector(wpinksO_scst.df$STREAMID)

statesLongE <- statesLongE %>% 
  mutate(state = c(rep(statesE[1], 32),
                   rep(statesE[2], 32),
                   rep(statesE[3], 32),
                   rep(statesE[4], 32),
                   rep(statesE[5], 32),
                   rep(statesE[6], 32),
                   rep(statesE[7], 32),
                   rep(statesE[8], 32),
                   rep(statesE[9], 32),
                   rep(statesE[10], 32),
                   rep(statesE[11], 32),
                   rep(statesE[12], 32),
                   rep(statesE[13], 32),
                   rep(statesE[14], 32),
                   rep(statesE[15], 32),
                   rep(statesE[16], 32),
                   rep(statesE[17], 32),
                   rep(statesE[18], 32),
                   rep(statesE[19], 32),
                   rep(statesE[20], 32),
                   rep(statesE[21], 32),
                   rep(statesE[22], 32),
                   rep(statesE[23], 32),
                   rep(statesE[24], 32),
                   rep(statesE[25], 32),
                   rep(statesE[26], 32),
                   rep(statesE[27], 32),
                   rep(statesE[28], 32),
                   rep(statesE[29], 32),
                   rep(statesE[30], 32),
                   rep(statesE[31], 32),
                   rep(statesE[32], 32),
                   rep(statesE[33], 32),
                   rep(statesE[34], 32),
                   rep(statesE[35], 32),
                   rep(statesE[36], 32)))

statesLongO <- statesLongO %>% 
  mutate(state = c(rep(statesO[1], 32),
                   rep(statesO[2], 32),
                   rep(statesO[3], 32),
                   rep(statesO[4], 32),
                   rep(statesO[5], 32),
                   rep(statesO[6], 32),
                   rep(statesO[7], 32),
                   rep(statesO[8], 32),
                   rep(statesO[9], 32),
                   rep(statesO[10], 32),
                   rep(statesO[11], 32),
                   rep(statesO[12], 32),
                   rep(statesO[13], 32),
                   rep(statesO[14], 32),
                   rep(statesO[15], 32),
                   rep(statesO[16], 32),
                   rep(statesO[17], 32),
                   rep(statesO[18], 32),
                   rep(statesO[19], 32),
                   rep(statesO[20], 32),
                   rep(statesO[21], 32),
                   rep(statesO[22], 32),
                   rep(statesO[23], 32),
                   rep(statesO[24], 32),
                   rep(statesO[25], 32),
                   rep(statesO[26], 32),
                   rep(statesO[27], 32),
                   rep(statesO[28], 32),
                   rep(statesO[29], 32),
                   rep(statesO[30], 32),
                   rep(statesO[31], 32),
                   rep(statesO[32], 32),
                   rep(statesO[33], 32),
                   rep(statesO[34], 32),
                   rep(statesO[35], 32),
                   rep(statesO[36], 32)))

statesseLongE <- statesEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesseLongO <- statesOse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLongE<- statesLongE %>% 
  mutate(se = statesseLongE$se)

statesLongO<- statesLongO %>% 
  mutate(se = statesseLongO$se)

statesLongE$ub = statesLongE$fitted + statesLongE$se
statesLongE$lb = statesLongE$fitted - statesLongE$se

statesLongO$ub = statesLongO$fitted + statesLongO$se
statesLongO$lb = statesLongO$fitted - statesLongO$se

# save long state estimates
save(statesLongE, file=here("data", "STKste_statesLongE.Rda"))
save(statesLongO, file=here("data", "STKste_statesLongO.Rda"))

statesLongE <- statesLongE %>% 
  mutate(state = c(rep(statesE[1], 32),
                   rep(statesE[2], 32),
                   rep(statesE[3], 32),
                   rep(statesE[4], 32),
                   rep(statesE[5], 32),
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River",
                   rep(statesE[7], 32),
                   rep(statesE[8], 32),
                   rep(statesE[9], 32),
                   rep(statesE[10], 32),
                   rep(statesE[11], 32),
                   rep(statesE[12], 32),
                   rep(statesE[13], 32),
                   rep(statesE[14], 32),
                   rep(statesE[15], 32),
                   rep(statesE[16], 32),
                   rep(statesE[17], 32),
                   rep(statesE[18], 32),
                   rep(statesE[19], 32),
                   rep(statesE[20], 32),
                   rep(statesE[21], 32),
                   rep(statesE[22], 32),
                   rep(statesE[23], 32),
                   rep(statesE[24], 32),
                   rep(statesE[25], 32),
                   rep(statesE[26], 32),
                   rep(statesE[27], 32),
                   rep(statesE[28], 32),
                   rep(statesE[29], 32),
                   rep(statesE[30], 32),
                   rep(statesE[31], 32),
                   rep(statesE[32], 32),
                   rep(statesE[33], 32),
                   rep(statesE[34], 32),
                   rep(statesE[35], 32),
                   rep(statesE[36], 32)))

statesLongO <- statesLongO %>% 
  mutate(state = c(rep(statesO[1], 32),
                   rep(statesO[2], 32),
                   rep(statesO[3], 32),
                   rep(statesO[4], 32),
                   rep(statesO[5], 32),
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River","Indian River","Indian River","Indian River",
                   "Indian River","Indian River",
                   rep(statesO[7], 32),
                   rep(statesO[8], 32),
                   rep(statesO[9], 32),
                   rep(statesO[10], 32),
                   rep(statesO[11], 32),
                   rep(statesO[12], 32),
                   rep(statesO[13], 32),
                   rep(statesO[14], 32),
                   rep(statesO[15], 32),
                   rep(statesO[16], 32),
                   rep(statesO[17], 32),
                   rep(statesO[18], 32),
                   rep(statesO[19], 32),
                   rep(statesO[20], 32),
                   rep(statesO[21], 32),
                   rep(statesO[22], 32),
                   rep(statesO[23], 32),
                   rep(statesO[24], 32),
                   rep(statesO[25], 32),
                   rep(statesO[26], 32),
                   rep(statesO[27], 32),
                   rep(statesO[28], 32),
                   rep(statesO[29], 32),
                   rep(statesO[30], 32),
                   rep(statesO[31], 32),
                   rep(statesO[32], 32),
                   rep(statesO[33], 32),
                   rep(statesO[34], 32),
                   rep(statesO[35], 32),
                   rep(statesO[36], 32)))

# plotting
STKsteE <- ggplot(data = statesLongE, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estiamtes of even year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteE

STKsteO <- ggplot(data = statesLongO, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estiamtes of odd year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteO

ggsave(here("output", "figures", "STKsteE.png"), plot=STKsteE, device="png", dpi=300)
ggsave(here("output", "figures", "STKsteO.png"), plot=STKsteO, device="png", dpi=300)
