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
bE.model <- "identity"
uEity.model <- matrix(paste0("u", seq(nE)))
uE2st.model <- matrix(paste0("u", seq(2)))
qE.model <- "diagonal and equal"
zEity.model <- "identity"
zE2st.model <- matrix(
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
cEity.model <- matrix("pdo", nE, 1)
cE2st.model <- matrix("pdo", 2, 1)
aE.model <- "zero"
rE.model <- "equalvarcov" 
dE.model <- matrix(list(0), nE, nE)
diag(dE.model) <- paste0("d", seq(nE))
x0E.model <- "unequal"
v0E.model <- "zero"

model.listE_Zity <- list(
  B = bE.model, U = uEity.model, Q = qE.model,
  Z = zEity.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0
  , C= cEity.model, c = pdoE
  , D = dE.model, d = obsE
  )

model.listE_Z2st <- list(
  B = bE.model, U = uE2st.model, Q = qE.model,
  Z = zE2st.model, A = aE.model, R = rE.model,
  x0 = x0E.model, V0 = v0E.model, tinitx = 0
  , C= cE2st.model, c = pdoE
  , D = dE.model, d = obsE
  )

# specify MARSS model
ptm <- proc.time()
ssE_Zity <- MARSS(datE, model = model.listE_Zity, method = "kem")
proc.time()[3] - ptm  

ptm <- proc.time()  
ssE_Z2st <- MARSS(datE, model = model.listE_Z2st, method = "kem")
proc.time()[3] - ptm

#grabbing data for figures
statesEity.est <- ssE_Zity$states
statesEityse.est <- ssE_Zity$states.se
statesEity.est
statesEityse.est
statesE2st.est <- ssE_Z2st$states
statesE2stse.est <- ssE_Z2st$states.se
statesE2st.est
statesE2stse.est

# long data for plotting
statesE.df <- as.data.frame(statesE2st.est)
statesEse.df <- as.data.frame(statesE2stse.est)
statesE_i.df <- as.data.frame(statesEity.est)
statesEse_i.df <- as.data.frame(statesEityse.est)

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

## stray rate part
# read in data
strays <- read_csv(here("data", "raw", "strayrates_raw.csv"), col_names = TRUE)

# dismissing NSE inner and SSE
straysIR <- strays %>% filter(Source=="ESCAPEMENT SURVEY")
straysSJ <- strays %>% filter(Source!="ESCAPEMENT SURVEY")

# detail cleaning, IR
straysIR <- straysIR %>% filter(`AStream / Watershed Code`=="113-41-10190")
# dismiss weird streamID from IR
straysIR <- straysIR %>% filter(`Stat Week` < 42)
# dismiss real late samples (past mid-october)

# collapse around year, IR
straysIR <- straysIR %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# build total and pct, IR
straysIR$ir_total = straysIR$Marked + straysIR$`Not Marked`
straysIR <- straysIR %>% filter(ir_total>=30)
# dismiss sampling efforts w/ fewer than 30 observaitons
straysIR$ir_pctMark = straysIR$Marked/straysIR$ir_total
straysIR <- straysIR[-c(2:3)]

# detail cleaning, SJ
straysSJ <- straysSJ %>% filter(`Stat Week` < 42)
# dismiss real late samples (past mid-october)

# collapse around year, SJ
straysSJ <- straysSJ %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# build total and pct, SJ
straysSJ$sj_total = straysSJ$Marked + straysSJ$`Not Marked`
straysSJ <- straysSJ %>% filter(sj_total>=30)
# dismiss sampling efforts w/ fewer than 30 observaitons
straysSJ$sj_pctMark = straysSJ$Marked/straysSJ$sj_total
straysSJ <- straysSJ[-c(2:3)]

straysIRlo <- straysIR %>% filter(ir_pctMark < 0.1)
straysIRmd <- straysIR %>% filter(ir_pctMark > 0.1 & ir_pctMark < .25)
straysIRhi <- straysIR %>% filter(ir_pctMark > 0.25)

straysSJlo <- straysSJ %>% filter(sj_pctMark < 0.25)
straysSJmd <- straysSJ %>% filter(sj_pctMark > 0.25 & sj_pctMark < .7)
straysSJhi <- straysSJ %>% filter(sj_pctMark > 0.7)
