library(here) 
# set loc
here::i_am("code/primary/15-DD_covariates.R")
options(max.print=2000)

# load data 
  # pinkies
load(here("data", "clean", "DD_pinksE_scst.Rda"))
load(here("data", "clean", "DD_pinksO_scst.Rda"))

  # pdo
load(here("data", "clean", "pdoE.Rda"))
load(here("data", "clean", "pdoO.Rda"))

  # observers
load(here("data", "clean", "NSEout_observerE.Rda"))
load(here("data", "clean", "NSEout_observerO.Rda"))

observerE.df$ID <- as.factor(observerE.df$ID)
observerO.df$ID <- as.factor(observerO.df$ID)

# quick clean on pdo varnames
names(pdoE)[names(pdoE) == "returnYear"] <- "YEAR"
names(pdoE)[names(pdoE) == "annual"] <- "lagPDO"
pdoE <- pdoE[-c(1)]

names(pdoO)[names(pdoO) == "returnYear"] <- "YEAR"
names(pdoO)[names(pdoO) == "annual"] <- "lagPDO"
pdoO <- pdoO[-c(1)]

# merge
  # pinks and pdo(lag)
mergeE.df <- merge(DD_pinksE_scst.df, pdoE, by="YEAR")
mergeO.df <- merge(DD_pinksO_scst.df, pdoO, by="YEAR")
  # pinks/pdo and observer
DD_E <- merge(mergeE.df, observerE.df, by=c("YEAR","STREAMID"))
DD_O <- merge(mergeO.df, observerO.df, by=c("YEAR","STREAMID"))
  # clean
names(DD_E)[names(DD_E) == "ID"] <- "Observer"
names(DD_O)[names(DD_O) == "ID"] <- "Observer"

# save merged data
save(DD_E, file=here("data", "clean", "DD_E.Rda"))
save(DD_O, file=here("data", "clean", "DD_O.Rda"))

# split data
DD_E1980 <- DD_E[-c(6)]
DD_O1980 <- DD_O[-c(6)]
DD_E2010 <- DD_E[-c(5)]
DD_O2010 <- DD_O[-c(5)]

names(DD_E1980)[names(DD_E1980) == "dPost1980"] <- "dPost"
names(DD_O1980)[names(DD_O1980) == "dPost1980"] <- "dPost"
names(DD_E2010)[names(DD_E2010) == "dPost2010"] <- "dPost"
names(DD_O2010)[names(DD_O2010) == "dPost2010"] <- "dPost"

# save merged and split data
save(DD_E1980, file=here("data", "clean", "DD_E1980.Rda"))
save(DD_O1980, file=here("data", "clean", "DD_O1980.Rda"))
save(DD_E2010, file=here("data", "clean", "DD_E2010.Rda"))
save(DD_O2010, file=here("data", "clean", "DD_O2010.Rda"))

