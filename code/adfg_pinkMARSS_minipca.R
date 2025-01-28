## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(dplyr)
library(factoextra)
library(FactoMineR)
# library(gghighlight)
# library(ggplot2)
# library(gplots)
library(here)
# library(lmtest)
library (MARSS)
library (marssTMB)
library(openxlsx)
# library (panelr)
# library(plm)
# library(readxl)
library(tidyverse)
library(TMB)
# library(tseries)

# set working dir
here::i_am("code/adfg_pinkMARSS_minipca.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "wpinksE_scst.Rda"))
load(here("data", "wpinksO_scst.Rda"))

# load observer data - from adfgobserver_mini
load(here("data", "wobserverE_mini.Rda"))
load(here("data", "wobserverO_mini.Rda"))

# # and standardized data - from adfg_pinkWIDEstandardize
# load(here("data", "wpinksEst.Rda"))
# load(here("data", "wpinksOst.Rda"))

wpinksE_scst.df <- wpinksE_scst.df[542:595, ]
wpinksO_scst.df <- wpinksO_scst.df[542:595, ]

# wpinksEst.df <- wpinksEst.df[236:296, ]
# wpinksOst.df <- wpinksOst.df[236:296, ]

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
wobserverE.df <- wobserverE.df[c(2:6, 9:12, 14:35, 38:41, 43:48, 50:62), ]
wobserverO.df <- wobserverO.df[c(2:6, 9:12, 14:35, 38:41, 43:48, 50:62), ]

# replace missing values 
wobserverE.df$ID_1960  <- wobserverE.df$ID_1960 %>% replace_na(12)
wobserverE.df$ID_2016  <- wobserverE.df$ID_2016 %>% replace_na(12)
wobserverE.df$ID_2018  <- wobserverE.df$ID_2018 %>% replace_na(12)
wobserverE.df$ID_2020  <- wobserverE.df$ID_2020 %>% replace_na(12)
wobserverE.df$ID_2022  <- wobserverE.df$ID_2022 %>% replace_na(12)

wobserverO.df$ID_1961  <- wobserverO.df$ID_1961 %>% replace_na(12)
wobserverO.df$ID_2015  <- wobserverO.df$ID_2015 %>% replace_na(12)
wobserverO.df$ID_2017  <- wobserverO.df$ID_2017 %>% replace_na(12)
wobserverO.df$ID_2019  <- wobserverO.df$ID_2019 %>% replace_na(12)
wobserverO.df$ID_2021  <- wobserverO.df$ID_2021 %>% replace_na(12)
wobserverO.df$ID_2023  <- wobserverO.df$ID_2023 %>% replace_na(12)
# stupid brute force solution, there's got to be a better way to do this

# convert observer ID to matrix
obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
obsO <- data.matrix(wobserverO.df[2:ncol(wobserverO.df)])

# specify matrices for MARSS models
bE.model <- "identity"
bO.model <- "identity"

uE.model <- matrix(c(paste0("u", seq(nE))))
uO.model <- matrix(c(paste0("u", seq(nO))))

qE.model <- "diagonal and unequal"
qO.model <- "diagonal and unequal"

zE.model <- "identity"
zO.model <- "identity"

aE.model <- "zero"
aO.model <- "zero"

rE.model <- "diagonal and equal"
rO.model <- "diagonal and equal"

dE.model <- matrix(list(0), nE, nE)
diag(dE.model) <- paste0("d", seq(nE))
dO.model <- matrix(list(0), nO, nO)
diag(dO.model) <- paste0("d", seq(nO))

x0E.model <- "unequal"
x0O.model <- "unequal"

v0E.model <- "zero"
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
ssE <- MARSS(datE, model = model.listE, method = "TMB")
proc.time()[3] - ptm

ptm <- proc.time()
ssO <- MARSS(datO, model = model.listO, method = "TMB")
proc.time()[3] - ptm

# let's see those estimates
varE.est <- ssE$par[[6]] # variance terms
varE.est

varO.est <- ssO$par[[6]] # variance terms
varO.est

# get var (even and odd) all in once place
strvar.df <- data.frame(
  strID <- wpinksE_scst.df$STREAMID,
  varE <- varE.est,
  varO <- varO.est
)
colnames(strvar.df) <- c("strID","varE", "varO")

# principal component analysis
pca_strvar.df <- strvar.df[, -1]
rownames(pca_strvar.df) <- strvar.df[, 1]

res.pca <- PCA(pca_strvar.df)
res.pca$eig

# scree plot
fviz_screeplot(res.pca, addlabels = TRUE)

# show principal component score
res.pca$ind$coord

# biplot
fviz_pca_biplot(res.pca, repel = TRUE)



# write.csv(strvarEu.df, here("data", "strvarEu.csv"), row.names = TRUE)
