library(here) # set workind directory
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/modelexplore.R")
options(max.print=2000)

# load data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinks_scst.Rda"))
# observer data - from NSEout_observer
load(here("data", "clean", "NSEout_wobserver.Rda"))
# pdo data - from pdo_clean
load(here("data", "clean", "Wpdo.Rda"))

# grab year lists, # of observations, & pure count data
years <- names(wpinks_scst.df)
years <- years[-1]
years <- substring(years, first=13, last=16)
n <- nrow(wpinks_scst.df)

# convert counts to matrix
dat <- data.matrix(wpinks_scst.df[2:ncol(wpinks_scst.df)])

# grab IR data for output
IRdata <- as.numeric(as.vector(dat[6,]))
saveRDS(IRdata, file=here("data", "clean", "IRdata.rds"))

# drop observer streams missing from peak count data
## why are these missing?
wobserver.df <- wobserver.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

# replace missing values 
wobserver.df[is.na(wobserver.df)] <- 11

# convert observer ID and pdo data to matrix
obs <- data.matrix(wobserver.df[2:ncol(wobserver.df)])
pdo <- data.matrix(Wpdo)

# Data structure
for (i in 1:nrow(obsE)) {
  object_name <- paste0("POP",i,"_obs")
  assign(object_name, factor(c(obsE[i,])))
}

# Generate dummy matrices
d_mat1 <- t(model.matrix(~ POP1_obs - 1)) # Remove intercept
d_mat2 <- t(model.matrix(~ POP2_obs - 1))
