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
wpinks_scst.df <- wpinks_scst.df[-c(66)]
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
Wpdo <- Wpdo[-c(65, 66)]
pdo <- data.matrix(Wpdo)

# # Data structure
# for (i in 1:nrow(obsE)) {
#   object_name <- paste0("POP",i,"_obs")
#   assign(object_name, factor(c(obsE[i,])))
# }
# 
# # Generate dummy matrices
# d_mat1 <- t(model.matrix(~ POP1_obs - 1)) # Remove intercept
# d_mat2 <- t(model.matrix(~ POP2_obs - 1))


d.model <- matrix(list(0), 2*n, 11*n)

obs_vector <- c("obs01",
                "obs02",
                "obs03",
                "obs04",
                "obs05",
                "obs06",
                "obs07",
                "obs08",
                "obs09",
                "obs10",
                "obs11")
num_rowsD <- nrow(d.model)
num_colsD <- ncol(d.model)

d.model <- matrix(0, nrow=num_rowsD, ncol=num_colsD)



d.model[(row(d.model) == col(d.model))] <- obs_vector



indices2replace <- 
  
indices_to_replace <- c(1+(0*72), 
                        1+(1*72), 
                        1+(2*72), 
                        1+(3*72), 
                        1+(4*72), 
                        1+(5*72), 
                        1+(6*72), 
                        1+(7*72), 
                        1+(8*72), 
                        1+(9*72), 
                        1+(10*72),
                        2+(11*72),
                        2+(12*72),
                        2+(13*72),
                        2+(14*72),
                        2+(15*72),
                        2+(16*72),
                        2+(17*72),
                        2+(18*72),
                        2+(19*72),
                        2+(20*72),
                        2+(21*72),
                        3+(22*72),
                        3+(23*72),
                        3+(24*72),
                        3+(25*72),
                        3+(26*72),
                        3+(27*72),
                        3+(28*72),
                        3+(29*72),
                        3+(30*72),
                        3+(31*72),
                        3+(32*72)
                        )
d.model[indices2replace] <- obs_vector
