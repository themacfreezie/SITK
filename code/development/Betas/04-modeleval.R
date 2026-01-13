## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/Betas/04-modeleval.R")
options(max.print=2000)

# load in ss models
ssEbeta_AMPr_r1 <- readRDS(file=here("data", "clean", "ssEbeta_AMPr_r1.rds"))
ssEbeta_DFGr_r1 <- readRDS(file=here("data", "clean", "ssEbeta_DFGr_r1.rds"))
ssEbeta_DFGe_r1 <- readRDS(file=here("data", "clean", "ssEbeta_DFGe_r1.rds"))
ssObeta_AMPr_r1 <- readRDS(file=here("data", "clean", "ssObeta_AMPr_r1.rds"))
ssObeta_DFGr_r1 <- readRDS(file=here("data", "clean", "ssObeta_DFGr_r1.rds"))
ssObeta_DFGe_r1 <- readRDS(file=here("data", "clean", "ssObeta_DFGe_r1.rds"))

ssEbeta_AMPr_r2 <- readRDS(file=here("data", "clean", "ssEbeta_AMPr_r2.rds"))
ssEbeta_DFGr_r2 <- readRDS(file=here("data", "clean", "ssEbeta_DFGr_r2.rds"))
ssEbeta_DFGe_r2 <- readRDS(file=here("data", "clean", "ssEbeta_DFGe_r2.rds"))
ssObeta_AMPr_r2 <- readRDS(file=here("data", "clean", "ssObeta_AMPr_r2.rds"))
ssObeta_DFGr_r2 <- readRDS(file=here("data", "clean", "ssObeta_DFGr_r2.rds"))
ssObeta_DFGe_r2 <- readRDS(file=here("data", "clean", "ssObeta_DFGe_r2.rds"))

# aicc comps
MARSSaic(ssEbeta_AMPr_r1, output = c("AIC")) # 150.9931  
MARSSaic(ssEbeta_AMPr_r2, output = c("AIC")) # 151.2781 

MARSSaic(ssEbeta_DFGr_r1, output = c("AIC")) # 149.4795 
MARSSaic(ssEbeta_DFGr_r2, output = c("AIC")) # 144.3393 ! 

MARSSaic(ssEbeta_DFGe_r1, output = c("AIC")) # 218.4966 ! 
MARSSaic(ssEbeta_DFGe_r2, output = c("AIC")) # 284.0343 

MARSSaic(ssObeta_AMPr_r1, output = c("AIC")) # 178.787
MARSSaic(ssObeta_AMPr_r2, output = c("AIC")) # 178.8156 

MARSSaic(ssObeta_DFGr_r1, output = c("AIC")) # 179.3377  
MARSSaic(ssObeta_DFGr_r2, output = c("AIC")) # 176.7666 !

MARSSaic(ssObeta_DFGe_r1, output = c("AIC")) # 358.9765
MARSSaic(ssObeta_DFGe_r2, output = c("AIC")) # 236.3334 !
  # seems like generally r2 is best ("equalvarcov")

# assign best fit model
ssEbeta_AMPr <- ssEbeta_AMPr_r2
ssEbeta_DFGr <- ssEbeta_DFGr_r2
ssEbeta_DFGe <- ssEbeta_DFGe_r2
ssObeta_AMPr <- ssObeta_AMPr_r2
ssObeta_DFGr <- ssObeta_DFGr_r2
ssObeta_DFGe <- ssObeta_DFGe_r2

# save best fit models
saveRDS(ssEbeta_AMPr, file=here("data", "clean", "ssEbeta_AMPr.rds"))
saveRDS(ssEbeta_DFGr, file=here("data", "clean", "ssEbeta_DFGr.rds"))
saveRDS(ssEbeta_DFGe, file=here("data", "clean", "ssEbeta_DFGe.rds"))
saveRDS(ssObeta_AMPr, file=here("data", "clean", "ssObeta_AMPr.rds"))
saveRDS(ssObeta_DFGr, file=here("data", "clean", "ssObeta_DFGr.rds"))
saveRDS(ssObeta_DFGe, file=here("data", "clean", "ssObeta_DFGe.rds"))

# # plots
# plot(ssEbeta_AMPr, plot.type = c("fitted.ytt"))
# plot(ssEbeta_DFGr, plot.type = c("fitted.ytt"))
# plot(ssEbeta_DFGe, plot.type = c("fitted.ytt"))
# plot(ssObeta_AMPr, plot.type = c("fitted.ytt"))
# plot(ssObeta_DFGr, plot.type = c("fitted.ytt"))
# plot(ssObeta_DFGe, plot.type = c("fitted.ytt"))

# data comparison
mod <- ssEbeta_AMPr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

statesE_AMPr <- df

mod <- ssEbeta_DFGr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

statesE_DFGr <- df

mod <- ssEbeta_DFGe

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

statesE_DFGe <- df

mod <- ssObeta_AMPr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

statesO_AMPr <- df

mod <- ssObeta_DFGr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

statesO_DFGr <- df

mod <- ssObeta_DFGe

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

statesO_DFGe <- df
