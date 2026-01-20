## SET WORKING DIR & PACKAGES
library(here)
library(reshape2)
library(str2str)
library(tidyverse)

# load model objects
bootE_AMPr <- readRDS(here("data", "clean", "bootE_AMPr.rds"))
bootE_DFGr <- readRDS(here("data", "clean", "bootE_DFGr.rds"))
bootO_AMPr <- readRDS(here("data", "clean", "bootO_AMPr.rds"))
bootO_DFGr <- readRDS(here("data", "clean", "bootO_DFGr.rds"))

# modular code
mod <- bootE_AMPr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:12)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsE_AMPr <- data.frame(Beta = names, mean = mean, sd = sd)
quantsE_AMPr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_AMPr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "E_AMPr_bplot.png"), plot=bplot, device="png", dpi=300)

# # dropping outliers and recalculating points
# df$HHz <- scale(df$`Hatchery(t)<<Hatchery(t-1)`)
# df$RHz <- scale(df$`River(t)<<Hatchery(t-1)`)
# df$HRz <- scale(df$`Hatchery(t)<<River(t-1)`)
# df$RRz <- scale(df$`River(t)<<River(t-1)`)
# 
# dfHH <- df[, -c(2:4, 6:8)]
# dfRH <- df[, -c(1,3,4,5,7,8)]
# dfHR <- df[, -c(1,2,4,5,6,8)]
# dfRR <- df[, -c(1:3, 5:7)]
# 
# dfHH <- dfHH[abs(dfHH$HHz) < 3, ]
# dfRH <- dfRH[abs(dfRH$RHz) < 3, ]
# dfHR <- dfHR[abs(dfHR$HRz) < 3, ]
# dfRR <- dfRR[abs(dfRR$RRz) < 3, ]
# 
# dfZ <- cbind_fill(dfHH, dfRH, dfHR, dfRR,
#                   fill = NA)
# dfZ <- dfZ[, -c(2,4,6,8,9)]
# 
# # grab mean and sd of dfZ
# mean <- sapply(dfZ, mean, na.rm = TRUE)
# sd <- sapply(dfZ, sd, na.rm = TRUE)
# pointsE_AMPrZ <- data.frame(Beta = names, mean = mean, sd = sd)

# modular code
mod <- bootE_DFGr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:12)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsE_DFGr <- data.frame(Beta = names, mean = mean, sd = sd)
quantsE_DFGr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_DFGr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "E_DFGr_bplot.png"), plot=bplot, device="png", dpi=300)

# modular code
mod <- bootO_AMPr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:12)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsO_AMPr <- data.frame(Beta = names, mean = mean, sd = sd)
quantsO_AMPr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_AMPr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "O_AMPr_bplot.png"), plot=bplot, device="png", dpi=300)

# modular code
mod <- bootO_DFGr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:12)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
  # let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsO_DFGr <- data.frame(Beta = names, mean = mean, sd = sd)
quantsO_DFGr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_DFGr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "O_DFGr_bplot.png"), plot=bplot, device="png", dpi=300)