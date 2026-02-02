## SET WORKING DIR & PACKAGES
library(here)
library(reshape2)
library(str2str)
library(tidyverse)

# set loc
here::i_am("code/primary/13-boot_output.R")

# load model objects
bootE_AMPr <- readRDS(here("data", "clean", "bootE_AMPr.rds"))
bootE_DFGr <- readRDS(here("data", "clean", "bootE_DFGr.rds"))
bootO_AMPr <- readRDS(here("data", "clean", "bootO_AMPr.rds"))
bootO_DFGr <- readRDS(here("data", "clean", "bootO_DFGr.rds"))

bootE_DFGob <- readRDS(here("data", "clean", "bootE_DFGob.rds"))
bootO_DFGob <- readRDS(here("data", "clean", "bootO_DFGob.rds"))

# # modular code - bootE_AMPr
# mod <- bootE_AMPr
# 
# # grab bootstrap parameter estimates for b
# df <- mod$boot.params
# df <- data.frame(t(df))
# df <- df[, -c(1,2, 7:12)]
# 
# # rename columns
# names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
# names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
# names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
# names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# # let's doulbe check with mark and make sure I'm assigning hese correctly
# 
# # grab mean and sd
# names <- colnames(df)
# mean <- sapply(df, mean)
# sd <- sapply(df, sd)
# pointsE_AMPr <- data.frame(Beta = names, mean = mean, sd = sd)
# quantsE_AMPr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))
# 
# # plots
# bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
#   geom_boxplot() +
#   labs(x = NULL,
#        title='IR/SJH interactions',
#        subtitle="E_AMPr",
#        y=NULL) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_classic()
# bplot
# 
# ggsave(here("output", "figures", "E_AMPr_bplot.png"), plot=bplot, device="png", dpi=300)
#
# mod <- bootE_DFGr
# 
# # grab bootstrap parameter estimates for b
# df <- mod$boot.params
# df <- data.frame(t(df))
# df <- df[, -c(1,2, 7:12)]
# 
# # rename columns
# names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
# names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
# names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
# names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# # let's doulbe check with mark and make sure I'm assigning hese correctly
# 
# # grab mean and sd
# names <- colnames(df)
# mean <- sapply(df, mean)
# sd <- sapply(df, sd)
# pointsE_DFGr <- data.frame(Beta = names, mean = mean, sd = sd)
# quantsE_DFGr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))
# 
# # plots
# bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
#   geom_boxplot() +
#   labs(x = NULL,
#        title='IR/SJH interactions',
#        subtitle="E_DFGr",
#        y=NULL) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_classic()
# bplot
# 
# ggsave(here("output", "figures", "E_DFGr_bplot.png"), plot=bplot, device="png", dpi=300)
# 
# # modular code - bootO_AMPr
# mod <- bootO_AMPr
# 
# # grab bootstrap parameter estimates for b
# df <- mod$boot.params
# df <- data.frame(t(df))
# df <- df[, -c(1,2, 7:12)]
# 
# # rename columns
# names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
# names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
# names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
# names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# # let's doulbe check with mark and make sure I'm assigning hese correctly
# 
# # grab mean and sd
# names <- colnames(df)
# mean <- sapply(df, mean)
# sd <- sapply(df, sd)
# pointsO_AMPr <- data.frame(Beta = names, mean = mean, sd = sd)
# quantsO_AMPr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))
# 
# # plots
# bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
#   geom_boxplot() +
#   labs(x = NULL,
#        title='IR/SJH interactions',
#        subtitle="O_AMPr",
#        y=NULL) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_classic()
# bplot
# 
# ggsave(here("output", "figures", "O_AMPr_bplot.png"), plot=bplot, device="png", dpi=300)
# 
# # modular code - bootO_DFGr
# mod <- bootO_DFGr
# 
# # grab bootstrap parameter estimates for b
# df <- mod$boot.params
# df <- data.frame(t(df))
# df <- df[, -c(1,2, 7:12)]
# 
# # rename columns
# names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
# names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
# names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)" # parameter of interest
# names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
#   # let's doulbe check with mark and make sure I'm assigning hese correctly
# 
# # grab mean and sd
# names <- colnames(df)
# mean <- sapply(df, mean)
# sd <- sapply(df, sd)
# pointsO_DFGr <- data.frame(Beta = names, mean = mean, sd = sd)
# quantsO_DFGr <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))
# 
# # plots
# bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
#   geom_boxplot() +
#   labs(x = NULL,
#        title='IR/SJH interactions',
#        subtitle="O_DFGr",
#        y=NULL) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_classic()
# bplot
# 
# ggsave(here("output", "figures", "O_DFGr_bplot.png"), plot=bplot, device="png", dpi=300)

# modular code - bootE_DFGr
mod <- bootE_DFGob

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 6:12)]

# rename columns
names(df)[names(df) == "B.bHt.Rt.1"] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B.bRt.Ht.1"] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df)[names(df) == "B.bRR"] <- "River(t)<<River(t-1)"
  # let's doulbe check with mark and make sure I'm assigning these correctly

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
       subtitle="E_DFGob",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "E_DFGob_bplot.png"), plot=bplot, device="png", dpi=300)

# modular code - bootO_DFGr
mod <- bootO_DFGob

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 6:12)]

# rename columns
names(df)[names(df) == "B.bHt.Rt.1"] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B.bRt.Ht.1"] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df)[names(df) == "B.bRR"] <- "River(t)<<River(t-1)"
  # let's doulbe check with mark and make sure I'm assigning these correctly

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
       subtitle="O_DFGob",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "O_DFGob_bplot.png"), plot=bplot, device="png", dpi=300)