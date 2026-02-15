## SET WORKING DIR & PACKAGES
library(here)
library(reshape2)
library(str2str)
library(tidyverse)

# set loc
here::i_am("code/primary/13-BETA_boot_output.R")

# pull in bootstrap data
boot_DFGob <- readRDS(file=here("data", "clean", "boot_DFGob.rds"))

# grab bootstrap parameter estimates for b
df <- boot_DFGob$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 9:16)]
df_E <- df[-c(4:6)]
df_O <- df[-c(1:3)]

# rename columns
names(df_E)[names(df_E) == "B.bE_Ht.Rt.1"] <- "Hatchery(t)<<River(t-1)"
names(df_E)[names(df_E) == "B.bE_Rt.Ht.1"] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df_E)[names(df_E) == "B.bE_RR"] <- "River(t)<<River(t-1)"

names(df_O)[names(df_O) == "B.bO_Ht.Rt.1"] <- "Hatchery(t)<<River(t-1)"
names(df_O)[names(df_O) == "B.bO_Rt.Ht.1"] <- "River(t)<<Hatchery(t-1)" # parameter of interest
names(df_O)[names(df_O) == "B.bO_RR"] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning these correctly

# modular - even
df <- df_E

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsE_DFGob <- data.frame(Beta = names, mean = mean, sd = sd)
quantsE_DFGob <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="Even year runs",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "E_DFGob_bplot.png"), plot=bplot, device="png", dpi=300)

# modular - odd
df <- df_O

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsO_DFGob <- data.frame(Beta = names, mean = mean, sd = sd)
quantsO_DFGob <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="Odd year runs",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

ggsave(here("output", "figures", "O_DFGob_bplot.png"), plot=bplot, device="png", dpi=300)
