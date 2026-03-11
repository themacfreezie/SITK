## SET WORKING DIR & PACKAGES
library(here)
library(patchwork)
library(reshape2)
library(str2str)
library(tidyverse)

# set loc
here::i_am("code/primary/23-BETA_boot_output.R")

# pull in bootstrap data
boot_DFGob <- readRDS(file=here("data", "clean", "boot_DFGob.rds"))

# grab bootstrap parameter estimates for b
df <- boot_DFGob$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 9:16)]
df_E <- df[-c(4:6)]
df_O <- df[-c(1:3)]

# rename columns
names(df_E)[names(df_E) == "B.bE_Ht.Rt.1"] <- "River(t-1)>>Hatchery(t)"
names(df_E)[names(df_E) == "B.bE_Rt.Ht.1"] <- "Hatchery(t-1)>>River(t)" # parameter of interest
names(df_E)[names(df_E) == "B.bE_RR"] <- "River(t-1)>>River(t)"

names(df_O)[names(df_O) == "B.bO_Ht.Rt.1"] <- "River(t-1)>>Hatchery(t)"
names(df_O)[names(df_O) == "B.bO_Rt.Ht.1"] <- "Hatchery(t-1)>>River(t)" # parameter of interest
names(df_O)[names(df_O) == "B.bO_RR"] <- "River(t-1)>>River(t)"

# modular - even
df <- df_E

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsE_DFGob <- data.frame(Beta = names, mean = mean, sd = sd)
quantsE_DFGob <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplotE <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Indian River/Sheldon Jackson Hatchery interactions',
       subtitle="Even year runs",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplotE

ggsave(here("output", "figures", "E_DFGob_bplot.png"), plot=bplotE, device="png", dpi=300)

# modular - odd
df <- df_O

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
pointsO_DFGob <- data.frame(Beta = names, mean = mean, sd = sd)
quantsO_DFGob <- data.frame(sapply(df, quantile, probs=c(0.05, .25, .5, .75, 0.95)))

# plots
bplotO <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='Indian River/Sheldon Jackson Hatchery interactions',
       subtitle="Odd year runs",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplotO

ggsave(here("output", "figures", "O_DFGob_bplot.png"), plot=bplotO, device="png", dpi=300)

bplotE <- bplotE +
  theme(axis.text.x=element_blank())
bplotE

bplotO <- bplotO +
  labs(x = NULL,
       title= NULL,
       subtitle="Odd year runs",
       y=NULL) +
  scale_x_discrete(labels=c("Hatchery(t-1)>>River(t)" = "Effect of prior brood year (t-1) hatchery \n returns on current brood year (t) Indian \n River abundance", 
                            "River(t-1)>>Hatchery(t)" = "Effect of prior brood year (t-1) Indian \n River abundance on current brood \n year (t) hatchery returns",
                            "River(t-1)>>River(t)" = "Effect of prior brood year (t-1) Indian River \n abundance on current brood year (t) \n Indian River abundance"))
bplotO

bplotE/bplotO
bplot <- bplotE/bplotO
bplot

ggsave(here("output", "figures", "DFGob_bplotPANEL.png"), plot=bplot, device="png", dpi=300)

# what about all info in one figure?
# set data real long
wdf_E <- df_E |>
  pivot_longer(
    cols = c(colnames(df_E)),
    names_to = "effect",
    values_to = "value"
  )

wdf_O <- df_O |>
  pivot_longer(
    cols = c(colnames(df_O)),
    names_to = "effect",
    values_to = "value"
  )

wdf_E$run <- "Even-year"
wdf_O$run <- "Odd-year"

df <- bind_rows(wdf_E, wdf_O)

# plot
bplot <- ggplot(df, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) + 
  scale_x_discrete(labels=c("Hatchery(t-1)>>River(t)" = "Effect of prior brood year (t-1) hatchery \n returns on current brood year (t) Indian \n River abundance", 
                            "River(t-1)>>Hatchery(t)" = "Effect of prior brood year (t-1) Indian \n River abundance on current brood \n year (t) hatchery returns",
                            "River(t-1)>>River(t)" = "Effect of prior brood year (t-1) Indian River \n abundance on current brood year (t) \n Indian River abundance"))
bplot

ggsave(here("output", "figures", "DFGob_bplot.png"), plot=bplot, device="png", dpi=300) 