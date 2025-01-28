library(here)
library(tidyverse)

here::i_am("code/strayrates_timing.R")

# read in data
stray_weeks <- read_xlsx(here("data", "ADFG_strayrates.xlsx"), sheet= "timing", col_names = TRUE)

strayrate_timing <- ggplot(stray_weeks, aes(StatWeek, fill = Site)) + 
  geom_histogram(alpha = 0.5) +
  labs(x = "",
       y='Number of Surveys Conducted',
       title='Week of occurance of stray rate surveys',
       subtitle='2011 - 2024') + 
  scale_x_continuous(expand = c(0, 0.1), 
                     breaks=c(31, 34, 37, 40),
                     labels=c("July", "August", "September", "October")) +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
strayrate_timing

ggsave(here("output", "figures", "stray_timing.png"), plot=strayrate_timing, device="png", dpi=300)