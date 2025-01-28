library(ggplot2)
library(here)

here::i_am("code/strayrates_compare.R")

# load data
load(here("data", "strays_clean.Rda"))

stray_comp <- ggplot(data = strays_clean, aes(x = ir_pctMark, y = sj_pctMark, label=Year)) +
  geom_point() +
  geom_text(hjust = -0.2, vjust = 0) +
  theme_classic() +
  labs(x = "Percent of fish marked at Indian River", y = "Percent of fish marked at SJ Hatchery")
stray_comp

ggsave(here("output", "figures", "stray_comp.png"), plot=stray_comp, device="png", dpi=300, height= 6, width = 9.5)

stray_ir <- strays_clean[ , c(1:3)]
stray_sj <- strays_clean[ , c(1, 4, 5)]

names(stray_ir)[names(stray_ir) == 'ir_total'] <- 'total'
names(stray_ir)[names(stray_ir) == 'ir_pctMark'] <- 'pctMark'
stray_ir$site <- "IR"

names(stray_sj)[names(stray_sj) == 'sj_total'] <- 'total'
names(stray_sj)[names(stray_sj) == 'sj_pctMark'] <- 'pctMark'
stray_sj$site <- "SJ"

stray_long <- rbind(stray_ir, stray_sj)

stray_time <- ggplot(stray_long, aes(x = Year, y = pctMark)) +
  geom_area(aes(fill = site), position = position_dodge(), alpha = 0.3) +
  geom_point(aes(color = site))+
  labs(x = "",
       y='Percent of fish surveyed with otolith mark',
       title='Stray rates by year',
       subtitle='2011 - 2021') + 
  scale_x_continuous(expand = c(0.01, 0.0)) +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
stray_time

ggsave(here("output", "figures", "stray_time.png"), plot=stray_time, device="png", dpi=300)

# pull in IR estimated states
load(here("data", "IR_stateE.Rda"))
load(here("data", "IR_stateO.Rda"))

# merge
mergeE <- merge(IR_stateE, strays_clean, by="Year")
mergeO <- merge(IR_stateO, strays_clean, by="Year")
  # so few observations

