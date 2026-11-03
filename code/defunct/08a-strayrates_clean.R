library(here)
library(tidyverse)

here::i_am("code/defunct/08a-strayrates_clean.R")

# read in data
strays <- read_csv(here("data", "raw", "strayrates_raw.csv"), col_names = TRUE)

# dismissing NSE inner and SSE
straysIR <- strays %>% filter(Source=="ESCAPEMENT SURVEY")
straysSJ <- strays %>% filter(Source!="ESCAPEMENT SURVEY")

# detail cleaning, IR
straysIR <- straysIR %>% filter(`AStream / Watershed Code`=="113-41-10190")
  # dismiss 1 weird streamID that is not IR
straysIR <- straysIR %>% filter(`Stat Week` < 42)
  # dismiss real late samples (past mid-october)

# collapse around year, IR
straysIR <- straysIR %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# build total and pct, IR
straysIR$ir_total = straysIR$Marked + straysIR$`Not Marked`
straysIR <- straysIR %>% filter(ir_total>=30)
  # dismiss sampling efforts w/ fewer than 30 observaitons
straysIR$ir_pctMark = straysIR$Marked/straysIR$ir_total
straysIR <- straysIR[-c(2:3)]

# detail cleaning, SJ
straysSJ <- straysSJ %>% filter(`Stat Week` < 42)
  # dismiss real late samples (past mid-october)

# grab cost recovery only
straysCRw <- straysSJ %>% filter(Source=="COST RECOVERY")

# collapse around year, SJ
straysSJ <- straysSJ %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# collapse around year, cost recovery
straysCR <- straysCRw %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))
straysCRw <- straysCRw[-c(2:5, 7:14, 17:39)]

# build total and pct, SJ
straysSJ$sj_total = straysSJ$Marked + straysSJ$`Not Marked`
straysSJ <- straysSJ %>% filter(sj_total>=30)
  # dismiss sampling efforts w/ fewer than 30 observaitons
straysSJ$sj_pctMark = straysSJ$Marked/straysSJ$sj_total
straysSJ <- straysSJ[-c(2:3)]

# build total and pct, cost recovery
straysCR$CR_total = straysCR$Marked + straysCR$`Not Marked`
straysCR <- straysCR %>% filter(CR_total>=30)
  # dismiss sampling efforts w/ fewer than 30 observaitons
straysCR$CR_pctMark = straysCR$Marked/straysCR$CR_total

# build total and pct, cost recovery
straysCRw$CRw_total = straysCRw$Marked + straysCRw$`Not Marked`
straysCRw <- straysCRw %>% filter(CRw_total>=30)
  # dismiss sampling efforts w/ fewer than 30 observaitons
straysCRw$CRw_pctMark = straysCRw$Marked/straysCRw$CRw_total

strays_clean <- merge(straysIR, straysSJ, by="Year")

save(strays_clean, file=here("data", "clean", "strays_clean.Rda"))