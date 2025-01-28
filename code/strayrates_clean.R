library(here)
library(tidyverse)

here::i_am("code/strayrates_clean.R")

# read in data
strays <- read_csv(here("data", "strayrates_raw.csv"), col_names = TRUE)

# dismissing NSE inner and SSE
straysIR <- strays %>% filter(Source=="ESCAPEMENT SURVEY")
straysSJ <- strays %>% filter(Source!="ESCAPEMENT SURVEY")

# detail cleaning, IR
straysIR <- straysIR %>% filter(`AStream / Watershed Code`=="113-41-10190")
  # dismiss weird streamID from IR
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

# collapse around year, SJ
straysSJ <- straysSJ %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# build total and pct, SJ
straysSJ$sj_total = straysSJ$Marked + straysSJ$`Not Marked`
straysSJ <- straysSJ %>% filter(sj_total>=30)
  # dismiss sampling efforts w/ fewer than 30 observaitons
straysSJ$sj_pctMark = straysSJ$Marked/straysSJ$sj_total
straysSJ <- straysSJ[-c(2:3)]

strays_clean <- merge(straysIR, straysSJ, by="Year")

save(strays_clean, file=here("data", "strays_clean.Rda"))
