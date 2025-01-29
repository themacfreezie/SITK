library(here) # set workind directory
library(tidyverse)

# set loc
here::i_am("code/primary/07b-IR_statetransform.R")
options(max.print=2000)

# load data - from adfg_pinkWIDE
load(here("data", "clean", "STKste_statesLongE.Rda"))
load(here("data", "clean", "STKste_statesLongO.Rda"))
load(here("data", "clean", "stream_mctE.Rda"))
load(here("data", "clean", "stream_mctO.Rda"))

# reaname state variables to match
names(statesLongE)[names(statesLongE) == "state"] <- "STREAMID"
names(statesLongO)[names(statesLongO) == "state"] <- "STREAMID"

# merge
mergeE <- merge(statesLongE, stream_mctE, by="STREAMID")
mergeO <- merge(statesLongO, stream_mctO, by="STREAMID")

# apply transofrmations
mergeE$ct <- (mergeE$fitted*mergeE$sd)+mergeE$mean
mergeE$ct <- exp(mergeE$ct)
mergeE$ct <- mergeE$ct - 1

mergeO$ct <- (mergeO$fitted*mergeO$sd)+mergeO$mean
mergeO$ct <- exp(mergeO$ct)
mergeO$ct <- mergeO$ct - 1

IR_stateE <- mergeE %>% filter(STREAMID=="113-41-019")
IR_stateO <- mergeO %>% filter(STREAMID=="113-41-019")

save(IR_stateE, file=here("data", "clean", "IR_stateE.Rda"))
save(IR_stateO, file=here("data", "clean", "IR_stateO.Rda"))
