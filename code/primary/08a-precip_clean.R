library(here)

here::i_am("code/primary/08a-precip_clean.R")

col_classes = c(rep("numeric", 14))
precip <- read.table(here("data", "raw", "NOWdata_precipitation_sitkaairport.txt"), sep = "", header = TRUE, nrows = 82)

save(precip, file=here("data", "clean", "precip_sitka.Rda"))
