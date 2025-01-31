library(here)

here::i_am("code/primary/09b-temp_clean.R")

col_classes = c(rep("numeric", 14))
temp <- read.table(here("data", "raw", "NOWdata_temperature_sitkaairport.txt"), sep = "", header = TRUE, nrows = 82)

save(temp, file=here("data", "clean", "temp_sitka.Rda"))
