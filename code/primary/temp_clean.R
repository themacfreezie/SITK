library(here)

here::i_am("code/temp_clean.R")

col_classes = c(rep("numeric", 14))
temp <- read.table(here("data", "NOWdata_temperature_sitkaairport.txt"), sep = "", header = TRUE, nrows = 82)

save(temp, file=here("data", "temp_sitka.Rda"))
