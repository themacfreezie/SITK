## SET WORKING DIR & PACKAGES

library(here)
library(MARSS)

# load in ss models
ssEbeta_AMPr <- readRDS(file=here("data", "clean", "ssEbeta_AMPr.rds"))
ssEbeta_DFGr <- readRDS(file=here("data", "clean", "ssEbeta_DFGr.rds"))
ssEbeta_DFGe <- readRDS(file=here("data", "clean", "ssEbeta_DFGe.rds"))
ssObeta_AMPr <- readRDS(file=here("data", "clean", "ssObeta_AMPr.rds"))
ssObeta_DFGr <- readRDS(file=here("data", "clean", "ssObeta_DFGr.rds"))
ssObeta_DFGe <- readRDS(file=here("data", "clean", "ssObeta_DFGe.rds"))

# bootstrap estimates
bootE_AMPr <- MARSSboot(ssEbeta_AMPr, nboot=100, output="parameters", sim = "parametric")
bootE_DFGr <- MARSSboot(ssEbeta_DFGr, nboot=100, output="parameters", sim = "parametric")
bootE_DFGe <- MARSSboot(ssEbeta_DFGe, nboot=100, output="parameters", sim = "parametric")
bootO_AMPr <- MARSSboot(ssObeta_AMPr, nboot=100, output="parameters", sim = "parametric")
bootO_DFGr <- MARSSboot(ssObeta_DFGr, nboot=100, output="parameters", sim = "parametric")
bootO_DFGe <- MARSSboot(ssObeta_DFGe, nboot=100, output="parameters", sim = "parametric")

# save bootstrap estimates
saveRDS(bootE_AMPr, file=here::here("data", "clean", "bootE_AMPr.rds"))
saveRDS(bootE_DFGr, file=here::here("data", "clean", "bootE_DFGr.rds"))
saveRDS(bootE_DFGe, file=here::here("data", "clean", "bootE_DFGe.rds"))
saveRDS(bootO_AMPr, file=here::here("data", "clean", "bootO_AMPr.rds"))
saveRDS(bootO_DFGr, file=here::here("data", "clean", "bootO_DFGr.rds"))
saveRDS(bootO_DFGe, file=here::here("data", "clean", "bootO_DFGe.rds"))
