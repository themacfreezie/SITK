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
if(!file.exists(here("data", "clean", "bootE_AMPr.rds"))){
  bootE_AMPr <- MARSSboot(ssEbeta_AMPr, nboot=100, output="parameters", sim = "parametric")
  saveRDS(bootE_AMPr, file=here("data", "clean", "bootE_AMPr.rds"))
}
if(!file.exists(here("data", "clean", "bootE_DFGr.rds"))){
  bootE_DFGr <- MARSSboot(ssEbeta_DFGr, nboot=100, output="parameters", sim = "parametric")
  saveRDS(bootE_DFGr, file=here("data", "clean", "bootE_DFGr.rds"))
}
if(!file.exists(here("data", "clean", "bootE_DFGe.rds"))){
  bootE_DFGe <- MARSSboot(ssEbeta_DFGe, nboot=100, output="parameters", sim = "parametric")
  saveRDS(bootE_DFGe, file=here("data", "clean", "bootE_DFGe.rds"))
}
if(!file.exists(here("data", "clean", "bootO_AMPr.rds"))){
  bootO_AMPr <- MARSSboot(ssObeta_AMPr, nboot=100, output="parameters", sim = "parametric")
  saveRDS(bootO_AMPr, file=here("data", "clean", "bootO_AMPr.rds"))
}
if(!file.exists(here("data", "clean", "bootO_DFGr.rds"))){
  bootO_DFGr <- MARSSboot(ssObeta_DFGr, nboot=100, output="parameters", sim = "parametric")
  saveRDS(bootO_DFGr, file=here("data", "clean", "bootO_DFGr.rds"))
}
if(!file.exists(here("data", "clean", "bootO_DFGe.rds"))){
  bootO_DFGe <- MARSSboot(ssObeta_DFGe, nboot=100, output="parameters", sim = "parametric")
  saveRDS(bootO_DFGe, file=here("data", "clean", "bootO_DFGe.rds"))
}
