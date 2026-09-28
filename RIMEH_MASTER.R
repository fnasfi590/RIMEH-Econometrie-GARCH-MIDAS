# ============================================================
# RIMEH_MASTER.R
# ============================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("scripts/00_packages.R")

# 1) Download the available raw data automatically
source("scripts/00_download_data.R")
source("scripts/00_download_oil_svar.R")

# 2) Prepare the downloaded data
source("scripts/01_prepare_data.R")
source("scripts/02_garch.R")
source("scripts/03_svar_oil.R")
source("scripts/04_garch_midas.R")
source("scripts/05_panel_long_run.R")
