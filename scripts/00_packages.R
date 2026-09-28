# ============================================================
# 00_packages.R
# ============================================================
packages <- c(
  "tidyverse","data.table","lubridate","zoo","xts","quantmod",
  "rugarch","vars","tseries","FinTS","urca","forecast","moments",
  "broom","fixest","modelsummary","rumidas","WDI"
)

installed <- rownames(installed.packages())
for (p in packages) {
  if (!p %in% installed) install.packages(p, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))
cat("Packages loaded.\n")

# JSON parsing for EIA API
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(jsonlite)
