# ============================================================
# 05_panel_long_run.R
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

monthly <- read_csv("data/processed/monthly_clean.csv", show_col_types=FALSE)

required <- c("country","GPRT","GPRA","SUP","DEM","PRE","VIX",
              "interest_rate","exchange_rate","GDP","inflation","exporter")
missing <- setdiff(required, names(monthly))
if (length(missing)>0) stop("Missing columns: ", paste(missing,collapse=", "))

panel_data <- monthly %>%
  mutate(
    GPR_SUP=GPRT*SUP,
    SUP_EXPORTER=SUP*exporter
  ) %>%
  drop_na(all_of(required))

m <- fixest::feols(
  log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE +
    VIX + interest_rate + exchange_rate + GDP + inflation +
    GPR_SUP + SUP_EXPORTER | country,
  data=panel_data,
  cluster=~country
)

print(summary(m))

modelsummary::modelsummary(
  m,
  output="results/tables/panel_long_run.html"
)

saveRDS(m, "results/models/panel_long_run.rds")
