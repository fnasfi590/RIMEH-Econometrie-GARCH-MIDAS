# ============================================================
# RIMEH - PARTIE 3C
# Complementary panel fixed-effects long-run equation.
# This is NOT a replacement for GARCH-MIDAS; it summarizes the
# multivariate long-run association across countries.
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

if (!requireNamespace("fixest", quietly=TRUE)) stop("Install package 'fixest' first.")
if (!file.exists("data/processed/monthly_model_data.csv")) stop("Run script 06 first.")

d <- readr::read_csv("data/processed/monthly_model_data.csv", show_col_types=FALSE) %>%
  mutate(date=as.Date(date)) %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-01"))

dir.create("results/models", recursive=TRUE, showWarnings=FALSE)
dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)

# log1p(RV) is used because RV is non-negative and may contain zeros.
# Model ladder mirrors the thesis hypotheses.
models <- list(
  M0 = "log1p(RV) ~ SUP + DEM + PRE | country",
  M1 = "log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE | country",
  M2 = "log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE + VIX + interest_rate + exchange_rate + GDP + inflation | country",
  M3 = "log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE + VIX + interest_rate + exchange_rate + GDP + inflation + GPRT_SUP + SUP_EXPORTER | country",
  M4 = "log1p(RV) ~ GPRT + GPRA + SUP_positive + SUP_negative + DEM + PRE + VIX + interest_rate + exchange_rate + GDP + inflation + GPRT_SUP + SUP_EXPORTER | country"
)

fits <- list(); out <- list()
for (nm in names(models)) {
  f <- tryCatch(fixest::feols(as.formula(models[[nm]]), data=d, cluster=~country), error=function(e){message(nm,": ",e$message); NULL})
  fits[[nm]] <- f
  if (!is.null(f)) out[[nm]] <- broom::tidy(f) %>% mutate(model=nm)
}
saveRDS(fits, "results/models/panel_long_run_models.rds")
if (length(out)) readr::write_csv(dplyr::bind_rows(out), "results/tables/panel_long_run_coefficients.csv")

# Export a compact model comparison table if modelsummary is available.
if (requireNamespace("modelsummary", quietly=TRUE)) {
  try(modelsummary::modelsummary(fits, output="results/tables/panel_long_run_comparison.html"), silent=TRUE)
}
writeLines(capture.output(lapply(fits, summary)), "results/tables/panel_long_run_summaries.txt")
message("Panel long-run models completed.")
