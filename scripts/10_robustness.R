# ============================================================
# RIMEH - PARTIE 3D
# Robustness and hypothesis tests based on the complementary panel.
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

if (!file.exists("data/processed/monthly_model_data.csv")) stop("Run script 06 first.")
d <- readr::read_csv("data/processed/monthly_model_data.csv", show_col_types=FALSE)

dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)

safe_fe <- function(f) tryCatch(fixest::feols(f, data=d, cluster=~country), error=function(e) NULL)

robust <- list(
  baseline = safe_fe(log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE + VIX | country),
  interaction = safe_fe(log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE + VIX + GPRT_SUP + SUP_EXPORTER | country),
  asymmetry = safe_fe(log1p(RV) ~ GPRT + GPRA + SUP_positive + SUP_negative + DEM + PRE + VIX | country),
  importer_only = tryCatch(fixest::feols(log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE + VIX | country, data=d[d$exporter==0, ], cluster=~country), error=function(e) NULL),
  exporter_only = tryCatch(fixest::feols(log1p(RV) ~ GPRT + GPRA + SUP + DEM + PRE + VIX | country, data=d[d$exporter==1, ], cluster=~country), error=function(e) NULL)
)

coef_out <- purrr::imap_dfr(robust, function(fit,nm){ if(is.null(fit)) return(tibble::tibble()); broom::tidy(fit) %>% mutate(model=nm) })
readr::write_csv(coef_out, "results/tables/robustness_coefficients.csv")
writeLines(capture.output(lapply(robust, summary)), "results/tables/robustness_summaries.txt")

# Wald test for exporter difference in the interaction model when estimable.
if (!is.null(robust$interaction)) {
  wt <- tryCatch(fixest::wald(robust$interaction, "SUP_EXPORTER = 0"), error=function(e) NULL)
  if (!is.null(wt)) writeLines(capture.output(wt), "results/tables/test_SUP_exporter.txt")
}

# Asymmetry equality test: SUP_positive and -SUP_negative should have equal magnitudes under symmetry.
if (!is.null(robust$asymmetry)) {
  wt2 <- tryCatch(fixest::wald(robust$asymmetry, "SUP_positive + SUP_negative = 0"), error=function(e) NULL)
  if (!is.null(wt2)) writeLines(capture.output(wt2), "results/tables/test_SUP_asymmetry.txt")
}
message("Robustness stage completed.")
