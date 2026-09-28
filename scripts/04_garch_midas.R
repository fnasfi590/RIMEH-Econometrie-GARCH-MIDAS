# ============================================================
# 04_garch_midas.R
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

daily <- read_csv("data/processed/daily_clean.csv", show_col_types=FALSE)
monthly <- read_csv("data/processed/monthly_clean.csv", show_col_types=FALSE)

# The CRAN package 'rumidas' estimates GARCH-MIDAS and GARCH-MIDAS-X.
# It supports one/two MIDAS variables plus an X term.
# Therefore, for the thesis specification with many monthly regressors,
# run a sequence of parsimonious GARCH-MIDAS models and use the panel/fixed-
# effects regression as a complementary multivariate long-run equation.
#
# Example: USA, GPRT as the MIDAS variable.
#
# Convert daily returns to xts:
usa <- daily %>%
  filter(country=="USA", !is.na(return)) %>%
  arrange(date)

r_t <- xts::xts(usa$return, order.by=usa$date)

gpr_usa <- monthly %>%
  filter(country=="USA") %>%
  arrange(date) %>%
  dplyr::select(date,GPRT) %>%
  drop_na()

gpr_xts <- xts::xts(gpr_usa$GPRT, order.by=gpr_usa$date)

# Align monthly GPR with daily returns.
mv_m <- rumidas::mv_into_mat(
  r_t,
  gpr_xts,
  K=12,
  type="monthly"
)

fit_gm_gpr <- rumidas::ugmfit(
  model="GM",
  skew="NO",
  distribution="std",
  daily_ret=r_t,
  mv_m=mv_m,
  K=12,
  lag_fun="Beta",
  R=50
)

print(fit_gm_gpr)
print(summary(fit_gm_gpr))
writeLines(capture.output(summary(fit_gm_gpr)),
           "results/tables/USA_GARCH_MIDAS_GPR_summary.txt")

saveRDS(fit_gm_gpr, "results/models/USA_GARCH_MIDAS_GPR.rds")

# Example with VIX as the X variable (daily X required by rumidas GMX).
# If VIX is monthly in the thesis, do not simply pass monthly observations
# here; first construct a daily-aligned series according to the information
# timing chosen in the methodology.
message("Basic GARCH-MIDAS example completed if the input data are populated.")
