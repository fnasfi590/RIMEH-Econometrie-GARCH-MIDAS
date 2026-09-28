P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

library(readr)
library(dplyr)
library(lubridate)

cat("\n============================================================\n")
cat(" RIMEH - VALIDATION MATHEMATIQUE DE RV.x\n")
cat("============================================================\n")

FM <- file.path(P, "data", "processed", "monthly_model_data_INFLATION_FIX.csv")
FD <- file.path(P, "data", "processed", "daily_model_data.csv")

m <- read_csv(FM, show_col_types = FALSE)
d <- read_csv(FD, show_col_types = FALSE)

# ------------------------------------------------------------
# Colonnes par position
# ------------------------------------------------------------

m_country <- m[[2]]
m_date    <- as.Date(m[[1]])
rv_month  <- as.numeric(m[[7]])   # RV.x

d_country <- d[[1]]
d_date    <- as.Date(d[[2]])
ret       <- as.numeric(d[[5]])

daily <- data.frame(
  country = d_country,
  date = d_date,
  return = ret,
  stringsAsFactors = FALSE
)

daily$year  <- year(daily$date)
daily$month <- month(daily$date)

# ------------------------------------------------------------
# RV calculee a partir des rendements journaliers
#
# RV = somme(r_t^2)
# ------------------------------------------------------------

rv_calc <- daily %>%
  group_by(country, year, month) %>%
  summarise(
    N_daily = sum(is.finite(return)),
    RV_calc = sum(return[is.finite(return)]^2),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Ajouter date mensuelle
# ------------------------------------------------------------

rv_calc$date <- as.Date(
  sprintf("%04d-%02d-01", rv_calc$year, rv_calc$month)
)

# ------------------------------------------------------------
# Comparaison
# ------------------------------------------------------------

monthly <- data.frame(
  country = m_country,
  date = m_date,
  RV_x = rv_month,
  stringsAsFactors = FALSE
)

cmp <- monthly %>%
  left_join(
    rv_calc %>% select(country, date, N_daily, RV_calc),
    by = c("country", "date")
  ) %>%
  mutate(
    difference = RV_x - RV_calc,
    abs_difference = abs(difference),
    ratio = RV_x / RV_calc,
    relative_error = abs_difference / abs(RV_calc)
  )

valid <- cmp %>%
  filter(
    is.finite(RV_x),
    is.finite(RV_calc),
    RV_calc > 0
  )

cat("\n============================================================\n")
cat(" COMPARAISON GLOBALE\n")
cat("============================================================\n")

cat("Observations mensuelles :", nrow(cmp), "\n")
cat("RV.x valide :", sum(is.finite(cmp$RV_x)), "\n")
cat("RV recalculable :", sum(is.finite(cmp$RV_calc)), "\n")
cat("Comparaisons valides :", nrow(valid), "\n")

cat("\nCorrelation Pearson :", 
    cor(valid$RV_x, valid$RV_calc, method="pearson"),
    "\n")

cat("Correlation Spearman :",
    cor(valid$RV_x, valid$RV_calc, method="spearman"),
    "\n")

cat("\nDifference absolue moyenne :",
    mean(valid$abs_difference),
    "\n")

cat("Difference absolue mediane :",
    median(valid$abs_difference),
    "\n")

cat("RMSE :",
    sqrt(mean(valid$difference^2)),
    "\n")

cat("MAE relative moyenne :",
    mean(valid$relative_error) * 100,
    "%\n")

cat("\nRatio RV.x / RV_calc :\n")
print(summary(valid$ratio))

# ------------------------------------------------------------
# REGRESSION RV.x sur RV_calc
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" REGRESSION RV.x ~ RV_calc\n")
cat("============================================================\n")

fit <- lm(RV_x ~ RV_calc, data = valid)

print(summary(fit))

cat("\nCoefficient pente :", coef(fit)[2], "\n")
cat("Intercept :", coef(fit)[1], "\n")
cat("R2 :", summary(fit)$r.squared, "\n")

# ------------------------------------------------------------
# Comparaison par pays
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" COMPARAISON PAR PAYS\n")
cat("============================================================\n")

by_country <- valid %>%
  group_by(country) %>%
  summarise(
    N = n(),
    Correlation = cor(RV_x, RV_calc),
    Mean_RVx = mean(RV_x),
    Mean_RV_calc = mean(RV_calc),
    Mean_abs_diff = mean(abs_difference),
    Median_abs_diff = median(abs_difference),
    Mean_ratio = mean(ratio),
    Median_ratio = median(ratio),
    MAE_relative_pct = mean(relative_error) * 100,
    .groups = "drop"
  )

print(by_country, n = Inf)

# ------------------------------------------------------------
# Plus grandes differences
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" 20 PLUS GRANDES DIFFERENCES\n")
cat("============================================================\n")

top_diff <- valid %>%
  arrange(desc(abs_difference)) %>%
  select(
    country,
    date,
    N_daily,
    RV_x,
    RV_calc,
    difference,
    ratio,
    relative_error
  ) %>%
  slice_head(n = 20)

print(top_diff, n = 20)

# ------------------------------------------------------------
# Couverture
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" MOIS SANS RV RECALCULABLE\n")
cat("============================================================\n")

missing_calc <- cmp %>%
  filter(!is.finite(RV_calc))

cat("Nombre :", nrow(missing_calc), "\n")

if (nrow(missing_calc) > 0) {
  print(
    missing_calc %>%
      select(country, date, RV_x, RV_calc) %>%
      head(50)
  )
}

# ------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------

OUT <- file.path(P, "results", "preliminary_tests")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

write.csv(
  by_country,
  file.path(OUT, "TABLEAU_18_VALIDATION_RV_PAR_PAYS.csv"),
  row.names = FALSE
)

write.csv(
  top_diff,
  file.path(OUT, "TABLEAU_19_PLUS_GRANDES_DIFFERENCES_RV.csv"),
  row.names = FALSE
)

write.csv(
  cmp,
  file.path(OUT, "TABLEAU_20_RV_X_VS_RV_RECALCULEE.csv"),
  row.names = FALSE
)

cat("\n============================================================\n")
cat(" VALIDATION RV TERMINEE\n")
cat("============================================================\n")
