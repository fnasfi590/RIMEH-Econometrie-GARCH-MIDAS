P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

library(readr)
library(dplyr)
library(lubridate)

FM <- file.path(P, "data", "processed", "monthly_model_data_INFLATION_FIX.csv")
FD <- file.path(P, "data", "processed", "daily_model_data.csv")

m <- read_csv(FM, show_col_types = FALSE)
d <- read_csv(FD, show_col_types = FALSE)

monthly <- data.frame(
  country = m[[2]],
  date = as.Date(m[[1]]),
  RV_x = as.numeric(m[[7]]),
  stringsAsFactors = FALSE
)

daily <- data.frame(
  country = d[[1]],
  date = as.Date(d[[2]]),
  return = as.numeric(d[[5]]),
  stringsAsFactors = FALSE
)

daily$year <- year(daily$date)
daily$month <- month(daily$date)

rv_calc <- daily %>%
  group_by(country, year, month) %>%
  summarise(
    N_daily = sum(is.finite(return)),
    RV_calc = sum(return[is.finite(return)]^2),
    .groups = "drop"
  ) %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", year, month))
  ) %>%
  select(country, date, N_daily, RV_calc)

cmp <- monthly %>%
  left_join(rv_calc, by = c("country", "date")) %>%
  mutate(
    difference = RV_x - RV_calc,
    abs_difference = abs(difference),
    ratio = RV_x / RV_calc
  )

# ============================================================
# SAU
# ============================================================

cat("\n============================================================\n")
cat(" SAU - ANALYSE DETAILLEE\n")
cat("============================================================\n")

sau <- cmp %>%
  filter(country == "SAU") %>%
  arrange(desc(abs_difference))

cat("N =", nrow(sau), "\n")
cat("Correlation =", cor(sau$RV_x, sau$RV_calc), "\n")
cat("Mean RV.x =", mean(sau$RV_x), "\n")
cat("Mean RV_calc =", mean(sau$RV_calc), "\n")
cat("Median ratio =", median(sau$ratio), "\n")
cat("Mean ratio =", mean(sau$ratio), "\n")

cat("\n20 plus grandes differences SAU:\n")

sau_top <- sau %>%
  select(country, date, N_daily, RV_x, RV_calc, difference, ratio) %>%
  slice_head(n = 20)

capture.output(
  print(as.data.frame(sau_top), row.names = FALSE),
  file = stdout()
)

# ============================================================
# PAK
# ============================================================

cat("\n============================================================\n")
cat(" PAK - ANALYSE DETAILLEE\n")
cat("============================================================\n")

pak <- cmp %>%
  filter(country == "PAK") %>%
  arrange(desc(abs_difference))

cat("N =", nrow(pak), "\n")
cat("Correlation =", cor(pak$RV_x, pak$RV_calc), "\n")
cat("Mean RV.x =", mean(pak$RV_x), "\n")
cat("Mean RV_calc =", mean(pak$RV_calc), "\n")
cat("Median ratio =", median(pak$ratio), "\n")
cat("Mean ratio =", mean(pak$ratio), "\n")

cat("\n20 plus grandes differences PAK:\n")

pak_top <- pak %>%
  select(country, date, N_daily, RV_x, RV_calc, difference, ratio) %>%
  slice_head(n = 20)

capture.output(
  print(as.data.frame(pak_top), row.names = FALSE),
  file = stdout()
)

# ============================================================
# GLOBAL : 30 PLUS GRANDES DIFFERENCES
# ============================================================

cat("\n============================================================\n")
cat(" 30 PLUS GRANDES DIFFERENCES GLOBALES\n")
cat("============================================================\n")

top <- cmp %>%
  arrange(desc(abs_difference)) %>%
  select(country, date, N_daily, RV_x, RV_calc, difference, ratio) %>%
  slice_head(n = 30)

capture.output(
  print(as.data.frame(top), row.names = FALSE),
  file = stdout()
)

# ============================================================
# RATIO PAR PAYS
# ============================================================

cat("\n============================================================\n")
cat(" RATIO RV.x / RV_calc PAR PAYS\n")
cat("============================================================\n")

ratio_country <- cmp %>%
  group_by(country) %>%
  summarise(
    N = n(),
    Mean_ratio = mean(ratio, na.rm = TRUE),
    Median_ratio = median(ratio, na.rm = TRUE),
    Min_ratio = min(ratio, na.rm = TRUE),
    Max_ratio = max(ratio, na.rm = TRUE),
    .groups = "drop"
  )

capture.output(
  print(as.data.frame(ratio_country), row.names = FALSE),
  file = stdout()
)

# ============================================================
# TEST DES JOURS
# ============================================================

cat("\n============================================================\n")
cat(" N_DAILY PAR PAYS\n")
cat("============================================================\n")

nd <- cmp %>%
  group_by(country) %>%
  summarise(
    Mean_N_daily = mean(N_daily),
    Median_N_daily = median(N_daily),
    Min_N_daily = min(N_daily),
    Max_N_daily = max(N_daily),
    .groups = "drop"
  )

capture.output(
  print(as.data.frame(nd), row.names = FALSE),
  file = stdout()
)

# ============================================================
# EXPORT
# ============================================================

OUT <- file.path(P, "results", "preliminary_tests")

write.csv(
  sau_top,
  file.path(OUT, "TABLEAU_21_RV_SAU_DIAGNOSTIC.csv"),
  row.names = FALSE
)

write.csv(
  pak_top,
  file.path(OUT, "TABLEAU_22_RV_PAK_DIAGNOSTIC.csv"),
  row.names = FALSE
)

write.csv(
  top,
  file.path(OUT, "TABLEAU_23_RV_TOP_DIFFERENCES.csv"),
  row.names = FALSE
)

write.csv(
  ratio_country,
  file.path(OUT, "TABLEAU_24_RV_RATIO_PAR_PAYS.csv"),
  row.names = FALSE
)

cat("\n============================================================\n")
cat(" DIAGNOSTIC RV TERMINE\n")
cat("============================================================\n")
