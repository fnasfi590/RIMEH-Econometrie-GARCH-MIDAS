P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

library(readr)
library(dplyr)

FD <- file.path(P, "data", "processed", "daily_model_data.csv")
FM <- file.path(P, "data", "processed", "monthly_model_data_INFLATION_FIX.csv")

d <- read_csv(FD, show_col_types = FALSE)
m <- read_csv(FM, show_col_types = FALSE)

cat("\n============================================================\n")
cat(" RIMEH - AUDIT SOURCE RV SAU - CORRECTION\n")
cat("============================================================\n")

# ============================================================
# SAU DAILY
# ============================================================

country_daily <- d[[1]]
date_daily <- as.Date(d[[2]])

sau <- d[country_daily == "SAU", , drop = FALSE]

sau$date_tmp <- as.Date(sau[[2]])
sau$year_month <- format(sau$date_tmp, "%Y-%m")

cat("\nSAU observations :", nrow(sau), "\n")

# ============================================================
# COLONNES NUMERIQUES
# ============================================================

cat("\n============================================================\n")
cat(" COLONNES NUMERIQUES SAU\n")
cat("============================================================\n")

for (j in seq_along(sau)) {

  x <- suppressWarnings(as.numeric(sau[[j]]))

  if (sum(is.finite(x)) > 0) {
    cat(
      names(sau)[j],
      " | N=", sum(is.finite(x)),
      " | Min=", min(x, na.rm=TRUE),
      " | Median=", median(x, na.rm=TRUE),
      " | Mean=", mean(x, na.rm=TRUE),
      " | Max=", max(x, na.rm=TRUE),
      "\n"
    )
  }
}

# ============================================================
# MOIS PROBLEMATIQUES
# ============================================================

problem_months <- c(
  "2006-05",
  "2008-10",
  "2004-05",
  "2006-04",
  "2006-03",
  "2015-08",
  "2008-01",
  "2008-11",
  "2009-01",
  "2009-03"
)

cat("\n============================================================\n")
cat(" SAU - MOIS PROBLEMATIQUES\n")
cat("============================================================\n")

for (ym in problem_months) {

  cat("\n------------------------------------------------------------\n")
  cat("SAU :", ym, "\n")
  cat("------------------------------------------------------------\n")

  z <- sau[sau$year_month == ym, , drop = FALSE]

  cat("N observations :", nrow(z), "\n")

  keep <- intersect(
    c("country", "date", "stock_price", "return", "n_obs"),
    names(z)
  )

  print(as.data.frame(z[, keep, drop = FALSE]), row.names = FALSE)

  r <- suppressWarnings(as.numeric(z[["return"]]))

  cat(
    "\nSomme return^2 =",
    sum(r[is.finite(r)]^2),
    "\n"
  )

  cat(
    "N return valides =",
    sum(is.finite(r)),
    "\n"
  )
}

# ============================================================
# MONTHLY SAU
# ============================================================

cat("\n============================================================\n")
cat(" MONTHLY SAU - RV.x / RV.y\n")
cat("============================================================\n")

country_monthly <- m[[2]]

msau <- m[country_monthly == "SAU", , drop = FALSE]

rvx <- suppressWarnings(as.numeric(msau[[7]]))
rvy <- suppressWarnings(as.numeric(msau[[10]]))

cat("RV.x N :", sum(is.finite(rvx)), "\n")
cat("RV.y N :", sum(is.finite(rvy)), "\n")

cat("RV.x mean :", mean(rvx, na.rm=TRUE), "\n")
cat("RV.y mean :", mean(rvy, na.rm=TRUE), "\n")

cat("RV.x median :", median(rvx, na.rm=TRUE), "\n")
cat("RV.y median :", median(rvy, na.rm=TRUE), "\n")

cat(
  "Correlation RV.x/RV.y :",
  cor(rvx, rvy, use="complete.obs"),
  "\n"
)

cat("\n20 premières lignes monthly SAU :\n")

keepm <- intersect(
  c("date", "country", "RV.x", "RV.y", "n_daily", "n_obs"),
  names(msau)
)

print(
  as.data.frame(head(msau[, keepm, drop=FALSE], 20)),
  row.names=FALSE
)

# ============================================================
# VERIFICATION RETURN = 100 * DIFF(LOG(PRIX))
# ============================================================

cat("\n============================================================\n")
cat(" VERIFICATION RETURN SAU\n")
cat("============================================================\n")

price <- suppressWarnings(as.numeric(sau[["stock_price"]]))
ret <- suppressWarnings(as.numeric(sau[["return"]]))

calc_ret <- c(
  NA,
  diff(log(price)) * 100
)

ok <- is.finite(ret) & is.finite(calc_ret)

cat("Returns comparables :", sum(ok), "\n")

if (sum(ok) > 0) {

  cat(
    "Correlation return / diff(log(price))*100 :",
    cor(ret[ok], calc_ret[ok]),
    "\n"
  )

  cat(
    "Difference moyenne absolue :",
    mean(abs(ret[ok] - calc_ret[ok])),
    "\n"
  )

  cat(
    "Difference max absolue :",
    max(abs(ret[ok] - calc_ret[ok])),
    "\n"
  )

  cat("\nQuelques différences :\n")

  check <- data.frame(
    date = sau$date_tmp[ok],
    stock_price = price[ok],
    return_original = ret[ok],
    return_calcule = calc_ret[ok],
    difference = ret[ok] - calc_ret[ok]
  )

  print(
    head(check[order(-abs(check$difference)), ], 20),
    row.names=FALSE
  )
}

# ============================================================
# TEST RV.x PAR RAPPORT AUX RENDEMENTS SAU
# ============================================================

cat("\n============================================================\n")
cat(" TEST DIRECT RV.x = SOMME(return^2)\n")
cat("============================================================\n")

sau$return_num <- ret

rv_direct <- sau %>%
  group_by(year_month) %>%
  summarise(
    N = sum(is.finite(return_num)),
    RV_direct = sum(return_num[is.finite(return_num)]^2),
    .groups="drop"
  )

rv_direct$date <- as.Date(
  paste0(rv_direct$year_month, "-01")
)

monthly_sau <- data.frame(
  date = as.Date(msau[[1]]),
  RV_x = rvx
)

test_rv <- monthly_sau %>%
  left_join(rv_direct, by="date") %>%
  mutate(
    difference = RV_x - RV_direct,
    ratio = RV_x / RV_direct
  )

cat("Mois comparables :", sum(is.finite(test_rv$RV_direct)), "\n")

cat(
  "Correlation :",
  cor(
    test_rv$RV_x,
    test_rv$RV_direct,
    use="complete.obs"
  ),
  "\n"
)

cat(
  "Ratio median :",
  median(test_rv$ratio, na.rm=TRUE),
  "\n"
)

cat(
  "Ratio mean :",
  mean(test_rv$ratio, na.rm=TRUE),
  "\n"
)

cat("\n20 différences les plus importantes :\n")

top <- test_rv[
  order(-abs(test_rv$difference)),
  ,
  drop=FALSE
]

print(
  head(top, 20),
  row.names=FALSE
)

# ============================================================
# EXPORT
# ============================================================

OUT <- file.path(P, "results", "preliminary_tests")
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)

write.csv(
  test_rv,
  file.path(OUT, "TABLEAU_25_AUDIT_RV_SAU_DIRECT.csv"),
  row.names=FALSE
)

cat("\n============================================================\n")
cat(" AUDIT SOURCE SAU TERMINE\n")
cat("============================================================\n")
