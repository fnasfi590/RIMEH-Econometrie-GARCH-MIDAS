P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

library(readr)
library(dplyr)

FD <- file.path(P, "data", "processed", "daily_model_data.csv")
FM <- file.path(P, "data", "processed", "monthly_model_data_INFLATION_FIX.csv")

d <- read_csv(FD, show_col_types = FALSE)
m <- read_csv(FM, show_col_types = FALSE)

cat("\n============================================================\n")
cat(" RIMEH - AUDIT SOURCE RV SAU\n")
cat("============================================================\n")

cat("\nColonnes DAILY :\n")
print(names(d))

cat("\nColonnes MONTHLY :\n")
print(names(m))

# ------------------------------------------------------------
# SAU DAILY
# ------------------------------------------------------------

sau <- d %>%
  filter(.data[[1]] == "SAU")

cat("\n============================================================\n")
cat(" SAU DAILY - STRUCTURE\n")
cat("============================================================\n")

cat("N =", nrow(sau), "\n")

cat("\nPremieres observations :\n")
print(as.data.frame(head(sau, 15)), row.names = FALSE)

# ------------------------------------------------------------
# Identifier les colonnes numeriques
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# SAU mois problématiques
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" SAU - MOIS PROBLEMATIQUES\n")
cat("============================================================\n")

sau$date_tmp <- as.Date(sau[[2]])

sau$year_month <- format(sau$date_tmp, "%Y-%m")

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

for (ym in problem_months) {

  cat("\n------------------------------------------------------------\n")
  cat(" SAU :", ym, "\n")
  cat("------------------------------------------------------------\n")

  z <- sau %>%
    filter(year_month == ym)

  cat("N observations :", nrow(z), "\n")

  # afficher date + stock_price + return + n_obs
  keep <- intersect(
    c("country", "date", "stock_price", "return", "n_obs"),
    names(z)
  )

  print(as.data.frame(z[, keep, drop=FALSE]), row.names = FALSE)

  r <- suppressWarnings(as.numeric(z[["return"]]))

  cat("\nSomme return^2 =", sum(r[is.finite(r)]^2), "\n")
  cat("N return valides =", sum(is.finite(r)), "\n")
}

# ------------------------------------------------------------
# Recherche d'une RV dans les colonnes monthly
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" MONTHLY SAU - COLONNES ET RV\n")
cat("============================================================\n")

msau <- m %>%
  filter(.data[[2]] == "SAU")

for (j in seq_along(msau)) {

  x <- suppressWarnings(as.numeric(msau[[j]]))

  if (sum(is.finite(x)) > 0) {

    cat(
      names(msau)[j],
      " | N=", sum(is.finite(x)),
      " | Min=", min(x, na.rm=TRUE),
      " | Median=", median(x, na.rm=TRUE),
      " | Mean=", mean(x, na.rm=TRUE),
      " | Max=", max(x, na.rm=TRUE),
      "\n"
    )
  }
}

# ------------------------------------------------------------
# Comparaison stock_price / return
# ------------------------------------------------------------

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
}

cat("\n============================================================\n")
cat(" FIN AUDIT SOURCE RV SAU\n")
cat("============================================================\n")
