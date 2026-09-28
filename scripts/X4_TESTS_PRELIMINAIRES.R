# ============================================================
# RIMEH - X.4 TESTS ECONOMETRIQUES PRELIMINAIRES
# ADF / PP / KPSS / VIF / ARCH-LM / LJUNG-BOX
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"
setwd(base)

cat("\n============================================================\n")
cat(" RIMEH - TESTS ECONOMETRIQUES PRELIMINAIRES X.4\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

packages <- c(
  "readr",
  "dplyr",
  "tseries",
  "FinTS",
  "car",
  "openxlsx"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(readr)
library(dplyr)
library(tseries)
library(FinTS)
library(car)
library(openxlsx)

# ------------------------------------------------------------
# 2. Fichiers
# ------------------------------------------------------------

daily_file   <- "data/processed/daily_clean.csv"
monthly_file <- "data/processed/monthly_model_data.csv"

if (!file.exists(daily_file)) {
  stop("Fichier absent : ", daily_file)
}

if (!file.exists(monthly_file)) {
  stop("Fichier absent : ", monthly_file)
}

daily <- read_csv(daily_file, show_col_types = FALSE)
monthly <- read_csv(monthly_file, show_col_types = FALSE)

if ("date" %in% names(daily)) {
  daily$date <- as.Date(daily$date)
}

if ("date" %in% names(monthly)) {
  monthly$date <- as.Date(monthly$date)
}

cat("[OK] daily_clean.csv :", nrow(daily), "lignes\n")
cat("[OK] monthly_model_data.csv :", nrow(monthly), "lignes\n\n")

# ------------------------------------------------------------
# 3. Dossiers de sortie
# ------------------------------------------------------------

out <- "results/analysis/X4_PRELIMINARY_TESTS"

dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Vérification des variables
# ------------------------------------------------------------

cat("Variables daily :\n")
print(names(daily))

cat("\nVariables monthly :\n")
print(names(monthly))

# ------------------------------------------------------------
# 5. ADF / PP / KPSS - variables mensuelles
# ------------------------------------------------------------

monthly_vars <- intersect(
  c(
    "GPRT",
    "GPRA",
    "SUP",
    "DEM",
    "PRE",
    "VIX",
    "inflation",
    "interest_rate",
    "exchange_rate",
    "GDP"
  ),
  names(monthly)
)

cat("\nVariables mensuelles testees :\n")
print(monthly_vars)

countries <- sort(unique(monthly$country))

stationarity_results <- list()

safe_adf <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 25) {
    return(c(statistic = NA, p_value = NA))
  }

  z <- tryCatch(
    tseries::adf.test(x),
    error = function(e) NULL
  )

  if (is.null(z)) {
    return(c(statistic = NA, p_value = NA))
  }

  c(
    statistic = unname(z$statistic),
    p_value = z$p.value
  )
}

safe_pp <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) < 25) {
    return(c(statistic = NA, p_value = NA))
  }

  z <- tryCatch(
    tseries::pp.test(x),
    error = function(e) NULL
  )

  if (is.null(z)) {
    return(c(statistic = NA, p_value = NA))
  }

  c(
    statistic = unname(z$statistic),
    p_value = z$p.value
  )
}

safe_kpss <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) < 25) {
    return(c(statistic = NA, p_value = NA))
  }

  z <- tryCatch(
    tseries::kpss.test(x),
    error = function(e) NULL
  )

  if (is.null(z)) {
    return(c(statistic = NA, p_value = NA))
  }

  c(
    statistic = unname(z$statistic),
    p_value = z$p.value
  )
}

for (cc in countries) {

  dat <- monthly %>%
    filter(country == cc) %>%
    arrange(date)

  for (v in monthly_vars) {

    x <- suppressWarnings(as.numeric(dat[[v]]))
    ok <- is.finite(x)

    if (sum(ok) < 25) next

    a <- safe_adf(x[ok])
    p <- safe_pp(x[ok])
    k <- safe_kpss(x[ok])

    stationarity_results[[length(stationarity_results) + 1]] <-
      data.frame(
        country = cc,
        variable = v,
        N = sum(ok),

        ADF_statistic = a["statistic"],
        ADF_pvalue = a["p_value"],

        PP_statistic = p["statistic"],
        PP_pvalue = p["p_value"],

        KPSS_statistic = k["statistic"],
        KPSS_pvalue = k["p_value"],

        ADF_stationary_5pct =
          !is.na(a["p_value"]) && a["p_value"] < 0.05,

        PP_stationary_5pct =
          !is.na(p["p_value"]) && p["p_value"] < 0.05,

        KPSS_stationary_5pct =
          !is.na(k["p_value"]) && k["p_value"] >= 0.05
      )
  }
}

stationarity <- bind_rows(stationarity_results)

write_csv(
  stationarity,
  file.path(out, "X4_STATIONARITY_ADF_PP_KPSS.csv")
)

cat("\n[OK] Tests ADF / PP / KPSS :", nrow(stationarity), "tests\n")

# ------------------------------------------------------------
# 6. Tests ADF / PP / KPSS sur les rendements journaliers
# ------------------------------------------------------------

return_results <- list()

if ("return" %in% names(daily)) {

  for (cc in sort(unique(daily$country))) {

    x <- daily %>%
      filter(country == cc) %>%
      arrange(date) %>%
      pull(return)

    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) < 100) next

    a <- safe_adf(x)
    p <- safe_pp(x)
    k <- safe_kpss(x)

    return_results[[length(return_results) + 1]] <-
      data.frame(
        country = cc,
        variable = "return",
        N = length(x),

        ADF_statistic = a["statistic"],
        ADF_pvalue = a["p_value"],

        PP_statistic = p["statistic"],
        PP_pvalue = p["p_value"],

        KPSS_statistic = k["statistic"],
        KPSS_pvalue = k["p_value"],

        ADF_stationary_5pct =
          !is.na(a["p_value"]) && a["p_value"] < 0.05,

        PP_stationary_5pct =
          !is.na(p["p_value"]) && p["p_value"] < 0.05,

        KPSS_stationary_5pct =
          !is.na(k["p_value"]) && k["p_value"] >= 0.05
      )
  }
}

returns_stationarity <- bind_rows(return_results)

write_csv(
  returns_stationarity,
  file.path(out, "X4_RETURNS_STATIONARITY.csv")
)

cat("[OK] Stationnarite des rendements :", nrow(returns_stationarity), "\n")

# ------------------------------------------------------------
# 7. ARCH-LM sur les rendements journaliers
# ------------------------------------------------------------

arch_results <- list()

if ("return" %in% names(daily)) {

  for (cc in sort(unique(daily$country))) {

    x <- daily %>%
      filter(country == cc) %>%
      arrange(date) %>%
      pull(return)

    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) < 100) next

    # 12 retards mensuels approx. / diagnostic standard
    z <- tryCatch(
      FinTS::ArchTest(x, lags = 12),
      error = function(e) NULL
    )

    if (is.null(z)) next

    arch_results[[length(arch_results) + 1]] <-
      data.frame(
        country = cc,
        N = length(x),
        lags = 12,
        ARCH_LM_statistic = unname(z$statistic),
        ARCH_LM_pvalue = z$p.value,
        ARCH_effect_5pct =
          !is.na(z$p.value) && z$p.value < 0.05
      )
  }
}

arch <- bind_rows(arch_results)

write_csv(
  arch,
  file.path(out, "X4_ARCH_LM.csv")
)

cat("[OK] ARCH-LM :", nrow(arch), "pays\n")

# ------------------------------------------------------------
# 8. LJUNG-BOX sur rendements
# ------------------------------------------------------------

lb_results <- list()

if ("return" %in% names(daily)) {

  for (cc in sort(unique(daily$country))) {

    x <- daily %>%
      filter(country == cc) %>%
      arrange(date) %>%
      pull(return)

    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) < 100) next

    # Rendements
    lb1 <- tryCatch(
      Box.test(
        x,
        lag = 12,
        type = "Ljung-Box"
      ),
      error = function(e) NULL
    )

    # Rendements au carre
    lb2 <- tryCatch(
      Box.test(
        x^2,
        lag = 12,
        type = "Ljung-Box"
      ),
      error = function(e) NULL
    )

    if (is.null(lb1) || is.null(lb2)) next

    lb_results[[length(lb_results) + 1]] <-
      data.frame(
        country = cc,
        N = length(x),
        lags = 12,

        LB_returns_statistic = unname(lb1$statistic),
        LB_returns_pvalue = lb1$p.value,

        LB_squared_returns_statistic = unname(lb2$statistic),
        LB_squared_returns_pvalue = lb2$p.value,

        autocorrelation_returns_5pct =
          lb1$p.value < 0.05,

        dependence_squared_returns_5pct =
          lb2$p.value < 0.05
      )
  }
}

ljung_box <- bind_rows(lb_results)

write_csv(
  ljung_box,
  file.path(out, "X4_LJUNG_BOX.csv")
)

cat("[OK] Ljung-Box :", nrow(ljung_box), "pays\n")

# ------------------------------------------------------------
# 9. VIF - multicollinearite
# ------------------------------------------------------------

# VIF comme diagnostic complementaire.
# On utilise les variables explicatives disponibles
# dans le panel mensuel.

vif_vars <- intersect(
  c(
    "GPRT",
    "GPRA",
    "SUP",
    "DEM",
    "PRE",
    "VIX",
    "interest_rate",
    "exchange_rate",
    "GDP",
    "inflation"
  ),
  names(monthly)
)

vif_data <- monthly %>%
  select(all_of(vif_vars)) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(everything(), is.finite))

vif_results <- data.frame()

if (length(vif_vars) >= 2 && nrow(vif_data) > 50) {

  # Regression auxiliaire uniquement pour diagnostiquer
  # la collinearite entre variables explicatives.
  form <- as.formula(
    paste(vif_vars[1], "~", paste(vif_vars[-1], collapse = " + "))
  )

  aux <- tryCatch(
    lm(form, data = vif_data),
    error = function(e) NULL
  )

  if (!is.null(aux)) {

    vv <- tryCatch(
      car::vif(aux),
      error = function(e) NULL
    )

    if (!is.null(vv)) {

      if (is.matrix(vv)) {

        vif_results <- data.frame(
          variable = rownames(vv),
          VIF = vv[, 1],
          GVIF = vv[, 1],
          stringsAsFactors = FALSE
        )

      } else {

        vif_results <- data.frame(
          variable = names(vv),
          VIF = as.numeric(vv),
          GVIF = NA_real_,
          stringsAsFactors = FALSE
        )
      }

      vif_results$interpretation <- ifelse(
        vif_results$VIF < 5,
        "Faible / acceptable",
        ifelse(
          vif_results$VIF < 10,
          "A surveiller",
          "Multicollinearite elevee"
        )
      )
    }
  }
}

write_csv(
  vif_results,
  file.path(out, "X4_VIF.csv")
)

cat("[OK] VIF :", nrow(vif_results), "variables\n")

# ------------------------------------------------------------
# 10. Résumé global
# ------------------------------------------------------------

summary_table <- data.frame(
  Test = c(
    "ADF",
    "Phillips-Perron",
    "KPSS",
    "ARCH-LM",
    "Ljung-Box rendements",
    "Ljung-Box rendements carres",
    "VIF"
  ),

  Objet = c(
    "Stationnarite",
    "Stationnarite",
    "Stationnarite",
    "Heteroscedasticite conditionnelle",
    "Autocorrelation",
    "Dependance dans la variance",
    "Multicollinearite"
  ),

  Fichier = c(
    "X4_STATIONARITY_ADF_PP_KPSS.csv",
    "X4_STATIONARITY_ADF_PP_KPSS.csv",
    "X4_STATIONARITY_ADF_PP_KPSS.csv",
    "X4_ARCH_LM.csv",
    "X4_LJUNG_BOX.csv",
    "X4_LJUNG_BOX.csv",
    "X4_VIF.csv"
  )
)

write_csv(
  summary_table,
  file.path(out, "X4_SUMMARY.csv")
)

# ------------------------------------------------------------
# 11. Export Excel
# ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "Resume")
writeData(wb, "Resume", summary_table)

addWorksheet(wb, "ADF_PP_KPSS")
writeData(wb, "ADF_PP_KPSS", stationarity)

addWorksheet(wb, "Returns_Stationarity")
writeData(wb, "Returns_Stationarity", returns_stationarity)

addWorksheet(wb, "ARCH_LM")
writeData(wb, "ARCH_LM", arch)

addWorksheet(wb, "Ljung_Box")
writeData(wb, "Ljung_Box", ljung_box)

addWorksheet(wb, "VIF")
writeData(wb, "VIF", vif_results)

excel_file <- file.path(
  out,
  "X4_TESTS_ECONOMETRIQUES_PRELIMINAIRES.xlsx"
)

saveWorkbook(
  wb,
  excel_file,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 12. Rapport TXT
# ------------------------------------------------------------

sink(
  file.path(out, "X4_RAPPORT_TESTS_PRELIMINAIRES.txt")
)

cat("============================================================\n")
cat(" RIMEH - RAPPORT DES TESTS ECONOMETRIQUES PRELIMINAIRES\n")
cat("============================================================\n\n")

cat("Nombre d'observations mensuelles :", nrow(monthly), "\n")
cat("Nombre d'observations journalieres :", nrow(daily), "\n")
cat("Nombre de pays :", length(countries), "\n\n")

cat("------------------------------------------------------------\n")
cat("STATIONNARITE DES VARIABLES MENSUELLES\n")
cat("------------------------------------------------------------\n")
print(stationarity)

cat("\n------------------------------------------------------------\n")
cat("STATIONNARITE DES RENDEMENTS\n")
cat("------------------------------------------------------------\n")
print(returns_stationarity)

cat("\n------------------------------------------------------------\n")
cat("ARCH-LM\n")
cat("------------------------------------------------------------\n")
print(arch)

cat("\n------------------------------------------------------------\n")
cat("LJUNG-BOX\n")
cat("------------------------------------------------------------\n")
print(ljung_box)

cat("\n------------------------------------------------------------\n")
cat("VIF\n")
cat("------------------------------------------------------------\n")
print(vif_results)

cat("\n============================================================\n")
cat("FIN DES TESTS\n")
cat("============================================================\n")

sink()

# ------------------------------------------------------------
# 13. Affichage final
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" TESTS X.4 TERMINES\n")
cat("============================================================\n\n")

cat("Fichiers produits dans :\n")
cat(normalizePath(out), "\n\n")

cat(" - X4_STATIONARITY_ADF_PP_KPSS.csv\n")
cat(" - X4_RETURNS_STATIONARITY.csv\n")
cat(" - X4_ARCH_LM.csv\n")
cat(" - X4_LJUNG_BOX.csv\n")
cat(" - X4_VIF.csv\n")
cat(" - X4_SUMMARY.csv\n")
cat(" - X4_TESTS_ECONOMETRIQUES_PRELIMINAIRES.xlsx\n")
cat(" - X4_RAPPORT_TESTS_PRELIMINAIRES.txt\n\n")

cat("============================================================\n")