# ============================================================
# RIMEH - ETAPES 3 A 7
# DESCRIPTIVES + CORRELATIONS + STATIONNARITE + VIF + ARCH-LM
# ============================================================

options(stringsAsFactors = FALSE)

P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

DATA <- file.path(P, "data", "processed",
                  "monthly_model_data_INFLATION_FIX.csv")

OUT <- file.path(P, "results", "preliminary_tests")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat(" RIMEH - TESTS ECONOMETRIQUES PRELIMINAIRES\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "dplyr",
  "readr",
  "tseries",
  "lmtest",
  "car",
  "FinTS"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(dplyr)
library(readr)
library(tseries)
library(lmtest)
library(car)
library(FinTS)

# ------------------------------------------------------------
# 2. CHARGEMENT
# ------------------------------------------------------------

df <- read_csv(DATA, show_col_types = FALSE)

cat("Fichier :", DATA, "\n")
cat("Dimensions :", nrow(df), "x", ncol(df), "\n\n")

# Conversion date
df[[1]] <- as.Date(df[[1]])

# Variables principales
vars <- c(
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX",
  "inflation",
  "interest_rate",
  "exchange_rate",
  "GDP",
  "RV.x"
)

vars <- vars[vars %in% names(df)]

cat("Variables analysees :\n")
print(vars)

# ------------------------------------------------------------
# 3. STATISTIQUES DESCRIPTIVES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 3 - STATISTIQUES DESCRIPTIVES\n")
cat("============================================================\n")

desc_list <- lapply(vars, function(v) {

  x <- suppressWarnings(as.numeric(df[[v]]))
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(data.frame(
      Variable = v,
      N = 0,
      Min = NA,
      Median = NA,
      Mean = NA,
      Max = NA,
      SD = NA,
      Skewness = NA,
      Kurtosis = NA
    ))
  }

  m <- mean(x)
  s <- sd(x)

  skew <- if (s > 0) {
    mean((x - m)^3) / s^3
  } else NA

  kurt <- if (s > 0) {
    mean((x - m)^4) / s^4
  } else NA

  data.frame(
    Variable = v,
    N = length(x),
    Min = min(x),
    Median = median(x),
    Mean = m,
    Max = max(x),
    SD = s,
    Skewness = skew,
    Kurtosis = kurt
  )
})

DESC <- bind_rows(desc_list)

write.csv(
  DESC,
  file.path(OUT, "TABLEAU_01_STATISTIQUES_DESCRIPTIVES.csv"),
  row.names = FALSE
)

print(DESC)

# ------------------------------------------------------------
# 4. MATRICE DE CORRELATION PEARSON
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 4A - CORRELATION PEARSON\n")
cat("============================================================\n")

corr_vars <- c(
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX",
  "inflation"
)

corr_vars <- corr_vars[corr_vars %in% names(df)]

CORR_DATA <- df[, corr_vars, drop = FALSE]

CORR_PEARSON <- cor(
  CORR_DATA,
  use = "pairwise.complete.obs",
  method = "pearson"
)

write.csv(
  CORR_PEARSON,
  file.path(OUT, "TABLEAU_02_CORRELATION_PEARSON.csv")
)

print(round(CORR_PEARSON, 4))

# ------------------------------------------------------------
# 5. MATRICE SPEARMAN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 4B - CORRELATION SPEARMAN\n")
cat("============================================================\n")

CORR_SPEARMAN <- cor(
  CORR_DATA,
  use = "pairwise.complete.obs",
  method = "spearman"
)

write.csv(
  CORR_SPEARMAN,
  file.path(OUT, "TABLEAU_03_CORRELATION_SPEARMAN.csv")
)

print(round(CORR_SPEARMAN, 4))

# ------------------------------------------------------------
# 6. ADF + PP + KPSS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 5 - STATIONNARITE\n")
cat("============================================================\n")

station_vars <- c(
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX",
  "inflation"
)

station_vars <- station_vars[station_vars %in% names(df)]

ADF_RESULTS <- data.frame()
PP_RESULTS <- data.frame()
KPSS_RESULTS <- data.frame()

for (v in station_vars) {

  cat("\nVariable :", v, "\n")

  x <- suppressWarnings(as.numeric(df[[v]]))
  x <- x[is.finite(x)]

  if (length(x) < 30 || sd(x) == 0) {
    cat("  Serie insuffisante.\n")
    next
  }

  # ADF
  adf <- tryCatch(
    adf.test(x),
    error = function(e) NULL
  )

  # Phillips-Perron
  pp <- tryCatch(
    pp.test(x),
    error = function(e) NULL
  )

  # KPSS
  kpss <- tryCatch(
    kpss.test(x),
    error = function(e) NULL
  )

  ADF_RESULTS <- rbind(
    ADF_RESULTS,
    data.frame(
      Variable = v,
      Statistic = if (!is.null(adf)) unname(adf$statistic) else NA,
      P_Value = if (!is.null(adf)) adf$p.value else NA,
      Decision_5pct =
        if (!is.null(adf) && adf$p.value < 0.05)
          "Stationnaire"
        else
          "Non stationnaire / H0 non rejetee"
    )
  )

  PP_RESULTS <- rbind(
    PP_RESULTS,
    data.frame(
      Variable = v,
      Statistic = if (!is.null(pp)) unname(pp$statistic) else NA,
      P_Value = if (!is.null(pp)) pp$p.value else NA,
      Decision_5pct =
        if (!is.null(pp) && pp$p.value < 0.05)
          "Stationnaire"
        else
          "Non stationnaire / H0 non rejetee"
    )
  )

  KPSS_RESULTS <- rbind(
    KPSS_RESULTS,
    data.frame(
      Variable = v,
      Statistic = if (!is.null(kpss)) unname(kpss$statistic) else NA,
      P_Value = if (!is.null(kpss)) kpss$p.value else NA,
      Decision_5pct =
        if (!is.null(kpss) && kpss$p.value > 0.05)
          "Stationnaire"
        else
          "Non stationnaire / H0 rejetee"
    )
  )
}

write.csv(
  ADF_RESULTS,
  file.path(OUT, "TABLEAU_04_TEST_ADF.csv"),
  row.names = FALSE
)

write.csv(
  PP_RESULTS,
  file.path(OUT, "TABLEAU_05_TEST_PP.csv"),
  row.names = FALSE
)

write.csv(
  KPSS_RESULTS,
  file.path(OUT, "TABLEAU_06_TEST_KPSS.csv"),
  row.names = FALSE
)

cat("\nADF :\n")
print(ADF_RESULTS)

cat("\nPP :\n")
print(PP_RESULTS)

cat("\nKPSS :\n")
print(KPSS_RESULTS)

# ------------------------------------------------------------
# 7. VIF
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 6 - MULTICOLINEARITE / VIF\n")
cat("============================================================\n")

vif_vars <- c(
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX"
)

vif_vars <- vif_vars[vif_vars %in% names(df)]

vif_data <- df[, vif_vars, drop = FALSE]
vif_data <- vif_data[complete.cases(vif_data), ]

VIF_RESULTS <- data.frame()

if (ncol(vif_data) >= 2 && nrow(vif_data) > 20) {

  formula_vif <- as.formula(
    paste(
      vif_vars[1],
      "~",
      paste(vif_vars[-1], collapse = " + ")
    )
  )

  fit_vif <- lm(formula_vif, data = vif_data)

  v <- tryCatch(
    vif(fit_vif),
    error = function(e) NULL
  )

  if (!is.null(v)) {

    if (is.matrix(v)) {

      VIF_RESULTS <- data.frame(
        Variable = rownames(v),
        GVIF = v[, 1],
        Df = v[, 2],
        GVIF_Adjusted = v[, 3]
      )

    } else {

      VIF_RESULTS <- data.frame(
        Variable = names(v),
        VIF = as.numeric(v)
      )
    }
  }
}

write.csv(
  VIF_RESULTS,
  file.path(OUT, "TABLEAU_07_VIF.csv"),
  row.names = FALSE
)

print(VIF_RESULTS)

# ------------------------------------------------------------
# 8. ARCH-LM
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 7 - TEST ARCH-LM\n")
cat("============================================================\n")

ARCH_RESULTS <- data.frame()

# On teste les principales séries de volatilité / risque
arch_vars <- c(
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX",
  "RV.x"
)

arch_vars <- arch_vars[arch_vars %in% names(df)]

for (v in arch_vars) {

  x <- suppressWarnings(as.numeric(df[[v]]))
  x <- x[is.finite(x)]

  if (length(x) < 100 || sd(x) == 0) {
    next
  }

  # Différenciation uniquement pour rendre le test
  # plus adapté aux séries présentant une tendance éventuelle
  dx <- diff(x)

  if (length(dx) < 100 || sd(dx) == 0) {
    dx <- x
  }

  test_arch <- tryCatch(
    ArchTest(dx, lags = 12),
    error = function(e) NULL
  )

  if (!is.null(test_arch)) {

    ARCH_RESULTS <- rbind(
      ARCH_RESULTS,
      data.frame(
        Variable = v,
        LM_Statistic = unname(test_arch$statistic),
        P_Value = test_arch$p.value,
        Decision_5pct =
          if (test_arch$p.value < 0.05)
            "Effets ARCH presents"
          else
            "Pas d'effet ARCH detecte"
      )
    )
  }
}

write.csv(
  ARCH_RESULTS,
  file.path(OUT, "TABLEAU_08_ARCH_LM.csv"),
  row.names = FALSE
)

print(ARCH_RESULTS)

# ------------------------------------------------------------
# 9. RESUME
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ANALYSE TERMINEE\n")
cat("============================================================\n")

cat("\nDossier des resultats :\n")
cat(OUT, "\n\n")

cat("Fichiers produits :\n")

print(list.files(OUT))

cat("\n============================================================\n")
cat(" FIN RIMEH - ETAPES 3 A 7\n")
cat("============================================================\n")
