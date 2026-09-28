options(stringsAsFactors = FALSE)

P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

DATA <- file.path(
  P,
  "data",
  "processed",
  "monthly_model_data_INFLATION_FIX.csv"
)

OUT <- file.path(
  P,
  "results",
  "preliminary_tests"
)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat(" RIMEH - ETAPE 5.1 + AUDIT COMPLET RV\n")
cat("============================================================\n\n")

# ============================================================
# PACKAGES
# ============================================================

packages <- c(
  "readr",
  "dplyr",
  "tseries"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(
      p,
      repos = "https://cloud.r-project.org"
    )
  }
}

library(readr)
library(dplyr)
library(tseries)

# ============================================================
# 1. CHARGEMENT
# ============================================================

df <- read_csv(
  DATA,
  show_col_types = FALSE
)

cat("Fichier analyse :\n")
cat(DATA, "\n\n")

cat("Dimensions :", nrow(df), "x", ncol(df), "\n\n")

# Date
df[[1]] <- as.Date(df[[1]])

# ============================================================
# 2. ETAPE 5.1 - DIAGNOSTIC INFLATION
# ============================================================

cat("\n============================================================\n")
cat(" 5.1 - DIAGNOSTIC APPROFONDI DE LA STATIONNARITE\n")
cat("============================================================\n")

inflation <- suppressWarnings(
  as.numeric(df[["inflation"]])
)

inflation <- inflation[
  is.finite(inflation)
]

cat("\nNombre observations inflation :", length(inflation), "\n")
cat("Min :", min(inflation), "\n")
cat("Max :", max(inflation), "\n")
cat("Moyenne :", mean(inflation), "\n")
cat("SD :", sd(inflation), "\n")

# ------------------------------------------------------------
# ADF niveau - constante
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("ADF - NIVEAU - CONSTANTE\n")
cat("------------------------------------------------------------\n")

ADF_LEVEL_CONST <- tryCatch(
  adf.test(
    inflation,
    alternative = "stationary"
  ),
  error = function(e) NULL
)

if (!is.null(ADF_LEVEL_CONST)) {
  print(ADF_LEVEL_CONST)
}

# ------------------------------------------------------------
# ADF niveau - tendance
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("ADF - NIVEAU - TENDANCE\n")
cat("------------------------------------------------------------\n")

# tseries::adf.test ne permet pas directement de choisir
# la tendance. On utilise une régression ADF complémentaire.

adf_manual <- function(x, trend = FALSE, lags = 12) {

  x <- as.numeric(x)
  x <- x[is.finite(x)]

  dx <- diff(x)
  lagged <- x[-length(x)]

  n <- length(dx)

  if (n <= lags + 5) {
    return(NULL)
  }

  Y <- dx[(lags + 1):n]

  X <- data.frame(
    lagged = lagged[(lags + 1):n]
  )

  if (trend) {
    X$trend <- seq_len(nrow(X))
  }

  for (j in 1:lags) {
    X[[paste0("dlag", j)]] <-
      dx[(lags + 1 - j):(n - j)]
  }

  fit <- lm(Y ~ ., data = X)

  coef_lag <- summary(fit)$coefficients[
    "lagged",
    ,
    drop = FALSE
  ]

  data.frame(
    statistic = coef_lag[1, "t value"],
    p_value_lm = coef_lag[1, "Pr(>|t|)"],
    lags = lags,
    trend = trend
  )
}

ADF_MANUAL_CONST <- adf_manual(
  inflation,
  trend = FALSE,
  lags = 12
)

ADF_MANUAL_TREND <- adf_manual(
  inflation,
  trend = TRUE,
  lags = 12
)

cat("\nADF manuel - constante :\n")
print(ADF_MANUAL_CONST)

cat("\nADF manuel - constante + tendance :\n")
print(ADF_MANUAL_TREND)

# ------------------------------------------------------------
# PP
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("PHILLIPS-PERRON\n")
cat("------------------------------------------------------------\n")

PP_LEVEL <- tryCatch(
  pp.test(inflation),
  error = function(e) NULL
)

if (!is.null(PP_LEVEL)) {
  print(PP_LEVEL)
}

# ------------------------------------------------------------
# KPSS - niveau
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("KPSS - NIVEAU / CONSTANTE\n")
cat("------------------------------------------------------------\n")

KPSS_LEVEL <- tryCatch(
  kpss.test(
    inflation,
    null = "Level"
  ),
  error = function(e) NULL
)

if (!is.null(KPSS_LEVEL)) {
  print(KPSS_LEVEL)
}

# ------------------------------------------------------------
# KPSS - tendance
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("KPSS - TENDANCE\n")
cat("------------------------------------------------------------\n")

KPSS_TREND <- tryCatch(
  kpss.test(
    inflation,
    null = "Trend"
  ),
  error = function(e) NULL
)

if (!is.null(KPSS_TREND)) {
  print(KPSS_TREND)
}

# ============================================================
# 3. PREMIERE DIFFERENCE INFLATION
# ============================================================

cat("\n============================================================\n")
cat(" INFLATION - PREMIERE DIFFERENCE\n")
cat("============================================================\n")

inflation_diff <- diff(inflation)

inflation_diff <- inflation_diff[
  is.finite(inflation_diff)
]

cat("N :", length(inflation_diff), "\n")
cat("Min :", min(inflation_diff), "\n")
cat("Max :", max(inflation_diff), "\n")
cat("Moyenne :", mean(inflation_diff), "\n")
cat("SD :", sd(inflation_diff), "\n")

ADF_DIFF <- tryCatch(
  adf.test(inflation_diff),
  error = function(e) NULL
)

PP_DIFF <- tryCatch(
  pp.test(inflation_diff),
  error = function(e) NULL
)

KPSS_DIFF <- tryCatch(
  kpss.test(
    inflation_diff,
    null = "Level"
  ),
  error = function(e) NULL
)

cat("\nADF difference :\n")
if (!is.null(ADF_DIFF)) print(ADF_DIFF)

cat("\nPP difference :\n")
if (!is.null(PP_DIFF)) print(PP_DIFF)

cat("\nKPSS difference :\n")
if (!is.null(KPSS_DIFF)) print(KPSS_DIFF)

# ============================================================
# 4. RESUME STATIONNARITE
# ============================================================

stationarity_summary <- data.frame(
  Test = c(
    "ADF niveau",
    "ADF manuel constante",
    "ADF manuel tendance",
    "PP niveau",
    "KPSS niveau",
    "KPSS tendance",
    "ADF difference",
    "PP difference",
    "KPSS difference"
  ),
  Statistic = c(
    if (!is.null(ADF_LEVEL_CONST))
      unname(ADF_LEVEL_CONST$statistic) else NA,
    if (!is.null(ADF_MANUAL_CONST))
      ADF_MANUAL_CONST$statistic else NA,
    if (!is.null(ADF_MANUAL_TREND))
      ADF_MANUAL_TREND$statistic else NA,
    if (!is.null(PP_LEVEL))
      unname(PP_LEVEL$statistic) else NA,
    if (!is.null(KPSS_LEVEL))
      unname(KPSS_LEVEL$statistic) else NA,
    if (!is.null(KPSS_TREND))
      unname(KPSS_TREND$statistic) else NA,
    if (!is.null(ADF_DIFF))
      unname(ADF_DIFF$statistic) else NA,
    if (!is.null(PP_DIFF))
      unname(PP_DIFF$statistic) else NA,
    if (!is.null(KPSS_DIFF))
      unname(KPSS_DIFF$statistic) else NA
  ),
  P_Value = c(
    if (!is.null(ADF_LEVEL_CONST))
      ADF_LEVEL_CONST$p.value else NA,
    if (!is.null(ADF_MANUAL_CONST))
      ADF_MANUAL_CONST$p_value_lm else NA,
    if (!is.null(ADF_MANUAL_TREND))
      ADF_MANUAL_TREND$p_value_lm else NA,
    if (!is.null(PP_LEVEL))
      PP_LEVEL$p.value else NA,
    if (!is.null(KPSS_LEVEL))
      KPSS_LEVEL$p.value else NA,
    if (!is.null(KPSS_TREND))
      KPSS_TREND$p.value else NA,
    if (!is.null(ADF_DIFF))
      ADF_DIFF$p.value else NA,
    if (!is.null(PP_DIFF))
      PP_DIFF$p.value else NA,
    if (!is.null(KPSS_DIFF))
      KPSS_DIFF$p.value else NA
  )
)

write.csv(
  stationarity_summary,
  file.path(
    OUT,
    "TABLEAU_09_STATIONNARITE_INFLATION_COMPLET.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 5. AUDIT COMPLET DE RV
# ============================================================

cat("\n============================================================\n")
cat(" AUDIT COMPLET DE RV\n")
cat("============================================================\n")

rv_name <- NULL

if ("RV.x" %in% names(df)) {
  rv_name <- "RV.x"
} else if ("RV" %in% names(df)) {
  rv_name <- "RV"
} else if ("RV.y" %in% names(df)) {
  rv_name <- "RV.y"
}

if (is.null(rv_name)) {

  cat("\n!!! AUCUNE VARIABLE RV TROUVEE !!!\n")

} else {

  cat("\nVariable RV retenue :", rv_name, "\n")

  rv <- suppressWarnings(
    as.numeric(df[[rv_name]])
  )

  # ----------------------------------------------------------
  # Qualite globale
  # ----------------------------------------------------------

  n_total <- length(rv)

  n_na <- sum(is.na(rv))
  n_inf <- sum(is.infinite(rv))
  n_finite <- sum(is.finite(rv))
  n_zero <- sum(rv == 0, na.rm = TRUE)
  n_negative <- sum(rv < 0, na.rm = TRUE)
  n_positive <- sum(rv > 0, na.rm = TRUE)

  cat("\nN total :", n_total)
  cat("\nNA :", n_na)
  cat("\nInf :", n_inf)
  cat("\nFinite :", n_finite)
  cat("\nZeros :", n_zero)
  cat("\nNegatifs :", n_negative)
  cat("\nPositifs :", n_positive, "\n")

  rv_valid <- rv[
    is.finite(rv)
  ]

  if (length(rv_valid) > 0) {

    cat("\nStatistiques RV :\n")

    print(
      summary(rv_valid)
    )

    cat("\nEcart-type :", sd(rv_valid), "\n")

    cat("\nQuantiles :\n")

    print(
      quantile(
        rv_valid,
        probs = c(
          0,
          .01,
          .05,
          .10,
          .25,
          .50,
          .75,
          .90,
          .95,
          .99,
          1
        ),
        na.rm = TRUE
      )
    )

    cat("\nSkewness approx. :\n")

    m <- mean(rv_valid)
    s <- sd(rv_valid)

    skew <- if (s > 0) {
      mean((rv_valid - m)^3) / s^3
    } else NA

    kurt <- if (s > 0) {
      mean((rv_valid - m)^4) / s^4
    } else NA

    cat("Skewness :", skew, "\n")
    cat("Kurtosis :", kurt, "\n")
  }

  # ----------------------------------------------------------
  # RV par pays
  # ----------------------------------------------------------

  if ("country" %in% names(df)) {

    RV_COUNTRY <- df %>%
      mutate(
        RV_AUDIT = suppressWarnings(
          as.numeric(.data[[rv_name]])
        )
      ) %>%
      group_by(country) %>%
      summarise(
        N = n(),
        N_valid = sum(is.finite(RV_AUDIT)),
        N_NA = sum(is.na(RV_AUDIT)),
        N_Inf = sum(is.infinite(RV_AUDIT)),
        N_zero = sum(
          RV_AUDIT == 0,
          na.rm = TRUE
        ),
        N_negative = sum(
          RV_AUDIT < 0,
          na.rm = TRUE
        ),
        Min = ifelse(
          N_valid > 0,
          min(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        Median = ifelse(
          N_valid > 0,
          median(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        Mean = ifelse(
          N_valid > 0,
          mean(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        Max = ifelse(
          N_valid > 0,
          max(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        SD = ifelse(
          N_valid > 1,
          sd(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        .groups = "drop"
      )

    write.csv(
      RV_COUNTRY,
      file.path(
        OUT,
        "TABLEAU_10_RV_PAR_PAYS.csv"
      ),
      row.names = FALSE
    )

    cat("\n------------------------------------------------------------\n")
    cat("RV PAR PAYS\n")
    cat("------------------------------------------------------------\n")

    print(RV_COUNTRY)
  }

  # ----------------------------------------------------------
  # RV par année
  # ----------------------------------------------------------

  if (length(df[[1]]) == nrow(df)) {

    RV_YEAR <- df %>%
      mutate(
        YEAR = format(.data[[1]], "%Y"),
        RV_AUDIT = suppressWarnings(
          as.numeric(.data[[rv_name]])
        )
      ) %>%
      group_by(YEAR) %>%
      summarise(
        N = n(),
        N_valid = sum(is.finite(RV_AUDIT)),
        Mean = ifelse(
          N_valid > 0,
          mean(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        Median = ifelse(
          N_valid > 0,
          median(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        Max = ifelse(
          N_valid > 0,
          max(RV_AUDIT, na.rm = TRUE),
          NA
        ),
        .groups = "drop"
      )

    write.csv(
      RV_YEAR,
      file.path(
        OUT,
        "TABLEAU_11_RV_PAR_ANNEE.csv"
      ),
      row.names = FALSE
    )
  }

  # ----------------------------------------------------------
  # Dates dupliquées par pays
  # ----------------------------------------------------------

  if ("country" %in% names(df)) {

    DUPLICATES <- df %>%
      count(
        country,
        .data[[names(df)[1]]],
        name = "N"
      ) %>%
      filter(N > 1)

    write.csv(
      DUPLICATES,
      file.path(
        OUT,
        "TABLEAU_12_RV_DATES_DUPLIQUEES.csv"
      ),
      row.names = FALSE
    )

    cat("\nDates pays dupliquées :", nrow(DUPLICATES), "\n")
  }

  # ----------------------------------------------------------
  # Couverture mensuelle par pays
  # ----------------------------------------------------------

  if ("country" %in% names(df)) {

    COVERAGE <- df %>%
      group_by(country) %>%
      summarise(
        Date_min = min(.data[[names(df)[1]]], na.rm = TRUE),
        Date_max = max(.data[[names(df)[1]]], na.rm = TRUE),
        N_months = n(),
        N_RV_valid = sum(
          is.finite(
            suppressWarnings(
              as.numeric(.data[[rv_name]])
            )
          )
        ),
        .groups = "drop"
      )

    write.csv(
      COVERAGE,
      file.path(
        OUT,
        "TABLEAU_13_RV_COUVERTURE_PAYS.csv"
      ),
      row.names = FALSE
    )

    cat("\n------------------------------------------------------------\n")
    cat("COUVERTURE RV\n")
    cat("------------------------------------------------------------\n")

    print(COVERAGE)
  }

  # ----------------------------------------------------------
  # Relation RV / n_daily
  # ----------------------------------------------------------

  if ("n_daily" %in% names(df)) {

    nd <- suppressWarnings(
      as.numeric(df[["n_daily"]])
    )

    REL <- data.frame(
      n_daily = nd,
      RV = rv
    )

    REL <- REL[
      is.finite(REL$n_daily) &
      is.finite(REL$RV),
      ,
      drop = FALSE
    ]

    if (nrow(REL) > 10) {

      cat("\n------------------------------------------------------------\n")
      cat("CORRELATION RV / N_DAILY\n")
      cat("------------------------------------------------------------\n")

      cat(
        "Pearson :",
        cor(
          REL$RV,
          REL$n_daily,
          method = "pearson"
        ),
        "\n"
      )

      cat(
        "Spearman :",
        cor(
          REL$RV,
          REL$n_daily,
          method = "spearman"
        ),
        "\n"
      )

      write.csv(
        REL,
        file.path(
          OUT,
          "TABLEAU_14_RV_NDAILY.csv"
        ),
        row.names = FALSE
      )
    }
  }

  # ----------------------------------------------------------
  # Présence RV.y
  # ----------------------------------------------------------

  cat("\n------------------------------------------------------------\n")
  cat("VERIFICATION RV.x / RV.y\n")
  cat("------------------------------------------------------------\n")

  if ("RV.x" %in% names(df)) {
    cat("RV.x : PRESENTE\n")
  } else {
    cat("RV.x : ABSENTE\n")
  }

  if ("RV.y" %in% names(df)) {
    cat("RV.y : PRESENTE\n")

    rvy <- suppressWarnings(
      as.numeric(df[["RV.y"]])
    )

    cat(
      "RV.y valides :",
      sum(is.finite(rvy)),
      "\n"
    )

    if ("RV.x" %in% names(df)) {

      rvx <- suppressWarnings(
        as.numeric(df[["RV.x"]])
      )

      both <- is.finite(rvx) & is.finite(rvy)

      if (sum(both) > 0) {

        cat(
          "RV.x et RV.y simultanément valides :",
          sum(both),
          "\n"
        )

        cat(
          "Différence absolue moyenne :",
          mean(
            abs(rvx[both] - rvy[both])
          ),
          "\n"
        )
      }
    }

  } else {
    cat("RV.y : ABSENTE\n")
  }

}

# ============================================================
# 6. AUDIT DAILY_MODEL_DATA
# ============================================================

DAILY <- file.path(
  P,
  "data",
  "processed",
  "daily_model_data.csv"
)

cat("\n============================================================\n")
cat(" AUDIT DE LA BASE DAILY\n")
cat("============================================================\n")

if (file.exists(DAILY)) {

  daily <- read_csv(
    DAILY,
    show_col_types = FALSE
  )

  cat(
    "daily_model_data.csv trouve.\n"
  )

  cat(
    "Dimensions :",
    nrow(daily),
    "x",
    ncol(daily),
    "\n"
  )

  cat("\nColonnes daily :\n")
  print(names(daily))

  if ("country" %in% names(daily)) {

    cat("\nNombre de pays daily :",
        length(unique(daily[["country"]])),
        "\n")

    DAILY_COUNTRY <- daily %>%
      group_by(country) %>%
      summarise(
        N_daily = n(),
        Date_min = min(
          as.Date(.data[[names(daily)[1]]]),
          na.rm = TRUE
        ),
        Date_max = max(
          as.Date(.data[[names(daily)[1]]]),
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    write.csv(
      DAILY_COUNTRY,
      file.path(
        OUT,
        "TABLEAU_15_DAILY_COUVERTURE.csv"
      ),
      row.names = FALSE
    )

    print(DAILY_COUNTRY)
  }

} else {

  cat(
    "daily_model_data.csv NON TROUVE.\n"
  )
}

# ============================================================
# 7. RAPPORT FINAL AUTOMATIQUE
# ============================================================

REPORT <- data.frame(
  Controle = c(
    "Inflation_N",
    "Inflation_NA",
    "RV_variable",
    "RV_N",
    "RV_NA",
    "RV_Inf",
    "RV_zero",
    "RV_negative",
    "RV_valid",
    "RV_positif"
  ),
  Valeur = c(
    length(inflation),
    sum(is.na(df[["inflation"]])),
    ifelse(is.null(rv_name), "ABSENTE", rv_name),
    ifelse(
      is.null(rv_name),
      NA,
      length(rv)
    ),
    ifelse(
      is.null(rv_name),
      NA,
      sum(is.na(rv))
    ),
    ifelse(
      is.null(rv_name),
      NA,
      sum(is.infinite(rv))
    ),
    ifelse(
      is.null(rv_name),
      NA,
      sum(rv == 0, na.rm = TRUE)
    ),
    ifelse(
      is.null(rv_name),
      NA,
      sum(rv < 0, na.rm = TRUE)
    ),
    ifelse(
      is.null(rv_name),
      NA,
      sum(is.finite(rv))
    ),
    ifelse(
      is.null(rv_name),
      NA,
      sum(rv > 0, na.rm = TRUE)
    )
  )
)

write.csv(
  REPORT,
  file.path(
    OUT,
    "TABLEAU_16_RAPPORT_FINAL_ETAPE_5_1_RV.csv"
  ),
  row.names = FALSE
)

cat("\n============================================================\n")
cat(" ETAPE 5.1 + AUDIT RV TERMINE\n")
cat("============================================================\n")

cat("\nFichiers produits dans :\n")
cat(OUT, "\n\n")

print(
  list.files(
    OUT,
    pattern = "TABLEAU_(09|10|11|12|13|14|15|16)"
  )
)

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
