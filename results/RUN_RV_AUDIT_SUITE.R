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

library(readr)
library(dplyr)

cat("\n============================================================\n")
cat(" RIMEH - SUITE AUDIT RV\n")
cat("============================================================\n\n")

df <- read_csv(DATA, show_col_types = FALSE)

date_col <- names(df)[1]

df[[date_col]] <- as.Date(df[[date_col]])

rv_name <- if ("RV.x" %in% names(df)) {
  "RV.x"
} else if ("RV" %in% names(df)) {
  "RV"
} else if ("RV.y" %in% names(df)) {
  "RV.y"
} else {
  NA_character_
}

cat("Date :", date_col, "\n")
cat("RV retenue :", rv_name, "\n")
cat("Dimensions :", nrow(df), "x", ncol(df), "\n")

# ============================================================
# 1. RV PAR ANNEE
# ============================================================

cat("\n============================================================\n")
cat(" RV PAR ANNEE\n")
cat("============================================================\n")

rv_year <- df[[rv_name]]

RV_YEAR <- data.frame(
  DATE = df[[date_col]],
  COUNTRY = if ("country" %in% names(df)) df[["country"]] else NA,
  RV = suppressWarnings(as.numeric(rv_year))
)

RV_YEAR$YEAR <- format(RV_YEAR$DATE, "%Y")

RV_YEAR_SUMMARY <- RV_YEAR %>%
  group_by(YEAR) %>%
  summarise(
    N = n(),
    N_valid = sum(is.finite(RV)),
    Mean = ifelse(
      N_valid > 0,
      mean(RV, na.rm = TRUE),
      NA
    ),
    Median = ifelse(
      N_valid > 0,
      median(RV, na.rm = TRUE),
      NA
    ),
    SD = ifelse(
      N_valid > 1,
      sd(RV, na.rm = TRUE),
      NA
    ),
    Min = ifelse(
      N_valid > 0,
      min(RV, na.rm = TRUE),
      NA
    ),
    Max = ifelse(
      N_valid > 0,
      max(RV, na.rm = TRUE),
      NA
    ),
    .groups = "drop"
  )

write.csv(
  RV_YEAR_SUMMARY,
  file.path(
    OUT,
    "TABLEAU_11_RV_PAR_ANNEE.csv"
  ),
  row.names = FALSE
)

print(RV_YEAR_SUMMARY)

# ============================================================
# 2. DATES DUPLIQUEES
# ============================================================

cat("\n============================================================\n")
cat(" DATES DUPLIQUEES PAR PAYS\n")
cat("============================================================\n")

if ("country" %in% names(df)) {

  DUPLICATES <- df %>%
    count(
      country,
      .data[[date_col]],
      name = "N"
    ) %>%
    filter(N > 1)

} else {

  DUPLICATES <- df %>%
    count(
      .data[[date_col]],
      name = "N"
    ) %>%
    filter(N > 1)
}

write.csv(
  DUPLICATES,
  file.path(
    OUT,
    "TABLEAU_12_RV_DATES_DUPLIQUEES.csv"
  ),
  row.names = FALSE
)

cat("Nombre de dates dupliquees :", nrow(DUPLICATES), "\n")

if (nrow(DUPLICATES) > 0) {
  print(DUPLICATES)
}

# ============================================================
# 3. COUVERTURE PAR PAYS
# ============================================================

cat("\n============================================================\n")
cat(" COUVERTURE RV PAR PAYS\n")
cat("============================================================\n")

if ("country" %in% names(df)) {

  COVERAGE <- df %>%
    group_by(country) %>%
    summarise(
      Date_min = min(
        .data[[date_col]],
        na.rm = TRUE
      ),
      Date_max = max(
        .data[[date_col]],
        na.rm = TRUE
      ),
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

  print(COVERAGE)
}

# ============================================================
# 4. RV / N_DAILY
# ============================================================

cat("\n============================================================\n")
cat(" RELATION RV / N_DAILY\n")
cat("============================================================\n")

if ("n_daily" %in% names(df)) {

  REL <- data.frame(
    RV = suppressWarnings(
      as.numeric(df[[rv_name]])
    ),
    n_daily = suppressWarnings(
      as.numeric(df[["n_daily"]])
    )
  )

  REL <- REL[
    is.finite(REL$RV) &
    is.finite(REL$n_daily),
    ,
    drop = FALSE
  ]

  cat("Observations valides :", nrow(REL), "\n")

  if (nrow(REL) > 10) {

    pearson <- cor(
      REL$RV,
      REL$n_daily,
      method = "pearson"
    )

    spearman <- cor(
      REL$RV,
      REL$n_daily,
      method = "spearman"
    )

    cat("Correlation Pearson  :", pearson, "\n")
    cat("Correlation Spearman :", spearman, "\n")

    RV_NDAILY <- data.frame(
      Statistic = c(
        "N",
        "Pearson",
        "Spearman"
      ),
      Value = c(
        nrow(REL),
        pearson,
        spearman
      )
    )

    write.csv(
      RV_NDAILY,
      file.path(
        OUT,
        "TABLEAU_14_RV_NDAILY.csv"
      ),
      row.names = FALSE
    )
  }

} else {

  cat("n_daily ABSENT.\n")
}

# ============================================================
# 5. RV.x / RV.y
# ============================================================

cat("\n============================================================\n")
cat(" COMPARAISON RV.x / RV.y\n")
cat("============================================================\n")

if ("RV.x" %in% names(df) &&
    "RV.y" %in% names(df)) {

  rvx <- suppressWarnings(
    as.numeric(df[["RV.x"]])
  )

  rvy <- suppressWarnings(
    as.numeric(df[["RV.y"]])
  )

  both <- is.finite(rvx) &
          is.finite(rvy)

  cat("RV.x valides :", sum(is.finite(rvx)), "\n")
  cat("RV.y valides :", sum(is.finite(rvy)), "\n")
  cat("Les deux valides :", sum(both), "\n")

  if (sum(both) > 0) {

    difference <- rvx[both] - rvy[both]

    cat(
      "Difference absolue moyenne :",
      mean(abs(difference)),
      "\n"
    )

    cat(
      "Correlation Pearson :",
      cor(
        rvx[both],
        rvy[both],
        method = "pearson"
      ),
      "\n"
    )

    cat(
      "Correlation Spearman :",
      cor(
        rvx[both],
        rvy[both],
        method = "spearman"
      ),
      "\n"
    )

    RV_COMPARISON <- data.frame(
      Statistic = c(
        "N_both_valid",
        "Mean_abs_difference",
        "Pearson",
        "Spearman"
      ),
      Value = c(
        sum(both),
        mean(abs(difference)),
        cor(rvx[both], rvy[both]),
        cor(rvx[both], rvy[both], method = "spearman")
      )
    )

    write.csv(
      RV_COMPARISON,
      file.path(
        OUT,
        "TABLEAU_17_COMPARAISON_RV_X_Y.csv"
      ),
      row.names = FALSE
    )
  }

} else {

  cat("RV.x et/ou RV.y absent.\n")

  cat(
    "RV.x present :",
    "RV.x" %in% names(df),
    "\n"
  )

  cat(
    "RV.y present :",
    "RV.y" %in% names(df),
    "\n"
  )
}

# ============================================================
# 6. DAILY MODEL DATA
# ============================================================

cat("\n============================================================\n")
cat(" AUDIT DAILY_MODEL_DATA\n")
cat("============================================================\n")

DAILY <- file.path(
  P,
  "data",
  "processed",
  "daily_model_data.csv"
)

if (file.exists(DAILY)) {

  daily <- read_csv(
    DAILY,
    show_col_types = FALSE
  )

  cat("Fichier trouve.\n")
  cat(
    "Dimensions :",
    nrow(daily),
    "x",
    ncol(daily),
    "\n\n"
  )

  cat("Colonnes :\n")
  print(names(daily))

  daily_date_col <- names(daily)[1]

  daily[[daily_date_col]] <-
    as.Date(daily[[daily_date_col]])

  if ("country" %in% names(daily)) {

    DAILY_COUNTRY <- daily %>%
      group_by(country) %>%
      summarise(
        N_daily = n(),
        Date_min = min(
          .data[[daily_date_col]],
          na.rm = TRUE
        ),
        Date_max = max(
          .data[[daily_date_col]],
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

  cat("daily_model_data.csv NON TROUVE.\n")
}

# ============================================================
# 7. RESUME FINAL RV
# ============================================================

cat("\n============================================================\n")
cat(" RESUME FINAL AUDIT RV\n")
cat("============================================================\n")

rv <- suppressWarnings(
  as.numeric(df[[rv_name]])
)

SUMMARY_RV <- data.frame(
  Controle = c(
    "N_total",
    "N_valid",
    "NA",
    "Inf",
    "Zeros",
    "Negatifs",
    "Positifs",
    "Min",
    "Median",
    "Mean",
    "Max",
    "SD",
    "Skewness",
    "Kurtosis"
  ),
  Valeur = c(
    length(rv),
    sum(is.finite(rv)),
    sum(is.na(rv)),
    sum(is.infinite(rv)),
    sum(rv == 0, na.rm = TRUE),
    sum(rv < 0, na.rm = TRUE),
    sum(rv > 0, na.rm = TRUE),
    min(rv, na.rm = TRUE),
    median(rv, na.rm = TRUE),
    mean(rv, na.rm = TRUE),
    max(rv, na.rm = TRUE),
    sd(rv, na.rm = TRUE),
    mean((rv - mean(rv, na.rm = TRUE))^3, na.rm = TRUE) /
      sd(rv, na.rm = TRUE)^3,
    mean((rv - mean(rv, na.rm = TRUE))^4, na.rm = TRUE) /
      sd(rv, na.rm = TRUE)^4
  )
)

write.csv(
  SUMMARY_RV,
  file.path(
    OUT,
    "TABLEAU_16_RESUME_FINAL_RV.csv"
  ),
  row.names = FALSE
)

print(SUMMARY_RV)

cat("\n============================================================\n")
cat(" AUDIT RV TERMINE\n")
cat("============================================================\n")

cat("\nFichiers produits :\n")

print(
  list.files(
    OUT,
    pattern = "TABLEAU_(11|12|13|14|15|16|17)"
  )
)

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
