# ============================================================
# RIMEH - PIPELINE FINAL D'ASSEMBLAGE
# ============================================================
# Ne ré-estime PAS les modèles.
# Utilise les 76 RDS déjà estimés + résultats RIMEH_FINAL
# + tableau H1-H9 FORMEL V2.
# ============================================================

options(stringsAsFactors = FALSE)
options(warn = 1)

ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)

DIR_FINAL <- file.path(ROOT, "results", "analysis", "RIMEH_FINAL")
DIR_RDS   <- file.path(ROOT, "results", "models", "garch_midas")
DIR_H     <- file.path(ROOT, "results", "analysis")
DIR_TAB   <- file.path(ROOT, "results", "tables")
DIR_OUT   <- file.path(ROOT, "results", "analysis", "RIMEH_FINAL")

dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat(" RIMEH - PIPELINE FINAL\n")
cat("============================================================\n")
cat("ROOT :", ROOT, "\n")
cat("Date :", format(Sys.time()), "\n\n")

# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

pkgs <- c(
  "readr",
  "dplyr",
  "tidyr",
  "openxlsx",
  "tseries"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat("Installation du package :", p, "\n")
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(readr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tseries)

# ------------------------------------------------------------
# FONCTION LECTURE CSV ROBUSTE
# ------------------------------------------------------------

read_csv_safe <- function(path) {

  if (!file.exists(path)) {
    stop("Fichier absent : ", path)
  }

  x <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  x
}

# ------------------------------------------------------------
# 1. RESULTATS EXISTANTS
# ------------------------------------------------------------

files_final <- c(
  "01_STATISTIQUES_DESCRIPTIVES.csv",
  "02_CORRELATION_PEARSON.csv",
  "02B_CORRELATION_SPEARMAN.csv",
  "03_STATIONNARITE_ADF_PP_KPSS.csv",
  "04_VIF.csv",
  "05_TESTS_PRE_ESTIMATION.csv",
  "06_MODELES_76.csv",
  "07_COEFFICIENTS_76_MODELES.csv"
)

data_final <- list()

for (f in files_final) {

  p <- file.path(DIR_FINAL, f)

  cat("Lecture :", f, "\n")

  data_final[[f]] <- read_csv_safe(p)
}

# ------------------------------------------------------------
# 2. H1-H9 FORMEL V2
# ------------------------------------------------------------

H1H9_PATH <- file.path(
  DIR_H,
  "FINAL_TABLEAU_H1_H9_FORMEL_V2.csv"
)

H1H9 <- read_csv_safe(H1H9_PATH)

cat("\nH1-H9 :", nrow(H1H9), "lignes\n")

if (nrow(H1H9) != 165) {
  warning(
    "Le tableau H1-H9 ne contient pas 165 lignes : ",
    nrow(H1H9)
  )
}

# ------------------------------------------------------------
# 3. AUDIT H1-H9
# ------------------------------------------------------------

AUDIT_PATH <- file.path(
  DIR_TAB,
  "H1_H9_AUDIT_FINAL.csv"
)

AUDIT_H1H9 <- NULL

if (file.exists(AUDIT_PATH)) {

  AUDIT_H1H9 <- read_csv_safe(AUDIT_PATH)

  cat(
    "Audit H1-H9 :",
    nrow(AUDIT_H1H9),
    "lignes\n"
  )
}

# ------------------------------------------------------------
# 4. MAPPING H1-H9
# ------------------------------------------------------------

MAP_PATH <- file.path(
  DIR_H,
  "FINAL_H1_H9_MAPPING_EVIDENCE.csv"
)

MAPPING_H1H9 <- NULL

if (file.exists(MAP_PATH)) {

  MAPPING_H1H9 <- read_csv_safe(MAP_PATH)

  cat(
    "Mapping H1-H9 :",
    nrow(MAPPING_H1H9),
    "lignes\n"
  )
}

# ------------------------------------------------------------
# 5. VERIFICATION DES 76 MODELES
# ------------------------------------------------------------

MODELS <- data_final[["06_MODELES_76.csv"]]

cat("\nNombre de modèles :", nrow(MODELS), "\n")

if (nrow(MODELS) != 76) {
  warning("Nombre de modèles différent de 76.")
}

# ------------------------------------------------------------
# 6. VERIFICATION DES 572 COEFFICIENTS
# ------------------------------------------------------------

COEFS <- data_final[["07_COEFFICIENTS_76_MODELES.csv"]]

cat(
  "Nombre de coefficients :",
  nrow(COEFS),
  "\n"
)

# ------------------------------------------------------------
# 7. INVENTAIRE DES RDS
# ------------------------------------------------------------

RDS_FILES <- list.files(
  DIR_RDS,
  pattern = "\\.rds$",
  full.names = TRUE
)

cat(
  "RDS trouvés :",
  length(RDS_FILES),
  "\n"
)

if (length(RDS_FILES) != 76) {
  warning(
    "Le nombre de RDS est différent de 76 : ",
    length(RDS_FILES)
  )
}

# ------------------------------------------------------------
# 8. EXTRACTION DES INFORMATIONS RDS
# ------------------------------------------------------------

rds_inventory <- list()

for (f in RDS_FILES) {

  obj <- tryCatch(
    readRDS(f),
    error = function(e) NULL
  )

  model_name <- tools::file_path_sans_ext(
    basename(f)
  )

  if (is.null(obj)) {

    rds_inventory[[length(rds_inventory) + 1]] <- data.frame(
      Model = model_name,
      Class = NA_character_,
      Obs = NA_real_,
      LogLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      HasVolatility = FALSE,
      HasLongRun = FALSE,
      stringsAsFactors = FALSE
    )

    next
  }

  ic <- obj$inf_criteria

  AIC_value <- NA_real_
  BIC_value <- NA_real_

  if (!is.null(ic)) {

    if ("AIC" %in% names(ic))
      AIC_value <- as.numeric(ic["AIC"])

    if ("BIC" %in% names(ic))
      BIC_value <- as.numeric(ic["BIC"])
  }

  rds_inventory[[length(rds_inventory) + 1]] <- data.frame(
    Model = model_name,
    Class = paste(class(obj), collapse = ";"),
    Obs = if (!is.null(obj$obs)) as.numeric(obj$obs) else NA_real_,
    LogLik = if (!is.null(obj$loglik)) as.numeric(obj$loglik) else NA_real_,
    AIC = AIC_value,
    BIC = BIC_value,
    HasVolatility = !is.null(obj$est_vol_in_s),
    HasLongRun = !is.null(obj$est_lr_in_s),
    stringsAsFactors = FALSE
  )
}

RDS_INVENTORY <- bind_rows(rds_inventory)

# ------------------------------------------------------------
# 9. VALIDATION DES 76 MODELES
# ------------------------------------------------------------

VALIDATION_76 <- data.frame(
  Test = c(
    "Nombre RDS",
    "Nombre modèles tableau",
    "Nombre coefficients",
    "RDS avec volatilité",
    "RDS avec long-run",
    "RDS avec LogLik",
    "RDS avec AIC",
    "RDS avec BIC"
  ),
  Valeur = c(
    nrow(RDS_INVENTORY),
    nrow(MODELS),
    nrow(COEFS),
    sum(RDS_INVENTORY$HasVolatility, na.rm = TRUE),
    sum(RDS_INVENTORY$HasLongRun, na.rm = TRUE),
    sum(is.finite(RDS_INVENTORY$LogLik)),
    sum(is.finite(RDS_INVENTORY$AIC)),
    sum(is.finite(RDS_INVENTORY$BIC))
  )
)

write.csv(
  VALIDATION_76,
  file.path(
    DIR_OUT,
    "08_VALIDATION_76_MODELES.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 10. SYNTHESE DES COEFFICIENTS
# ------------------------------------------------------------

COEF_SUMMARY <- COEFS %>%
  mutate(
    Estimate = as.numeric(Estimate),
    `Std. Error` = as.numeric(`Std. Error`),
    `t value` = as.numeric(`t value`),
    `Pr(>|t|)` = as.numeric(`Pr(>|t|)`)
  ) %>%
  group_by(Parameter) %>%
  summarise(
    N = sum(is.finite(Estimate)),
    Min = min(Estimate, na.rm = TRUE),
    Median = median(Estimate, na.rm = TRUE),
    Mean = mean(Estimate, na.rm = TRUE),
    Max = max(Estimate, na.rm = TRUE),
    SD = sd(Estimate, na.rm = TRUE),
    Sig_5pct = sum(
      is.finite(`Pr(>|t|)`) &
      `Pr(>|t|)` < 0.05
    ),
    Sig_1pct = sum(
      is.finite(`Pr(>|t|)`) &
      `Pr(>|t|)` < 0.01
    ),
    .groups = "drop"
  )

write.csv(
  COEF_SUMMARY,
  file.path(
    DIR_OUT,
    "09_SYNTHESE_COEFFICIENTS.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 11. MODELES PAR PAYS / SPECIFICATION
# ------------------------------------------------------------

MODEL_COUNTS <- MODELS %>%
  count(Country, Specification, name = "N_models")

write.csv(
  MODEL_COUNTS,
  file.path(
    DIR_OUT,
    "10_CONTROLE_MODELES_PAR_PAYS.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 12. DIAGNOSTICS RDS DISPONIBLES
# ------------------------------------------------------------

DIAG_RDS <- RDS_INVENTORY %>%
  mutate(
    Volatility = ifelse(
      HasVolatility,
      "Disponible",
      "Absent"
    ),
    LongRun = ifelse(
      HasLongRun,
      "Disponible",
      "Absent"
    )
  )

write.csv(
  DIAG_RDS,
  file.path(
    DIR_OUT,
    "11_DIAGNOSTICS_DISPONIBILITE_RDS.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 13. INTEGRATION H1-H9
# ------------------------------------------------------------

H1H9_SYNTHESIS <- H1H9

if (!is.null(AUDIT_H1H9)) {

  names(AUDIT_H1H9) <-
    make.unique(names(AUDIT_H1H9))

  # Le tableau H1-H9 reste la source principale.
  # L'audit est conservé séparément.
}

write.csv(
  H1H9_SYNTHESIS,
  file.path(
    DIR_OUT,
    "12_H1_H9_FORMEL_V2_INTEGRE.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 14. CONTROLES DE COHERENCE
# ------------------------------------------------------------

CONTROL <- data.frame(
  Element = c(
    "Statistiques descriptives",
    "Pearson",
    "Spearman",
    "Stationnarite",
    "VIF",
    "Tests pre-estimation",
    "Modeles GARCH-MIDAS",
    "Coefficients",
    "RDS",
    "H1-H9"
  ),
  Lignes = c(
    nrow(data_final[["01_STATISTIQUES_DESCRIPTIVES.csv"]]),
    nrow(data_final[["02_CORRELATION_PEARSON.csv"]]),
    nrow(data_final[["02B_CORRELATION_SPEARMAN.csv"]]),
    nrow(data_final[["03_STATIONNARITE_ADF_PP_KPSS.csv"]]),
    nrow(data_final[["04_VIF.csv"]]),
    nrow(data_final[["05_TESTS_PRE_ESTIMATION.csv"]]),
    nrow(MODELS),
    nrow(COEFS),
    nrow(RDS_INVENTORY),
    nrow(H1H9)
  ),
  Statut = c(
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    ifelse(nrow(MODELS) == 76, "OK", "A_VERIFIER"),
    ifelse(nrow(COEFS) > 0, "OK", "A_VERIFIER"),
    ifelse(nrow(RDS_INVENTORY) == 76, "OK", "A_VERIFIER"),
    ifelse(nrow(H1H9) == 165, "OK", "A_VERIFIER")
  )
)

write.csv(
  CONTROL,
  file.path(
    DIR_OUT,
    "13_CONTROLE_FINAL.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 15. CREATION DU CLASSEUR EXCEL FINAL
# ------------------------------------------------------------

wb <- createWorkbook()

add_sheet <- function(
  wb,
  sheet_name,
  data
) {

  addWorksheet(wb, sheet_name)

  if (is.null(data)) {
    writeData(
      wb,
      sheet_name,
      data.frame(
        Information = "Aucune donnée disponible"
      )
    )
    return()
  }

  writeData(
    wb,
    sheet_name,
    data,
    withFilter = TRUE
  )

  freezePane(
    wb,
    sheet_name,
    firstRow = TRUE
  )

  setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}

add_sheet(
  wb,
  "01_Descriptives",
  data_final[["01_STATISTIQUES_DESCRIPTIVES.csv"]]
)

add_sheet(
  wb,
  "02_Pearson",
  data_final[["02_CORRELATION_PEARSON.csv"]]
)

add_sheet(
  wb,
  "03_Spearman",
  data_final[["02B_CORRELATION_SPEARMAN.csv"]]
)

add_sheet(
  wb,
  "04_Stationnarite",
  data_final[["03_STATIONNARITE_ADF_PP_KPSS.csv"]]
)

add_sheet(
  wb,
  "05_VIF",
  data_final[["04_VIF.csv"]]
)

add_sheet(
  wb,
  "06_Tests_Pre",
  data_final[["05_TESTS_PRE_ESTIMATION.csv"]]
)

add_sheet(
  wb,
  "07_Modeles_76",
  MODELS
)

add_sheet(
  wb,
  "08_Coefficients_76",
  COEFS
)

add_sheet(
  wb,
  "09_Coeff_Synthese",
  COEF_SUMMARY
)

add_sheet(
  wb,
  "10_Validation_76",
  VALIDATION_76
)

add_sheet(
  wb,
  "11_RDS_Diagnostics",
  DIAG_RDS
)

add_sheet(
  wb,
  "12_H1_H9_165",
  H1H9
)

add_sheet(
  wb,
  "13_H1_H9_Audit",
  AUDIT_H1H9
)

add_sheet(
  wb,
  "14_H1_H9_Mapping",
  MAPPING_H1H9
)

add_sheet(
  wb,
  "15_Controle_Final",
  CONTROL
)

FINAL_XLSX <- file.path(
  DIR_OUT,
  "RIMEH_RESULTATS_FINAL_COMPLET.xlsx"
)

saveWorkbook(
  wb,
  FINAL_XLSX,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 16. RAPPORT TEXTE
# ------------------------------------------------------------

REPORT <- file.path(
  DIR_OUT,
  "RIMEH_RESULTATS_FINAL_REPORT.txt"
)

sink(REPORT)

cat("============================================================\n")
cat(" RIMEH - RAPPORT FINAL D'ASSEMBLAGE\n")
cat("============================================================\n\n")

cat("Date :", format(Sys.time()), "\n\n")

cat("1. MODELES\n")
cat("Nombre de modèles :", nrow(MODELS), "\n")
cat("Nombre de RDS :", nrow(RDS_INVENTORY), "\n")
cat("Nombre de coefficients :", nrow(COEFS), "\n\n")

cat("2. H1-H9\n")
cat("Nombre de lignes :", nrow(H1H9), "\n")

if (!is.null(AUDIT_H1H9)) {

  cat("\nStatuts de l'audit H1-H9 :\n")

  print(
    table(
      AUDIT_H1H9$Statut,
      useNA = "ifany"
    )
  )
}

cat("\n3. CONTROLE FINAL\n\n")
print(CONTROL)

cat("\n4. VALIDATION RDS\n\n")
print(VALIDATION_76)

cat("\n============================================================\n")
cat(" FIN DU PIPELINE FINAL\n")
cat("============================================================\n")

sink()

cat("\n============================================================\n")
cat(" PIPELINE FINAL TERMINE\n")
cat("============================================================\n")
cat("Excel :", FINAL_XLSX, "\n")
cat("Rapport :", REPORT, "\n")
cat("============================================================\n")