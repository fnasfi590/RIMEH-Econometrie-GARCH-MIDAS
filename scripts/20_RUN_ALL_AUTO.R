# ============================================================
# RIMEH - GARCH-MIDAS : WORKFLOW AUTOMATIQUE COMPLET
# ============================================================

options(stringsAsFactors = FALSE)

project_dir <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"
setwd(project_dir)

cat("\n============================================================\n")
cat(" RIMEH - WORKFLOW AUTOMATIQUE GARCH-MIDAS\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. Vérification du projet
# ------------------------------------------------------------

dirs <- c("data", "scripts", "results", "results/tables", "results/analysis", "results/figures")

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("[1/8] Projet :", getwd(), "\n")

csv <- "results/tables/garch_midas_all_results.csv"

if (!file.exists(csv)) {
  stop(
    "\nERREUR : fichier introuvable : ", csv,
    "\nLes résultats GARCH-MIDAS doivent d'abord être générés.\n"
  )
}

cat("[OK] Résultats GARCH-MIDAS trouvés.\n\n")

# ------------------------------------------------------------
# 2. Lecture des résultats
# ------------------------------------------------------------

cat("[2/8] Lecture des résultats...\n")

res <- read.csv(csv, check.names = FALSE)

cat("[OK] ", nrow(res), " lignes chargées.\n")
cat("[OK] ", ncol(res), " colonnes.\n\n")

# ------------------------------------------------------------
# 3. Détection automatique des colonnes
# ------------------------------------------------------------

find_col <- function(patterns, nms) {
  for (p in patterns) {
    x <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(x) > 0) return(x[1])
  }
  NA_character_
}

nms <- names(res)

country_col <- find_col(
  c("^country$", "country", "pays"),
  nms
)

model_col <- find_col(
  c("^model$", "model", "spec"),
  nms
)

param_col <- find_col(
  c("^parameter$", "^param$", "parameter", "param"),
  nms
)

estimate_col <- find_col(
  c("^estimate$", "estimate", "coef", "coefficient"),
  nms
)

pvalue_col <- find_col(
  c("^p.value$", "^pvalue$", "p.value", "pvalue", "prob"),
  nms
)

cat("Colonnes détectées :\n")
cat(" country  =", country_col, "\n")
cat(" model    =", model_col, "\n")
cat(" parameter=", param_col, "\n")
cat(" estimate =", estimate_col, "\n")
cat(" p-value  =", pvalue_col, "\n\n")

# ------------------------------------------------------------
# 4. Normalisation
# ------------------------------------------------------------

if (!is.na(country_col)) res$COUNTRY <- as.character(res[[country_col]])
if (!is.na(model_col))   res$MODEL   <- as.character(res[[model_col]])
if (!is.na(param_col))   res$PARAM   <- as.character(res[[param_col]])

if (!is.na(estimate_col)) {
  res$ESTIMATE <- suppressWarnings(as.numeric(res[[estimate_col]]))
}

if (!is.na(pvalue_col)) {
  res$P_VALUE <- suppressWarnings(as.numeric(res[[pvalue_col]]))
}

# Vérification minimale
required <- c("COUNTRY", "MODEL", "PARAM", "ESTIMATE", "P_VALUE")

missing_required <- setdiff(required, names(res))

if (length(missing_required) > 0) {
  cat("\nColonnes disponibles :\n")
  print(names(res))
  stop(
    "\nImpossible de détecter automatiquement : ",
    paste(missing_required, collapse = ", ")
  )
}

res$SIGNIFICANT_5 <- !is.na(res$P_VALUE) & res$P_VALUE < 0.05
res$SIGNIFICANT_10 <- !is.na(res$P_VALUE) & res$P_VALUE < 0.10

res$SIGN <- ifelse(
  is.na(res$ESTIMATE),
  NA,
  ifelse(res$ESTIMATE > 0, "POSITIVE",
         ifelse(res$ESTIMATE < 0, "NEGATIVE", "ZERO"))
)

# ------------------------------------------------------------
# 5. Statistiques générales
# ------------------------------------------------------------

cat("[3/8] Statistiques générales...\n")

countries <- sort(unique(res$COUNTRY))
models <- sort(unique(res$MODEL))

cat("Pays :", length(countries), "\n")
cat("Modèles :", length(models), "\n")
cat("Coefficients :", nrow(res), "\n\n")

# ------------------------------------------------------------
# 6. Extraction des theta
# ------------------------------------------------------------

cat("[4/8] Analyse des coefficients theta...\n")

theta <- res[
  grepl("theta", res$PARAM, ignore.case = TRUE),
]

if (nrow(theta) > 0) {

  theta$VARIABLE <- NA_character_

  theta$VARIABLE[
    grepl("SUP", theta$MODEL, ignore.case = TRUE)
  ] <- "SUPPLY"

  theta$VARIABLE[
    grepl("DEM", theta$MODEL, ignore.case = TRUE)
  ] <- "DEMAND"

  theta$VARIABLE[
    grepl("PRE", theta$MODEL, ignore.case = TRUE)
  ] <- "PRICE"

  theta$TYPE <- NA_character_

  theta$TYPE[
    grepl("GPRT", theta$MODEL, ignore.case = TRUE)
  ] <- "GPRT"

  theta$TYPE[
    grepl("GPRA", theta$MODEL, ignore.case = TRUE)
  ] <- "GPRA"

  theta$TYPE[
    grepl("^SUP", theta$MODEL, ignore.case = TRUE)
  ] <- "SUP"

  # Sauvegarde détaillée
  write.csv(
    theta,
    "results/analysis/H1_H9_THETA_DETAIL.csv",
    row.names = FALSE
  )

  cat("[OK] Theta :", nrow(theta), "\n")
} else {
  cat("[ATTENTION] Aucun theta détecté.\n")
}

# ------------------------------------------------------------
# 7. Construction du tableau H1-H9
# ------------------------------------------------------------

cat("[5/8] Construction du tableau H1-H9...\n")

safe_summary <- function(data, hypothesis, expected_sign = NA) {

  if (nrow(data) == 0) {
    return(data.frame(
      Hypothesis = hypothesis,
      N = 0,
      Significant_5pct = 0,
      Positive = 0,
      Negative = 0,
      Significant_Positive = 0,
      Significant_Negative = 0,
      Mean_Estimate = NA,
      Mean_PValue = NA,
      Conclusion = "NOT TESTABLE"
    ))
  }

  sig <- !is.na(data$P_VALUE) & data$P_VALUE < 0.05
  pos <- data$ESTIMATE > 0
  neg <- data$ESTIMATE < 0

  sig_pos <- sig & pos
  sig_neg <- sig & neg

  conclusion <- "DESCRIPTIVE / NON CONCLUSIVE"

  if (!is.na(expected_sign)) {

    if (expected_sign == "positive" && sum(sig_pos) > 0)
      conclusion <- "EVIDENCE POSITIVE"

    if (expected_sign == "negative" && sum(sig_neg) > 0)
      conclusion <- "EVIDENCE NEGATIVE"
  }

  data.frame(
    Hypothesis = hypothesis,
    N = nrow(data),
    Significant_5pct = sum(sig),
    Positive = sum(pos, na.rm = TRUE),
    Negative = sum(neg, na.rm = TRUE),
    Significant_Positive = sum(sig_pos, na.rm = TRUE),
    Significant_Negative = sum(sig_neg, na.rm = TRUE),
    Mean_Estimate = mean(data$ESTIMATE, na.rm = TRUE),
    Mean_PValue = mean(data$P_VALUE, na.rm = TRUE),
    Conclusion = conclusion
  )
}

# H1 : effet positif du GPR sur la volatilité
H1 <- theta[
  grepl("GPR", theta$MODEL, ignore.case = TRUE) &
  grepl("PRE", theta$MODEL, ignore.case = TRUE),
]

# H2 : demande
H2 <- theta[
  grepl("DEM", theta$MODEL, ignore.case = TRUE),
]

# H3 : offre
H3 <- theta[
  grepl("SUP", theta$MODEL, ignore.case = TRUE),
]

# H4 : différences exportateurs/importateurs
H4 <- theta

# H5 : demande selon horizon
H5 <- H2

# H6 : offre selon horizon
H6 <- H3

# H7 : GPRT vs GPRA sur prix/volatilité
H7 <- theta[
  grepl("GPRT|GPRA", theta$MODEL, ignore.case = TRUE) &
  grepl("PRE", theta$MODEL, ignore.case = TRUE),
]

# H8 : GPRT vs GPRA sur demande
H8 <- theta[
  grepl("GPRT|GPRA", theta$MODEL, ignore.case = TRUE) &
  grepl("DEM", theta$MODEL, ignore.case = TRUE),
]

# H9 : GPRT vs GPRA sur offre
H9 <- theta[
  grepl("GPRT|GPRA", theta$MODEL, ignore.case = TRUE) &
  grepl("SUP", theta$MODEL, ignore.case = TRUE),
]

final <- rbind(
  safe_summary(H1, "H1", "positive"),
  safe_summary(H2, "H2", "positive"),
  safe_summary(H3, "H3", "negative"),
  safe_summary(H4, "H4"),
  safe_summary(H5, "H5"),
  safe_summary(H6, "H6"),
  safe_summary(H7, "H7"),
  safe_summary(H8, "H8"),
  safe_summary(H9, "H9")
)

write.csv(
  final,
  "results/analysis/H1_H9_FINAL.csv",
  row.names = FALSE
)

cat("[OK] H1-H9 :", nrow(final), "hypothèses.\n\n")

# ------------------------------------------------------------
# 8. Rapport automatique
# ------------------------------------------------------------

cat("[6/8] Création du rapport texte...\n")

report_file <- "results/analysis/H1_H9_AUTO_REPORT.txt"

sink(report_file)

cat("============================================================\n")
cat("RIMEH - RAPPORT AUTOMATIQUE H1-H9\n")
cat("============================================================\n\n")

cat("Date :", as.character(Sys.time()), "\n\n")

cat("DONNEES\n")
cat("-------\n")
cat("Nombre de coefficients :", nrow(res), "\n")
cat("Nombre de pays :", length(countries), "\n")
cat("Nombre de modèles :", length(models), "\n\n")

cat("PAYS\n")
print(countries)

cat("\nMODELES\n")
print(models)

cat("\nRESULTATS H1-H9\n")
print(final)

cat("\n\nIMPORTANT\n")
cat("H1-H9 sont présentées ici comme une analyse automatique.\n")
cat("Les formulations théoriques originales des neuf hypothèses\n")
cat("ne doivent pas être considérées comme vérifiées mot pour mot\n")
cat("sans le document source contenant leur formulation.\n")

sink()

cat("[OK] Rapport :", report_file, "\n\n")

# ------------------------------------------------------------
# 9. Audit des données
# ------------------------------------------------------------

cat("[7/8] Audit final...\n")

audit <- data.frame(
  Metric = c(
    "Rows",
    "Countries",
    "Models",
    "Theta rows",
    "Significant 5pct",
    "Significant 10pct",
    "Positive",
    "Negative"
  ),
  Value = c(
    nrow(res),
    length(countries),
    length(models),
    nrow(theta),
    sum(res$SIGNIFICANT_5, na.rm = TRUE),
    sum(res$SIGNIFICANT_10, na.rm = TRUE),
    sum(res$SIGN == "POSITIVE", na.rm = TRUE),
    sum(res$SIGN == "NEGATIVE", na.rm = TRUE)
  )
)

write.csv(
  audit,
  "results/analysis/AUTO_AUDIT.csv",
  row.names = FALSE
)

print(audit)

# ------------------------------------------------------------
# 10. Résumé final
# ------------------------------------------------------------

cat("\n[8/8] TERMINE\n")
cat("============================================================\n")
cat("FICHIERS CREES :\n")
cat(" - results/analysis/H1_H9_FINAL.csv\n")
cat(" - results/analysis/H1_H9_THETA_DETAIL.csv\n")
cat(" - results/analysis/H1_H9_AUTO_REPORT.txt\n")
cat(" - results/analysis/AUTO_AUDIT.csv\n")
cat("============================================================\n")

cat("\nATTENTION :\n")
cat("Ce script analyse les résultats existants.\n")
cat("Il ne ré-estime pas automatiquement le modèle GBR/SUP_PRE.\n")
cat("Ce modèle doit être traité séparément si nécessaire.\n\n")

cat("FIN DU WORKFLOW.\n")

