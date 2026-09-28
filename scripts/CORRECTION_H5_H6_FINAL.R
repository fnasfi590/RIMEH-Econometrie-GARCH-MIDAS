# ============================================================
# CORRECTION FINALE H5 / H6
# H5 = DEMANDE SELON HORIZON
# H6 = OFFRE SELON HORIZON
# ============================================================

base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

source_file <- file.path(
  base,
  "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv"
)

out_dir <- file.path(
  base,
  "results/analysis/FINAL_FORMAL_H1_H9"
)

dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

# ------------------------------------------------------------
# Lecture
# ------------------------------------------------------------

x <- read.csv(
  source_file,
  stringsAsFactors=FALSE,
  check.names=FALSE
)

cat("\n============================================================\n")
cat("CORRECTION H5 / H6\n")
cat("============================================================\n")

cat("Observations source :", nrow(x), "\n")
cat("Modeles source :", paste(unique(x$model), collapse=", "), "\n")

# ------------------------------------------------------------
# H5 = DEMANDE SELON HORIZON
# ------------------------------------------------------------

h5_models <- c(
  "GPRT_DEM",
  "SUP_DEM"
)

h5x <- x[x$model %in% h5_models, ]

h5 <- data.frame(
  Hypothese="H5",
  Modele=h5x$model,
  Pays=h5x$country,
  Parametre="theta_1 - theta_2",
  H0="theta_1 - theta_2 = 0",
  H1="theta_1 - theta_2 != 0",
  Test="Test de difference sous hypothese d'independance",
  Estimation=h5x$diff,
  SE=h5x$se_ind,
  Statistique=h5x$z_ind,
  P_value=h5x$p_ind,
  Decision_5pct=ifelse(
    h5x$p_ind < 0.05,
    "Rejet de H0",
    "Non-rejet de H0"
  ),
  stringsAsFactors=FALSE
)

# ------------------------------------------------------------
# H6 = OFFRE SELON HORIZON
# ------------------------------------------------------------

h6_models <- c(
  "GPRA_SUP",
  "GPRT_SUP"
)

h6x <- x[x$model %in% h6_models, ]

h6 <- data.frame(
  Hypothese="H6",
  Modele=h6x$model,
  Pays=h6x$country,
  Parametre="theta_1 - theta_2",
  H0="theta_1 - theta_2 = 0",
  H1="theta_1 - theta_2 != 0",
  Test="Test de difference sous hypothese d'independance",
  Estimation=h6x$diff,
  SE=h6x$se_ind,
  Statistique=h6x$z_ind,
  P_value=h6x$p_ind,
  Decision_5pct=ifelse(
    h6x$p_ind < 0.05,
    "Rejet de H0",
    "Non-rejet de H0"
  ),
  stringsAsFactors=FALSE
)

# ------------------------------------------------------------
# Sauvegarde
# ------------------------------------------------------------

write.csv(
  h5,
  file.path(out_dir, "FINAL_H5_CORRECTED.csv"),
  row.names=FALSE
)

write.csv(
  h6,
  file.path(out_dir, "FINAL_H6_CORRECTED.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# Résumé
# ------------------------------------------------------------

cat("\n---------------- H5 ----------------\n")
cat("Modeles :", paste(unique(h5$Modele), collapse=", "), "\n")
cat("N =", nrow(h5), "\n")
cat("Rejets H0 =", sum(h5$P_value < 0.05, na.rm=TRUE), "\n")
cat("Non-rejets =", sum(h5$P_value >= 0.05, na.rm=TRUE), "\n")

if(nrow(h5) > 0) {
  cat("p-min =", min(h5$P_value, na.rm=TRUE), "\n")
  cat("p-mediane =", median(h5$P_value, na.rm=TRUE), "\n")
}

cat("\n---------------- H6 ----------------\n")
cat("Modeles :", paste(unique(h6$Modele), collapse=", "), "\n")
cat("N =", nrow(h6), "\n")
cat("Rejets H0 =", sum(h6$P_value < 0.05, na.rm=TRUE), "\n")
cat("Non-rejets =", sum(h6$P_value >= 0.05, na.rm=TRUE), "\n")

if(nrow(h6) > 0) {
  cat("p-min =", min(h6$P_value, na.rm=TRUE), "\n")
  cat("p-mediane =", median(h6$P_value, na.rm=TRUE), "\n")
}

cat("\nFichiers produits :\n")
cat("- FINAL_H5_CORRECTED.csv\n")
cat("- FINAL_H6_CORRECTED.csv\n")

cat("\n============================================================\n")
cat("CORRECTION TERMINEE\n")
cat("============================================================\n")
