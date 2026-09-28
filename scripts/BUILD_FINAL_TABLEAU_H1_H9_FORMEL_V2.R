base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

dir.create(
  file.path(base, "results", "analysis"),
  recursive = TRUE,
  showWarnings = FALSE
)

num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

decision <- function(p) {
  ifelse(
    is.na(p),
    "Non testable",
    ifelse(p < 0.05, "Rejet de H0", "Non-rejet de H0")
  )
}

empty_row <- function(h, param, h0, h1, test, justification) {
  data.frame(
    Hypothese = h,
    Parametre = param,
    H0 = h0,
    H1 = h1,
    Test = test,
    Estimation = NA_real_,
    SE = NA_real_,
    Statistique = NA_real_,
    P_value = NA_real_,
    Decision_5pct = "Non testable",
    Statut = "Non testable",
    Justification = justification,
    stringsAsFactors = FALSE
  )
}

# ============================================================
# H1
# ============================================================

f1 <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv"
)

d1 <- read.csv(
  f1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

H1_data <- d1[d1$model == "GPRT_VIX", ]

H1 <- data.frame(
  Hypothese = "H1",
  Parametre = H1_data$parameter,
  H0 = "Paramètre = 0",
  H1 = "Paramètre != 0",
  Test = "Test individuel du paramètre (GARCH-MIDAS)",
  Estimation = num(H1_data$Estimate),
  SE = num(H1_data$Std_Error),
  Statistique = num(H1_data$t_value),
  P_value = num(H1_data$p_value),
  Decision_5pct = decision(num(H1_data$p_value)),
  Statut = "Testable",
  Justification = paste("Pays", H1_data$country, "- modèle GPRT_VIX"),
  stringsAsFactors = FALSE
)

# ============================================================
# H2
# ============================================================

H2_data <- d1[d1$model %in% c("GPRT_DEM", "SUP_DEM"), ]

H2 <- data.frame(
  Hypothese = "H2",
  Parametre = H2_data$parameter,
  H0 = "Paramètre = 0",
  H1 = "Paramètre != 0",
  Test = "Test individuel du paramètre (GARCH-MIDAS)",
  Estimation = num(H2_data$Estimate),
  SE = num(H2_data$Std_Error),
  Statistique = num(H2_data$t_value),
  P_value = num(H2_data$p_value),
  Decision_5pct = decision(num(H2_data$p_value)),
  Statut = "Testable",
  Justification = paste("Pays", H2_data$country, "- modèle", H2_data$model),
  stringsAsFactors = FALSE
)

# ============================================================
# H3
# ============================================================

H3_data <- d1[d1$model %in% c("GPRA_SUP", "GPRT_SUP"), ]

H3 <- data.frame(
  Hypothese = "H3",
  Parametre = H3_data$parameter,
  H0 = "Paramètre = 0",
  H1 = "Paramètre != 0",
  Test = "Test individuel du paramètre (GARCH-MIDAS)",
  Estimation = num(H3_data$Estimate),
  SE = num(H3_data$Std_Error),
  Statistique = num(H3_data$t_value),
  P_value = num(H3_data$p_value),
  Decision_5pct = decision(num(H3_data$p_value)),
  Statut = "Testable",
  Justification = paste("Pays", H3_data$country, "- modèle", H3_data$model),
  stringsAsFactors = FALSE
)

# ============================================================
# H4
# ============================================================

H4 <- empty_row(
  "H4",
  "Différence exportateurs / importateurs",
  "Pas de différence entre exportateurs et importateurs",
  "Différence entre exportateurs et importateurs",
  "Test de différence exportateurs/importateurs",
  "Classification exportateur/importateur absente des données fournies"
)

# ============================================================
# H5
# ============================================================

f5 <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_FORMAL_H1_H9",
  "FINAL_H5_CORRECTED.csv"
)

H5_data <- read.csv(
  f5,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

H5 <- data.frame(
  Hypothese = H5_data$Hypothese,
  Parametre = H5_data$Parametre,
  H0 = H5_data$H0,
  H1 = H5_data$H1,
  Test = H5_data$Test,
  Estimation = num(H5_data$Estimation),
  SE = num(H5_data$SE),
  Statistique = num(H5_data$Statistique),
  P_value = num(H5_data$P_value),
  Decision_5pct = H5_data$Decision_5pct,
  Statut = "Testable",
  Justification = paste("Pays", H5_data$Pays, "- modèle", H5_data$Modele),
  stringsAsFactors = FALSE
)

# ============================================================
# H6
# ============================================================

f6 <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_FORMAL_H1_H9",
  "FINAL_H6_CORRECTED.csv"
)

H6_data <- read.csv(
  f6,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

H6 <- data.frame(
  Hypothese = H6_data$Hypothese,
  Parametre = H6_data$Parametre,
  H0 = H6_data$H0,
  H1 = H6_data$H1,
  Test = H6_data$Test,
  Estimation = num(H6_data$Estimation),
  SE = num(H6_data$SE),
  Statistique = num(H6_data$Statistique),
  P_value = num(H6_data$P_value),
  Decision_5pct = H6_data$Decision_5pct,
  Statut = "Testable",
  Justification = paste("Pays", H6_data$Pays, "- modèle", H6_data$Modele),
  stringsAsFactors = FALSE
)

# ============================================================
# H7
# ============================================================

H7 <- empty_row(
  "H7",
  "GPRT vs GPRA - PRICE",
  "Effet GPRT = effet GPRA",
  "Effet GPRT != effet GPRA",
  "Comparaison GPRT vs GPRA",
  "GPRA_PRE absent : aucune paire GPRT/GPRA compatible pour PRICE"
)

# ============================================================
# H8
# ============================================================

H8 <- empty_row(
  "H8",
  "GPRT vs GPRA - DEMAND",
  "Effet GPRT = effet GPRA",
  "Effet GPRT != effet GPRA",
  "Comparaison GPRT vs GPRA",
  "GPRA_DEM absent : aucune paire GPRT/GPRA compatible pour DEMAND"
)

# ============================================================
# H9
# ============================================================

f9 <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_FORMAL_H1_H9",
  "FINAL_H9_GPRT_GPRA_DETAIL.csv"
)

H9_data <- read.csv(
  f9,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

H9 <- data.frame(
  Hypothese = "H9",
  Parametre = H9_data$parameter,
  H0 = "Effet GPRT = effet GPRA",
  H1 = "Effet GPRT != effet GPRA",
  Test = "Test formel approximatif GPRT vs GPRA",
  Estimation = num(H9_data$diff),
  SE = num(H9_data$se_diff),
  Statistique = num(H9_data$z),
  P_value = num(H9_data$p),
  Decision_5pct = decision(num(H9_data$p)),
  Statut = "Testable approximativement",
  Justification = paste(
    "Pays", H9_data$country,
    "- covariance intra-estimation non disponible"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# ASSEMBLAGE
# ============================================================

FINAL <- rbind(
  H1, H2, H3, H4, H5, H6, H7, H8, H9
)

ordre <- c("H1","H2","H3","H4","H5","H6","H7","H8","H9")

FINAL$Hypothese <- factor(
  FINAL$Hypothese,
  levels = ordre
)

FINAL <- FINAL[order(FINAL$Hypothese), ]

FINAL$Hypothese <- as.character(FINAL$Hypothese)

rownames(FINAL) <- NULL

# ============================================================
# CONTROLE
# ============================================================

cat("\n==============================================\n")
cat("CONTROLE FINAL H1-H9\n")
cat("==============================================\n")

for (h in ordre) {
  cat(
    h, ":",
    sum(FINAL$Hypothese == h),
    "lignes\n"
  )
}

cat("----------------------------------------------\n")
cat("TOTAL :", nrow(FINAL), "lignes\n")
cat("==============================================\n")

cat("\nP-values disponibles :\n")

for (h in ordre) {
  d <- FINAL[FINAL$Hypothese == h, ]
  cat(
    h, ":",
    sum(!is.na(d$P_value)),
    "/",
    nrow(d),
    "\n"
  )
}

cat("\nRejets de H0 à 5% :\n")

for (h in ordre) {
  d <- FINAL[FINAL$Hypothese == h, ]
  cat(
    h, ":",
    sum(d$Decision_5pct == "Rejet de H0", na.rm = TRUE),
    "\n"
  )
}

# ============================================================
# EXPORTS
# ============================================================

out_csv <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_TABLEAU_H1_H9_FORMEL_V2.csv"
)

out_txt <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_TABLEAU_H1_H9_FORMEL_V2.txt"
)

out_xlsx <- file.path(
  base,
  "results",
  "analysis",
  "FINAL_TABLEAU_H1_H9_FORMEL_V2.xlsx"
)

write.csv(
  FINAL,
  out_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

capture.output(
  print(FINAL, row.names = FALSE),
  file = out_txt
)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages(
    "openxlsx",
    repos = "https://cloud.r-project.org"
  )
}

openxlsx::write.xlsx(
  FINAL,
  out_xlsx,
  overwrite = TRUE
)

cat("\n==============================================\n")
cat("FICHIERS CREES\n")
cat("==============================================\n")
cat("CSV  :", out_csv, "\n")
cat("TXT  :", out_txt, "\n")
cat("XLSX :", out_xlsx, "\n")
cat("\nTABLEAU FINAL H1-H9 V2 TERMINE.\n")
