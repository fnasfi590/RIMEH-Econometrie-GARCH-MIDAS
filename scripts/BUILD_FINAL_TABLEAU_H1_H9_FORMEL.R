# ============================================================
# FINAL TABLEAU H1-H9 - RIMEH
# Version formelle et mÃƒÂ©thodologiquement propre
# ============================================================

options(stringsAsFactors = FALSE)

base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

outdir <- file.path(base, "results", "analysis")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Fonction utilitaire
# ------------------------------------------------------------

safe_read <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

fmt <- function(x, digits = 6) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(as.numeric(x), format = "f", digits = digits)
  )
}

decision <- function(p) {
  ifelse(
    is.na(p),
    "Non testable",
    ifelse(p < 0.05, "Rejet de H0", "Non-rejet de H0")
  )
}

empty_row <- function(h, param, h0, h1, test,
                      estimation = NA, se = NA,
                      stat = NA, p = NA,
                      statut = "Non testable",
                      justification = "") {

  scalar <- function(x) {
    if (length(x) == 0 || is.null(x)) {
      return(NA)
    }
    x[1]
  }

  h <- scalar(h)
  param <- scalar(param)
  h0 <- scalar(h0)
  h1 <- scalar(h1)
  test <- scalar(test)
  estimation <- scalar(estimation)
  se <- scalar(se)
  stat <- scalar(stat)
  p <- scalar(p)
  statut <- scalar(statut)
  justification <- scalar(justification)

  dec <- if (is.na(p)) {
    "Non testable"
  } else if (p < 0.05) {
    "Rejet de H0"
  } else {
    "Non-rejet de H0"
  }

  data.frame(
    Hypothese = as.character(h),
    Parametre = as.character(param),
    H0 = as.character(h0),
    H1 = as.character(h1),
    Test = as.character(test),
    Estimation = suppressWarnings(as.numeric(estimation)),
    SE = suppressWarnings(as.numeric(se)),
    Statistique = suppressWarnings(as.numeric(stat)),
    P_value = suppressWarnings(as.numeric(p)),
    Decision_5pct = dec,
    Statut = as.character(statut),
    Justification = as.character(justification),
    stringsAsFactors = FALSE
  )
}

rows <- list()

# ============================================================
# H1
# GPRT -> volatilite
# ============================================================

h1file <- file.path(outdir, "FINAL_H1_H3_CORRECTED.csv")
h1detail <- file.path(outdir, "FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv")

d1 <- safe_read(h1detail)

if (!is.null(d1) && all(c("MODEL","PARAMETER","ESTIMATE","STD_ERROR","T_VALUE","P_VALUE") %in% names(d1))) {

  d <- d1[
    grepl("^GPRT_VIX$", d1$MODEL, ignore.case = TRUE),
  ]

  if (nrow(d) > 0) {

    for (i in seq_len(nrow(d))) {

      est <- suppressWarnings(as.numeric(d$ESTIMATE[i]))
      se  <- suppressWarnings(as.numeric(d$STD_ERROR[i]))
      st  <- suppressWarnings(as.numeric(d$T_VALUE[i]))
      pv  <- suppressWarnings(as.numeric(d$P_VALUE[i]))

      rows[[length(rows)+1]] <- empty_row(
        "H1",
        d$PARAMETER[i],
        "Le coefficient GPRT = 0",
        "Le coefficient GPRT > 0",
        "Test t individuel du coefficient",
        est, se, st, pv,
        "Test individuel",
        paste0(
          "Modele GPRT_VIX; pays = ",
          ifelse("COUNTRY" %in% names(d), d$COUNTRY[i], "NA"),
          ". Test individuel, pas un p-value global de H1."
        )
      )
    }

  } else {

    rows[[length(rows)+1]] <- empty_row(
      "H1",
      "GPRT_VIX",
      "Effet GPRT nul",
      "Effet GPRT positif",
      "Test t",
      statut = "Non testable",
      justification = "Aucun coefficient GPRT_VIX disponible."
    )
  }

} else {

  rows[[length(rows)+1]] <- empty_row(
    "H1",
    "GPRT_VIX",
    "Effet GPRT nul",
    "Effet GPRT positif",
    "Test t",
    statut = "Non testable",
    justification = "Structure du fichier de coefficients incompatible."
  )
}

# ============================================================
# H2
# DEMANDE
# ============================================================

d2 <- safe_read(h1detail)

if (!is.null(d2)) {

  d <- d2[
    grepl("^(GPRT_DEM|SUP_DEM)$", d2$MODEL, ignore.case = TRUE),
  ]

  if (nrow(d) > 0) {

    for (i in seq_len(nrow(d))) {

      rows[[length(rows)+1]] <- empty_row(
        "H2",
        d$PARAMETER[i],
        "Coefficient = 0",
        "Effet statistiquement different de zero",
        "Test t individuel du coefficient",
        suppressWarnings(as.numeric(d$ESTIMATE[i])),
        suppressWarnings(as.numeric(d$STD_ERROR[i])),
        suppressWarnings(as.numeric(d$T_VALUE[i])),
        suppressWarnings(as.numeric(d$P_VALUE[i])),
        "Test individuel",
        paste0(
          "Modele ", d$MODEL[i],
          "; H2 est evaluee au niveau des coefficients individuels. ",
          "Aucun test joint global n'est calcule ici."
        )
      )
    }

  } else {

    rows[[length(rows)+1]] <- empty_row(
      "H2",
      "GPRT_DEM / SUP_DEM",
      "Tous les coefficients sont nuls",
      "Au moins un effet est different de zero",
      "Test joint non disponible",
      statut = "Non testable globalement",
      justification = "Les tests individuels existent, mais aucun test joint global valide n'est disponible."
    )
  }

} else {

  rows[[length(rows)+1]] <- empty_row(
    "H2",
    "GPRT_DEM / SUP_DEM",
    "Effet nul",
    "Effet non nul",
    "Test t",
    statut = "Non testable",
    justification = "Fichier de coefficients absent."
  )
}

# ============================================================
# H3
# OFFRE
# ============================================================

if (!is.null(d2)) {

  d <- d2[
    grepl("^(GPRA_SUP|GPRT_SUP)$", d2$MODEL, ignore.case = TRUE),
  ]

  if (nrow(d) > 0) {

    for (i in seq_len(nrow(d))) {

      rows[[length(rows)+1]] <- empty_row(
        "H3",
        d$PARAMETER[i],
        "Coefficient = 0",
        "Effet statistiquement different de zero",
        "Test t individuel du coefficient",
        suppressWarnings(as.numeric(d$ESTIMATE[i])),
        suppressWarnings(as.numeric(d$STD_ERROR[i])),
        suppressWarnings(as.numeric(d$T_VALUE[i])),
        suppressWarnings(as.numeric(d$P_VALUE[i])),
        "Test individuel",
        paste0(
          "Modele ", d$MODEL[i],
          "; H3 est documentee par les tests individuels."
        )
      )
    }

  } else {

    rows[[length(rows)+1]] <- empty_row(
      "H3",
      "GPRA_SUP / GPRT_SUP",
      "Tous les coefficients sont nuls",
      "Au moins un effet est different de zero",
      "Test joint non disponible",
      statut = "Non testable globalement",
      justification = "Aucun test joint global valide disponible."
    )
  }
}

# ============================================================
# H4
# EXPORTATEURS / IMPORTATEURS
# ============================================================

rows[[length(rows)+1]] <- empty_row(
  "H4",
  "Exportateurs vs importateurs",
  "Les coefficients sont identiques entre groupes",
  "Les coefficients different entre groupes",
  "Test de difference entre groupes",
  statut = "NON TESTABLE",
  justification = paste(
    "La classification exportateur/importateur n'est pas disponible",
    "dans la structure actuelle des donnees."
  )
)

# ============================================================
# H5
# DEMANDE SELON HORIZON
# ============================================================

h5file <- file.path(
  outdir,
  "FINAL_FORMAL_H1_H9",
  "FINAL_H5_CORRECTED.csv"
)

h5 <- safe_read(h5file)

if (!is.null(h5) && nrow(h5) > 0) {

  for (i in seq_len(nrow(h5))) {

    est <- suppressWarnings(as.numeric(h5$diff[i]))
    se  <- suppressWarnings(as.numeric(h5$se_ind[i]))
    stat <- suppressWarnings(as.numeric(h5$z_ind[i]))
    pv <- suppressWarnings(as.numeric(h5$p_ind[i]))

    rows[[length(rows)+1]] <- empty_row(
      "H5",
      paste0(h5$model[i], " : theta1 - theta2"),
      "theta1 - theta2 = 0",
      "theta1 - theta2 != 0",
      "Test de difference sous hypothese d'independance",
      est,
      se,
      stat,
      pv,
      "Test formel approximatif",
      paste0(
        "Comparaison des horizons pour ",
        h5$model[i],
        "; covariance theta1/theta2 non disponible, ",
        "donc test sous hypothese d'independance."
      )
    )
  }

} else {

  rows[[length(rows)+1]] <- empty_row(
    "H5",
    "theta1 - theta2",
    "theta1 = theta2",
    "theta1 != theta2",
    "Test de difference",
    statut = "Non testable",
    justification = "Fichier H5 corrige absent."
  )
}

# ============================================================
# H6
# OFFRE SELON HORIZON
# ============================================================

h6file <- file.path(
  outdir,
  "FINAL_FORMAL_H1_H9",
  "FINAL_H6_CORRECTED.csv"
)

h6 <- safe_read(h6file)

if (!is.null(h6) && nrow(h6) > 0) {

  for (i in seq_len(nrow(h6))) {

    est <- suppressWarnings(as.numeric(h6$diff[i]))
    se  <- suppressWarnings(as.numeric(h6$se_ind[i]))
    stat <- suppressWarnings(as.numeric(h6$z_ind[i]))
    pv <- suppressWarnings(as.numeric(h6$p_ind[i]))

    rows[[length(rows)+1]] <- empty_row(
      "H6",
      paste0(h6$model[i], " : theta1 - theta2"),
      "theta1 - theta2 = 0",
      "theta1 - theta2 != 0",
      "Test de difference sous hypothese d'independance",
      est,
      se,
      stat,
      pv,
      "Test formel approximatif",
      paste0(
        "Comparaison des horizons pour ",
        h6$model[i],
        "; covariance theta1/theta2 non disponible, ",
        "donc test sous hypothese d'independance."
      )
    )
  }

} else {

  rows[[length(rows)+1]] <- empty_row(
    "H6",
    "theta1 - theta2",
    "theta1 = theta2",
    "theta1 != theta2",
    "Test de difference",
    statut = "Non testable",
    justification = "Fichier H6 corrige absent."
  )
}

# ============================================================
# H7
# GPRT vs GPRA - PRICE
# ============================================================

rows[[length(rows)+1]] <- empty_row(
  "H7",
  "GPRT vs GPRA - PRICE",
  "beta_GPRT = beta_GPRA",
  "beta_GPRT != beta_GPRA",
  "Comparaison de coefficients",
  statut = "NON TESTABLE",
  justification = paste(
    "Aucune paire compatible GPRT_PRE / GPRA_PRE.",
    "Le modele GPRA_PRE n'est pas disponible dans les estimations."
  )
)

# ============================================================
# H8
# GPRT vs GPRA - DEMAND
# ============================================================

rows[[length(rows)+1]] <- empty_row(
  "H8",
  "GPRT vs GPRA - DEMAND",
  "beta_GPRT = beta_GPRA",
  "beta_GPRT != beta_GPRA",
  "Comparaison de coefficients",
  statut = "NON TESTABLE",
  justification = paste(
    "Aucune paire compatible GPRT_DEM / GPRA_DEM.",
    "Le modele GPRA_DEM n'est pas disponible dans les estimations."
  )
)

# ============================================================
# H9
# GPRT vs GPRA - SUPPLY
# ============================================================

h9file <- file.path(
  outdir,
  "FINAL_FORMAL_H1_H9",
  "FINAL_H9_GPRT_GPRA_DETAIL.csv"
)

h9 <- safe_read(h9file)

if (!is.null(h9) && nrow(h9) > 0) {

  # verifier les noms disponibles
  print(names(h9))

  # colonnes attendues
  pcol <- intersect(c("p_value","p","P_value","P_VALUE"), names(h9))[1]
  dcol <- intersect(c("diff","Difference_GPRT_minus_GPRA","Difference"), names(h9))[1]
  scol <- intersect(c("se_diff","SE","se"), names(h9))[1]
  zcol <- intersect(c("z","z_ind","t"), names(h9))[1]

  for (i in seq_len(nrow(h9))) {

    pv <- if (!is.na(pcol)) suppressWarnings(as.numeric(h9[[pcol]][i])) else NA
    est <- if (!is.na(dcol)) suppressWarnings(as.numeric(h9[[dcol]][i])) else NA
    se <- if (!is.na(scol)) suppressWarnings(as.numeric(h9[[scol]][i])) else NA
    stat <- if (!is.na(zcol)) suppressWarnings(as.numeric(h9[[zcol]][i])) else NA

    param <- if ("param" %in% names(h9)) h9$param[i] else
             if ("parameter" %in% names(h9)) h9$parameter[i] else
             if ("PARAMETER" %in% names(h9)) h9$PARAMETER[i] else
             "theta"

    country <- if ("country" %in% names(h9)) h9$country[i] else
               if ("COUNTRY" %in% names(h9)) h9$COUNTRY[i] else
               "NA"

    rows[[length(rows)+1]] <- empty_row(
      "H9",
      paste0(param, " - ", country),
      "Coefficient GPRT = coefficient GPRA",
      "Coefficient GPRT != coefficient GPRA",
      "Comparaison appariee approximative",
      est,
      se,
      stat,
      pv,
      "Test formel approximatif",
      paste0(
        "Comparaison GPRT vs GPRA pour SUPPLY, pays = ",
        country,
        ". La covariance exacte entre estimations n'est pas disponible."
      )
    )
  }

} else {

  rows[[length(rows)+1]] <- empty_row(
    "H9",
    "GPRT vs GPRA - SUPPLY",
    "beta_GPRT = beta_GPRA",
    "beta_GPRT != beta_GPRA",
    "Comparaison de coefficients",
    statut = "Non testable",
    justification = "Fichier H9 detail absent."
  )
}

# ============================================================
# TABLEAU FINAL
# ============================================================

final <- do.call(rbind, rows)

# ordre des hypotheses
final$Hypothese <- factor(
  final$Hypothese,
  levels = paste0("H", 1:9)
)

final <- final[order(final$Hypothese), ]

final$Hypothese <- as.character(final$Hypothese)

# ============================================================
# EXPORT CSV
# ============================================================

csvfile <- file.path(
  outdir,
  "FINAL_TABLEAU_H1_H9_FORMEL.csv"
)

write.csv(
  final,
  csvfile,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# EXPORT TXT
# ============================================================

txtfile <- file.path(
  outdir,
  "FINAL_TABLEAU_H1_H9_FORMEL.txt"
)

sink(txtfile)

cat("============================================================\n")
cat(" TABLEAU FINAL FORMEL DES HYPOTHESES H1-H9\n")
cat(" RIMEH - GARCH-MIDAS\n")
cat("============================================================\n\n")

print(final, row.names = FALSE)

cat("\n\n============================================================\n")
cat(" SYNTHESE\n")
cat("============================================================\n\n")

for (h in paste0("H",1:9)) {

  d <- final[final$Hypothese == h, ]

  cat(h, " : ")

  if (all(d$Statut == "NON TESTABLE")) {
    cat("NON TESTABLE\n")
  } else {
    cat(
      nrow(d),
      "observation(s) / test(s) documente(s)\n"
    )
  }
}

sink()

# ============================================================
# XLSX SI OPENXLSX EST DISPONIBLE
# ============================================================

if (requireNamespace("openxlsx", quietly = TRUE)) {

  xlsxfile <- file.path(
    outdir,
    "FINAL_TABLEAU_H1_H9_FORMEL.xlsx"
  )

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(
    wb,
    "H1-H9"
  )

  openxlsx::writeData(
    wb,
    "H1-H9",
    final
  )

  openxlsx::freezePane(
    wb,
    "H1-H9",
    firstRow = TRUE
  )

  openxlsx::setColWidths(
    wb,
    "H1-H9",
    cols = 1:ncol(final),
    widths = "auto"
  )

  openxlsx::addStyle(
    wb,
    "H1-H9",
    style = openxlsx::createStyle(
      textDecoration = "bold"
    ),
    rows = 1,
    cols = 1:ncol(final),
    gridExpand = TRUE
  )

  openxlsx::saveWorkbook(
    wb,
    xlsxfile,
    overwrite = TRUE
  )

  cat("\nXLSX cree :", xlsxfile, "\n")

} else {

  cat(
    "\nPackage openxlsx non disponible.",
    "\nLe CSV et TXT ont quand meme ete crees.\n"
  )
}

# ============================================================
# CONTROLE FINAL
# ============================================================

cat("\n============================================================\n")
cat(" TABLEAU FINAL H1-H9 CREE\n")
cat("============================================================\n")

cat("\nCSV : ", csvfile, "\n")
cat("TXT : ", txtfile, "\n")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  cat(
    "XLSX : ",
    file.path(outdir, "FINAL_TABLEAU_H1_H9_FORMEL.xlsx"),
    "\n"
  )
}

cat("\nNombre total de lignes :", nrow(final), "\n")
cat("Hypotheses presentes :", paste(unique(final$Hypothese), collapse=", "), "\n")

cat("\n------------------------------------------------------------\n")
cat("H7 : NON TESTABLE - GPRA_PRE absent\n")
cat("H8 : NON TESTABLE - GPRA_DEM absent\n")
cat("H9 : GPRT_SUP vs GPRA_SUP - comparaison approximative\n")
cat("------------------------------------------------------------\n")

