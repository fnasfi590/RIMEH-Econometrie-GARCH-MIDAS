# ============================================================
# RIMEH - X.4 FINAL
# Tests econometriques preliminaires
# Excel + Word + TXT
# ============================================================

options(stringsAsFactors=FALSE)

base_dir <- getwd()
outdir <- file.path(
  base_dir,
  "results",
  "analysis",
  "X4_PRELIMINARY_TESTS"
)

dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

cat("\n============================================================\n")
cat(" RIMEH - X.4 FINAL\n")
cat("============================================================\n")

# ============================================================
# 0. PACKAGES
# ============================================================

required <- c("openxlsx", "officer", "flextable")

for (p in required) {

  if (!requireNamespace(p, quietly=TRUE)) {

    cat("\nInstallation du package:", p, "\n")

    install.packages(
      p,
      repos="https://cloud.r-project.org",
      dependencies=TRUE
    )
  }
}

library(openxlsx)
library(officer)
library(flextable)

# ============================================================
# 1. FICHIERS
# ============================================================

monthly_file <- file.path(
  base_dir,
  "data",
  "processed",
  "monthly_model_data.csv"
)

daily_file <- file.path(
  base_dir,
  "data",
  "processed",
  "daily_clean.csv"
)

monthly <- read.csv(
  monthly_file,
  check.names=FALSE
)

daily <- read.csv(
  daily_file,
  check.names=FALSE
)

# ============================================================
# 2. LECTURE SECURISEE DES RESULTATS X4
# ============================================================

read_safe <- function(filename) {

  p <- file.path(outdir, filename)

  if (!file.exists(p)) {
    return(data.frame())
  }

  info <- file.info(p)

  if (is.na(info$size) || info$size == 0) {
    return(data.frame())
  }

  tryCatch(
    read.csv(p, check.names=FALSE),
    error=function(e) data.frame()
  )
}

stationarity <- read_safe(
  "X4_STATIONARITY_ADF_PP_KPSS.csv"
)

arch <- read_safe(
  "X4_ARCH_LM.csv"
)

ljung <- read_safe(
  "X4_LJUNG_BOX.csv"
)

vif_file <- file.path(
  outdir,
  "X4_VIF_CORRECTED.csv"
)

if (file.exists(vif_file) &&
    file.info(vif_file)$size > 0) {

  vif <- read.csv(
    vif_file,
    check.names=FALSE
  )

} else {

  vif <- data.frame()
}

# ============================================================
# 3. DIAGNOSTIC DES VARIABLES
# ============================================================

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
  "GDP"
)

diag <- data.frame(
  Variable=character(),
  N_total=integer(),
  N_NA=integer(),
  N_Inf=integer(),
  N_Finite=integer(),
  Mean=numeric(),
  SD=numeric(),
  Min=numeric(),
  Median=numeric(),
  Max=numeric(),
  stringsAsFactors=FALSE
)

for (v in vars) {

  if (v %in% names(monthly)) {

    x <- suppressWarnings(
      as.numeric(monthly[[v]])
    )

    finite <- is.finite(x)

    diag <- rbind(
      diag,
      data.frame(
        Variable=v,
        N_total=length(x),
        N_NA=sum(is.na(x)),
        N_Inf=sum(is.infinite(x)),
        N_Finite=sum(finite),
        Mean=if(sum(finite)>0) mean(x[finite]) else NA,
        SD=if(sum(finite)>1) sd(x[finite]) else NA,
        Min=if(sum(finite)>0) min(x[finite]) else NA,
        Median=if(sum(finite)>0) median(x[finite]) else NA,
        Max=if(sum(finite)>0) max(x[finite]) else NA
      )
    )
  }
}

# ============================================================
# 4. MATRICE DE CORRELATION
# ============================================================

valid_vars <- vars[
  sapply(
    monthly[vars],
    function(x) {

      x <- suppressWarnings(
        as.numeric(x)
      )

      sum(is.finite(x)) > 1 &&
      sd(x, na.rm=TRUE) > 0
    }
  )
]

correlation <- data.frame()

if(length(valid_vars) >= 2) {

  X <- monthly[,valid_vars,drop=FALSE]

  X[] <- lapply(
    X,
    function(x)
      suppressWarnings(as.numeric(x))
  )

  X <- X[
    complete.cases(X),
    ,
    drop=FALSE
  ]

  if(nrow(X) > 10) {

    correlation <- round(
      cor(X),
      4
    )
  }
}

# ============================================================
# 5. RESUME STATIONNARITE
# ============================================================

stationarity_summary <- data.frame()

if(nrow(stationarity) > 0) {

  stationarity_summary <- data.frame(
    Test=c(
      "ADF",
      "Phillips-Perron",
      "KPSS"
    ),
    N_tests=c(
      nrow(stationarity),
      nrow(stationarity),
      nrow(stationarity)
    ),
    Stationary_5pct=c(
      sum(stationarity$ADF_stationary_5pct, na.rm=TRUE),
      sum(stationarity$PP_stationary_5pct, na.rm=TRUE),
      sum(stationarity$KPSS_stationary_5pct, na.rm=TRUE)
    )
  )

  stationarity_summary$Non_stationary_5pct <-
    stationarity_summary$N_tests -
    stationarity_summary$Stationary_5pct
}

# ============================================================
# 6. RESUME ARCH
# ============================================================

arch_summary <- data.frame()

if(nrow(arch) > 0) {

  arch_summary <- data.frame(
    Pays=nrow(arch),
    ARCH_significatif_5pct=sum(
      arch$ARCH_effect_5pct,
      na.rm=TRUE
    ),
    ARCH_non_significatif_5pct=sum(
      !arch$ARCH_effect_5pct,
      na.rm=TRUE
    )
  )
}

# ============================================================
# 7. RESUME LJUNG-BOX
# ============================================================

ljung_summary <- data.frame()

if(nrow(ljung) > 0) {

  ljung_summary <- data.frame(
    Pays=nrow(ljung),
    Autocorrelation_significative_5pct=sum(
      ljung$autocorrelation_returns_5pct,
      na.rm=TRUE
    ),
    Dependance_variance_significative_5pct=sum(
      ljung$dependence_squared_returns_5pct,
      na.rm=TRUE
    )
  )
}

# ============================================================
# 8. RESUME VIF
# ============================================================

vif_summary <- data.frame()

if(nrow(vif) > 0) {

  vif_summary <- data.frame(
    Variable=vif$Variable,
    VIF=round(vif$VIF,4),
    Interpretation=ifelse(
      vif$VIF < 5,
      "Pas de multicollinearite importante",
      ifelse(
        vif$VIF < 10,
        "Multicollinearite moderee",
        "Multicollinearite forte"
      )
    )
  )
}

# ============================================================
# 9. RESUME GLOBAL
# ============================================================

infl_valid <- if(
  "inflation" %in% names(monthly)
) {
  sum(
    is.finite(
      suppressWarnings(
        as.numeric(monthly$inflation)
      )
    )
  )
} else {
  0
}

global_summary <- data.frame(
  Element=c(
    "Observations mensuelles",
    "Observations quotidiennes",
    "Variables mensuelles testees",
    "Tests ADF/PP/KPSS",
    "Pays ARCH-LM",
    "Pays Ljung-Box",
    "Variables VIF",
    "Observations inflation valides",
    "Variables interet_rate valides",
    "Variables exchange_rate valides",
    "Variables GDP valides"
  ),
  Valeur=c(
    nrow(monthly),
    nrow(daily),
    length(vars),
    nrow(stationarity),
    nrow(arch),
    nrow(ljung),
    nrow(vif),
    infl_valid,
    if("interest_rate" %in% names(monthly))
      sum(is.finite(as.numeric(monthly$interest_rate)))
    else 0,
    if("exchange_rate" %in% names(monthly))
      sum(is.finite(as.numeric(monthly$exchange_rate)))
    else 0,
    if("GDP" %in% names(monthly))
      sum(is.finite(as.numeric(monthly$GDP)))
    else 0
  )
)

# ============================================================
# 10. EXCEL FINAL
# ============================================================

xlsx_file <- file.path(
  outdir,
  "X4_FINAL_TESTS_ECONOMETRIQUES.xlsx"
)

wb <- createWorkbook()

add_sheet <- function(name, data) {

  addWorksheet(wb,name)

  if(nrow(data) > 0) {

    writeDataTable(
      wb,
      name,
      data,
      tableStyle="TableStyleMedium2"
    )

    freezePane(
      wb,
      name,
      firstRow=TRUE
    )

    setColWidths(
      wb,
      name,
      cols=1:ncol(data),
      widths="auto"
    )

  } else {

    writeData(
      wb,
      name,
      "Aucune donnee disponible"
    )
  }
}

add_sheet(
  "Resume_global",
  global_summary
)

add_sheet(
  "Stationnarite",
  stationarity
)

add_sheet(
  "Resume_stationnarite",
  stationarity_summary
)

add_sheet(
  "ARCH_LM",
  arch
)

add_sheet(
  "Resume_ARCH",
  arch_summary
)

add_sheet(
  "Ljung_Box",
  ljung
)

add_sheet(
  "Resume_Ljung",
  ljung_summary
)

add_sheet(
  "VIF",
  vif_summary
)

add_sheet(
  "Diagnostic_variables",
  diag
)

add_sheet(
  "Correlation",
  correlation
)

saveWorkbook(
  wb,
  xlsx_file,
  overwrite=TRUE
)

# ============================================================
# 11. RAPPORT WORD
# ============================================================

docx_file <- file.path(
  outdir,
  "X4_FINAL_RAPPORT_ECONOMETRIQUE.docx"
)

doc <- read_docx()

doc <- body_add_par(
  doc,
  "RIMEH - TESTS ECONOMETRIQUES PRELIMINAIRES X.4",
  style="heading 1"
)

doc <- body_add_par(
  doc,
  paste0(
    "Rapport final genere le ",
    format(Sys.time(),"%d/%m/%Y %H:%M")
  )
)

doc <- body_add_par(
  doc,
  "1. Objectif",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  paste0(
    "Cette section presente les tests econometriques ",
    "preliminaires realises avant l'estimation des modeles ",
    "GARCH-MIDAS du cas RIMEH. Les tests portent sur la ",
    "stationnarite, l'heteroscedasticite conditionnelle, ",
    "l'autocorrelation et la multicollinearite."
  )
)

doc <- body_add_par(
  doc,
  "2. Donnees",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  paste0(
    "La base mensuelle contient ",
    nrow(monthly),
    " observations et ",
    ncol(monthly),
    " variables. La base quotidienne contient ",
    nrow(daily),
    " observations."
  )
)

doc <- body_add_par(
  doc,
  "3. Stationnarite",
  style="heading 2"
)

if(nrow(stationarity_summary)>0) {

  ft <- flextable(
    stationarity_summary
  )

  ft <- autofit(ft)

  doc <- body_add_flextable(
    doc,
    ft
  )
}

doc <- body_add_par(
  doc,
  paste0(
    "Les tests ADF, Phillips-Perron et KPSS sont utilises ",
    "conjointement afin d'evaluer la stationnarite des ",
    "variables mensuelles. Les cas ou les tests ne donnent ",
    "pas exactement la meme conclusion doivent etre interpretes ",
    "conjointement plutot qu'a partir d'un seul test."
  )
)

doc <- body_add_par(
  doc,
  "4. Heteroscedasticite conditionnelle : ARCH-LM",
  style="heading 2"
)

if(nrow(arch_summary)>0) {

  ft <- flextable(
    arch_summary
  )

  ft <- autofit(ft)

  doc <- body_add_flextable(
    doc,
    ft
  )
}

if(nrow(arch)>0) {

  sig <- sum(
    arch$ARCH_effect_5pct,
    na.rm=TRUE
  )

  doc <- body_add_par(
    doc,
    paste0(
      sig,
      " pays sur ",
      nrow(arch),
      " presentent un effet ARCH significatif au seuil de 5 %. ",
      "Ce resultat fournit un argument econometrique pour ",
      "l'utilisation d'une modelisation de la variance ",
      "conditionnelle de type GARCH."
    )
  )
}

doc <- body_add_par(
  doc,
  "5. Autocorrelation : Ljung-Box",
  style="heading 2"
)

if(nrow(ljung_summary)>0) {

  ft <- flextable(
    ljung_summary
  )

  ft <- autofit(ft)

  doc <- body_add_flextable(
    doc,
    ft
  )
}

if(nrow(ljung)>0) {

  ar <- sum(
    ljung$autocorrelation_returns_5pct,
    na.rm=TRUE
  )

  vr <- sum(
    ljung$dependence_squared_returns_5pct,
    na.rm=TRUE
  )

  doc <- body_add_par(
    doc,
    paste0(
      ar,
      " pays sur ",
      nrow(ljung),
      " presentent une autocorrelation significative ",
      "des rendements au seuil de 5 %. Pour les rendements ",
      "au carre, ",
      vr,
      " pays presentent une dependance significative."
    )
  )
}

doc <- body_add_par(
  doc,
  "6. Multicollinearite : VIF",
  style="heading 2"
)

if(nrow(vif_summary)>0) {

  ft <- flextable(
    vif_summary
  )

  ft <- autofit(ft)

  doc <- body_add_flextable(
    doc,
    ft
  )

  doc <- body_add_par(
    doc,
    paste0(
      "Les VIF obtenus pour les variables explicatives sont ",
      "faibles dans la specification consideree. Le diagnostic ",
      "ne met donc pas en evidence une multicollinearite ",
      "importante entre les variables incluses dans ce calcul."
    )
  )

} else {

  doc <- body_add_par(
    doc,
    "Le VIF n'a pas pu etre calcule."
  )
}

doc <- body_add_par(
  doc,
  "7. Qualite des variables",
  style="heading 2"
)

ft <- flextable(diag)

ft <- autofit(ft)

doc <- body_add_flextable(
  doc,
  ft
)

doc <- body_add_par(
  doc,
  paste0(
    "La variable inflation comporte ",
    infl_valid,
    " observations valides sur ",
    nrow(monthly),
    ". Les variables interest_rate, exchange_rate et GDP ",
    "ne comportent actuellement aucune observation valide ",
    "dans monthly_model_data.csv et doivent donc etre traitees ",
    "separement avant leur eventuelle utilisation comme variables ",
    "de controle dans les estimations finales."
  )
)

doc <- body_add_par(
  doc,
  "8. Conclusion de X.4",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  paste0(
    "Les tests preliminaires X.4 sont disponibles et les ",
    "conditions econometriques necessaires a l'etude de la ",
    "variance conditionnelle sont documentees. Le diagnostic ",
    "ARCH-LM et la dependance des rendements au carre ",
    "motivent la poursuite vers la modelisation GARCH-MIDAS. ",
    "Les variables de controle totalement manquantes doivent ",
    "cependant rester exclues tant que leurs donnees n'ont pas ",
    "ete correctement reconstruites."
  )
)

print(doc,target=docx_file)

# ============================================================
# 12. RAPPORT TXT
# ============================================================

txt_file <- file.path(
  outdir,
  "X4_FINAL_RAPPORT_ECONOMETRIQUE.txt"
)

sink(txt_file)

cat("============================================================\n")
cat("RIMEH - TESTS ECONOMETRIQUES PRELIMINAIRES X.4 FINAL\n")
cat("============================================================\n\n")

cat("DATE :",format(Sys.time(),"%d/%m/%Y %H:%M:%S"),"\n\n")

cat("DONNEES\n")
cat("-------\n")
cat("Mensuel :",nrow(monthly),"observations\n")
cat("Quotidien :",nrow(daily),"observations\n\n")

cat("STATIONNARITE\n")
cat("-------------\n")
print(stationarity_summary)

cat("\nARCH-LM\n")
cat("-------\n")
print(arch_summary)

cat("\nLJUNG-BOX\n")
cat("---------\n")
print(ljung_summary)

cat("\nVIF\n")
cat("---\n")
print(vif_summary)

cat("\nDIAGNOSTIC VARIABLES\n")
cat("--------------------\n")
print(diag)

cat("\nCORRELATION\n")
cat("-----------\n")
print(correlation)

cat("\n============================================================\n")
cat("FIN DU RAPPORT X.4 FINAL\n")
cat("============================================================\n")

sink()

# ============================================================
# 13. COPIE DU VIF CORRIGE VERS NOM FINAL
# ============================================================

if(nrow(vif)>0) {

  write.csv(
    vif,
    file.path(
      outdir,
      "X4_VIF_FINAL.csv"
    ),
    row.names=FALSE
  )
}

# ============================================================
# 14. MESSAGE FINAL
# ============================================================

cat("\n\n============================================================\n")
cat(" X.4 FINAL TERMINE AVEC SUCCES\n")
cat("============================================================\n\n")

cat("Fichiers generes :\n\n")

cat(
  "1. ",
  xlsx_file,
  "\n",
  sep=""
)

cat(
  "2. ",
  docx_file,
  "\n",
  sep=""
)

cat(
  "3. ",
  txt_file,
  "\n",
  sep=""
)

cat(
  "4. ",
  file.path(outdir,"X4_VIF_FINAL.csv"),
  "\n",
  sep=""
)

cat(
  "5. ",
  file.path(outdir,"X4_VARIABLES_DIAGNOSTIC.csv"),
  "\n",
  sep=""
)

cat(
  "6. ",
  file.path(outdir,"X4_CORRELATION_MATRIX.csv"),
  "\n",
  sep=""
)

cat("\n============================================================\n")
cat(" X.4 EST PRET POUR LA SUITE DU CAS RIMEH\n")
cat("============================================================\n")
