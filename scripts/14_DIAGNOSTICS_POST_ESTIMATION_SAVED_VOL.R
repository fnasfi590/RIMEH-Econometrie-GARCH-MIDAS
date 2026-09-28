suppressPackageStartupMessages({
  library(xts)
  library(FinTS)
  library(openxlsx)
})

ROOT <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

RDS_DIR <- file.path(ROOT, "results/models/garch_midas")
OUT_DIR <- file.path(ROOT, "results/analysis/RIMEH_FINAL")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# DONNEES DAILY
# ============================================================

daily <- read.csv(
  file.path(ROOT, "data/processed/daily_clean.csv"),
  stringsAsFactors = FALSE
)

daily$date <- as.Date(daily$date)

cat("============================================\n")
cat("RIMEH - DIAGNOSTICS POST-ESTIMATION\n")
cat("============================================\n")
cat("Daily :", nrow(daily), "observations\n")

# ============================================================
# LISTE DES MODELES
# ============================================================

files <- list.files(
  RDS_DIR,
  pattern = "\\.rds$",
  full.names = TRUE
)

files <- sort(files)

cat("RDS :", length(files), "\n\n")

# ============================================================
# FONCTION DIAGNOSTIC
# ============================================================

diagnostic_model <- function(f) {

  model_name <- tools::file_path_sans_ext(basename(f))

  ans <- data.frame(
    Modele = model_name,
    Pays = NA_character_,
    Specification = NA_character_,
    N_modele = NA_integer_,
    N_daily = NA_integer_,
    N_valides = NA_integer_,
    Mean_residus = NA_real_,
    SD_residus = NA_real_,
    Min_residus = NA_real_,
    Max_residus = NA_real_,
    LB_residus_stat = NA_real_,
    LB_residus_p = NA_real_,
    LB_residus2_stat = NA_real_,
    LB_residus2_p = NA_real_,
    ARCH_LM_stat = NA_real_,
    ARCH_LM_p = NA_real_,
    LogLik = NA_real_,
    AIC = NA_real_,
    BIC = NA_real_,
    MSE_pct = NA_real_,
    QLIKE = NA_real_,
    Statut = "ERREUR",
    Erreur = NA_character_,
    stringsAsFactors = FALSE
  )

  tryCatch({

    # --------------------------------------------------------
    # LECTURE MODELE
    # --------------------------------------------------------

    m <- readRDS(f)

    # --------------------------------------------------------
    # NOM MODELE
    # --------------------------------------------------------

    parts <- strsplit(model_name, "_", fixed = TRUE)[[1]]

    if (length(parts) >= 2) {
      ans$Pays <- parts[1]
      ans$Specification <- paste(parts[-1], collapse = "_")
    }

    ans$N_modele <- as.integer(m$obs)

    # --------------------------------------------------------
    # PERIODE
    # --------------------------------------------------------

    pstart <- as.Date(m$period[1])
    pend   <- as.Date(m$period[2])

    # --------------------------------------------------------
    # DAILY CORRESPONDANT
    # --------------------------------------------------------

    d <- daily[
      daily$country == ans$Pays &
      daily$date >= pstart &
      daily$date <= pend,
      ,
      drop = FALSE
    ]

    ans$N_daily <- nrow(d)

    if (nrow(d) == 0) {
      stop("Aucune observation daily correspondante")
    }

    # --------------------------------------------------------
    # RETOURS
    # --------------------------------------------------------

    r <- as.numeric(d$return)

    # --------------------------------------------------------
    # VOLATILITE SAUVEGARDEE
    # --------------------------------------------------------

    sigma <- as.numeric(m$est_vol_in_s)

    if (length(r) != length(sigma)) {
      stop(
        paste0(
          "Longueurs incompatibles return=",
          length(r),
          " vol=",
          length(sigma)
        )
      )
    }

    # --------------------------------------------------------
    # OBSERVATIONS VALIDES
    # --------------------------------------------------------

    valid <- is.finite(r) &
             is.finite(sigma) &
             sigma > 0

    ans$N_valides <- sum(valid)

    if (ans$N_valides < 100) {
      stop("Trop peu d'observations valides")
    }

    r <- r[valid]
    sigma <- sigma[valid]

    # --------------------------------------------------------
    # RESIDUS STANDARDISES
    # --------------------------------------------------------

    z <- r / sigma

    if (any(!is.finite(z))) {
      stop("Residus standardises non finis")
    }

    ans$Mean_residus <- mean(z)
    ans$SD_residus <- sd(z)
    ans$Min_residus <- min(z)
    ans$Max_residus <- max(z)

    # --------------------------------------------------------
    # LJUNG-BOX RESIDUS
    # --------------------------------------------------------

    lb1 <- Box.test(
      z,
      lag = 12,
      type = "Ljung-Box"
    )

    ans$LB_residus_stat <- unname(lb1$statistic)
    ans$LB_residus_p <- lb1$p.value

    # --------------------------------------------------------
    # LJUNG-BOX RESIDUS CARRES
    # --------------------------------------------------------

    lb2 <- Box.test(
      z^2,
      lag = 12,
      type = "Ljung-Box"
    )

    ans$LB_residus2_stat <- unname(lb2$statistic)
    ans$LB_residus2_p <- lb2$p.value

    # --------------------------------------------------------
    # ARCH-LM
    # --------------------------------------------------------

    arch <- FinTS::ArchTest(
      z,
      lags = 12
    )

    ans$ARCH_LM_stat <- unname(arch$statistic)
    ans$ARCH_LM_p <- arch$p.value

    # --------------------------------------------------------
    # CRITERES DU MODELE
    # --------------------------------------------------------

    ans$LogLik <- as.numeric(m$loglik)

    if (!is.null(names(m$inf_criteria))) {
      if ("AIC" %in% names(m$inf_criteria)) {
        ans$AIC <- as.numeric(m$inf_criteria["AIC"])
      }

      if ("BIC" %in% names(m$inf_criteria)) {
        ans$BIC <- as.numeric(m$inf_criteria["BIC"])
      }
    }

    if (!is.null(names(m$loss_in_s))) {
      if ("MSE(%)" %in% names(m$loss_in_s)) {
        ans$MSE_pct <- as.numeric(m$loss_in_s["MSE(%)"])
      }

      if ("QLIKE" %in% names(m$loss_in_s)) {
        ans$QLIKE <- as.numeric(m$loss_in_s["QLIKE"])
      }
    }

    ans$Statut <- "OK"

  }, error = function(e) {

    ans$Erreur <- conditionMessage(e)

  })

  return(ans)
}

# ============================================================
# EXECUTION
# ============================================================

results <- vector("list", length(files))

for (i in seq_along(files)) {

  cat(
    sprintf(
      "[%02d/%02d] %s ... ",
      i,
      length(files),
      basename(files[i])
    )
  )

  results[[i]] <- diagnostic_model(files[i])

  if (results[[i]]$Statut == "OK") {
    cat("OK\n")
  } else {
    cat("ERREUR :", results[[i]]$Erreur, "\n")
  }
}

diag <- do.call(rbind, results)

# ============================================================
# EXPORT CSV
# ============================================================

csv_file <- file.path(
  OUT_DIR,
  "14_DIAGNOSTICS_POST_ESTIMATION_76.csv"
)

write.csv(
  diag,
  csv_file,
  row.names = FALSE,
  na = ""
)

# ============================================================
# RESUME
# ============================================================

resume <- data.frame(
  Indicateur = c(
    "RDS total",
    "Diagnostics OK",
    "Erreurs",
    "LB residus calcule",
    "LB residus carres calcule",
    "ARCH-LM calcule",
    "LogLik disponible",
    "AIC disponible",
    "BIC disponible"
  ),
  Valeur = c(
    nrow(diag),
    sum(diag$Statut == "OK"),
    sum(diag$Statut != "OK"),
    sum(is.finite(diag$LB_residus_p)),
    sum(is.finite(diag$LB_residus2_p)),
    sum(is.finite(diag$ARCH_LM_p)),
    sum(is.finite(diag$LogLik)),
    sum(is.finite(diag$AIC)),
    sum(is.finite(diag$BIC))
  )
)

write.csv(
  resume,
  file.path(
    OUT_DIR,
    "14_RESUME_DIAGNOSTICS.csv"
  ),
  row.names = FALSE
)

# ============================================================
# ERREURS
# ============================================================

errors <- diag[diag$Statut != "OK", ]

write.csv(
  errors,
  file.path(
    OUT_DIR,
    "14_DIAGNOSTICS_POST_ESTIMATION_ERREURS.csv"
  ),
  row.names = FALSE
)

# ============================================================
# RAPPORT
# ============================================================

report_file <- file.path(
  OUT_DIR,
  "14_RAPPORT_DIAGNOSTICS.txt"
)

sink(report_file)

cat("============================================================\n")
cat("RIMEH - RAPPORT DIAGNOSTICS POST-ESTIMATION\n")
cat("============================================================\n\n")

cat("RDS total :", nrow(diag), "\n")
cat("Diagnostics OK :", sum(diag$Statut == "OK"), "\n")
cat("Erreurs :", sum(diag$Statut != "OK"), "\n\n")

cat("------------------------------------------------------------\n")
cat("CONTROLE RESIDUS\n")
cat("------------------------------------------------------------\n")

ok <- diag[diag$Statut == "OK", ]

if (nrow(ok) > 0) {

  cat(
    "Mean residus median :",
    median(ok$Mean_residus, na.rm = TRUE),
    "\n"
  )

  cat(
    "SD residus median :",
    median(ok$SD_residus, na.rm = TRUE),
    "\n"
  )

  cat(
    "LB residus p < 5% :",
    sum(ok$LB_residus_p < 0.05, na.rm = TRUE),
    "/",
    nrow(ok),
    "\n"
  )

  cat(
    "LB residus² p < 5% :",
    sum(ok$LB_residus2_p < 0.05, na.rm = TRUE),
    "/",
    nrow(ok),
    "\n"
  )

  cat(
    "ARCH-LM p < 5% :",
    sum(ok$ARCH_LM_p < 0.05, na.rm = TRUE),
    "/",
    nrow(ok),
    "\n"
  )
}

cat("\n------------------------------------------------------------\n")
cat("MODELES EN ERREUR\n")
cat("------------------------------------------------------------\n")

if (nrow(errors) > 0) {
  print(errors[, c("Modele", "Erreur")])
} else {
  cat("Aucune erreur.\n")
}

sink()

# ============================================================
# EXCEL
# ============================================================

xlsx_file <- file.path(
  OUT_DIR,
  "14_DIAGNOSTICS_POST_ESTIMATION_76.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "Diagnostics_76")
writeData(wb, "Diagnostics_76", diag)

addWorksheet(wb, "Resume")
writeData(wb, "Resume", resume)

addWorksheet(wb, "Erreurs")
writeData(wb, "Erreurs", errors)

saveWorkbook(
  wb,
  xlsx_file,
  overwrite = TRUE
)

# ============================================================
# CONTROLE FINAL
# ============================================================

control <- data.frame(
  Controle = c(
    "RDS total",
    "Diagnostics OK",
    "Erreurs",
    "LB residus",
    "LB residus carres",
    "ARCH-LM",
    "LogLik",
    "AIC",
    "BIC"
  ),
  N = c(
    nrow(diag),
    sum(diag$Statut == "OK"),
    sum(diag$Statut != "OK"),
    sum(is.finite(diag$LB_residus_p)),
    sum(is.finite(diag$LB_residus2_p)),
    sum(is.finite(diag$ARCH_LM_p)),
    sum(is.finite(diag$LogLik)),
    sum(is.finite(diag$AIC)),
    sum(is.finite(diag$BIC))
  )
)

write.csv(
  control,
  file.path(
    OUT_DIR,
    "14_CONTROLE_DIAGNOSTICS.csv"
  ),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("TERMINE\n")
cat("============================================================\n")
print(control)

cat("\nFichiers :\n")
cat(csv_file, "\n")
cat(xlsx_file, "\n")
cat(report_file, "\n")