# ============================================================
# RIMEH - DIAGNOSTICS POST-ESTIMATION EXACTS - FIX DATE
# ============================================================

options(stringsAsFactors = FALSE)
options(warn = 1)

ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)

OUT <- file.path(
  ROOT,
  "results",
  "analysis",
  "RIMEH_FINAL"
)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat(" RIMEH - DIAGNOSTICS POST-ESTIMATION EXACTS - FIX\n")
cat("============================================================\n")
cat("ROOT :", ROOT, "\n")
cat("Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

library(rumidas)
library(xts)
library(zoo)
library(dplyr)
library(readr)
library(openxlsx)

# ------------------------------------------------------------
# FONCTION DATE ROBUSTE
# ------------------------------------------------------------

to_date_safe <- function(x) {

  if (inherits(x, "Date"))
    return(x)

  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt"))
    return(as.Date(x))

  x <- as.character(x)

  out <- suppressWarnings(as.Date(x))

  bad <- is.na(out)

  if (any(bad)) {

    out[bad] <- suppressWarnings(
      as.Date(
        substr(x[bad], 1, 10),
        format = "%Y-%m-%d"
      )
    )
  }

  bad <- is.na(out)

  if (any(bad)) {

    out[bad] <- suppressWarnings(
      as.Date(
        x[bad],
        format = "%d/%m/%Y"
      )
    )
  }

  if (any(is.na(out))) {

    stop(
      "Impossible de convertir certaines dates : ",
      paste(head(x[is.na(out)], 10), collapse = ", ")
    )
  }

  out
}

# ------------------------------------------------------------
# FICHIERS
# ------------------------------------------------------------

daily_file <- file.path(
  ROOT,
  "data",
  "processed",
  "daily_clean.csv"
)

monthly_file <- file.path(
  ROOT,
  "data",
  "processed",
  "monthly_model_data.csv"
)

daily <- read_csv(
  daily_file,
  show_col_types = FALSE
)

monthly <- read_csv(
  monthly_file,
  show_col_types = FALSE
)

cat("Daily :", nrow(daily), "x", ncol(daily), "\n")
cat("Monthly :", nrow(monthly), "x", ncol(monthly), "\n\n")

# ------------------------------------------------------------
# COLONNES
# ------------------------------------------------------------

find_col <- function(df, patterns) {

  for (p in patterns) {

    hit <- grep(
      p,
      names(df),
      ignore.case = TRUE,
      value = TRUE
    )

    if (length(hit) > 0)
      return(hit[1])
  }

  stop(
    "Colonne introuvable : ",
    paste(patterns, collapse = ", ")
  )
}

daily_date_col <- find_col(
  daily,
  c("^date$", "date", "day")
)

daily_country_col <- find_col(
  daily,
  c("^country$", "country", "iso")
)

daily_return_col <- find_col(
  daily,
  c("^return$", "returns", "ret", "log_return", "logret")
)

monthly_date_col <- find_col(
  monthly,
  c("^date$", "date", "month")
)

monthly_country_col <- find_col(
  monthly,
  c("^country$", "country", "iso")
)

# ------------------------------------------------------------
# DATES
# ------------------------------------------------------------

daily[[daily_date_col]] <- to_date_safe(
  daily[[daily_date_col]]
)

monthly[[monthly_date_col]] <- to_date_safe(
  monthly[[monthly_date_col]]
)

daily$.__RETURN__ <- suppressWarnings(
  as.numeric(daily[[daily_return_col]])
)

cat(
  "Daily dates :",
  min(daily[[daily_date_col]], na.rm = TRUE),
  "->",
  max(daily[[daily_date_col]], na.rm = TRUE),
  "\n"
)

cat(
  "Monthly dates :",
  min(monthly[[monthly_date_col]], na.rm = TRUE),
  "->",
  max(monthly[[monthly_date_col]], na.rm = TRUE),
  "\n\n"
)

# ------------------------------------------------------------
# SPECIFICATIONS
# ------------------------------------------------------------

specs <- list(
  GPRA_SUP = c("GPRA", "SUP"),
  GPRT_DEM = c("GPRT", "DEM"),
  GPRT_PRE = c("GPRT", "PRE"),
  GPRT_SUP = c("GPRT", "SUP"),
  GPRT_VIX = c("GPRT", "VIX"),
  SUP_DEM  = c("SUP", "DEM"),
  SUP_PRE  = c("SUP", "PRE")
)

# ------------------------------------------------------------
# RDS
# ------------------------------------------------------------

rds_dir <- file.path(
  ROOT,
  "results",
  "models",
  "garch_midas"
)

rds_files <- list.files(
  rds_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

cat("RDS :", length(rds_files), "\n\n")

# ------------------------------------------------------------
# EXTRACTION NOM MODELE
# ------------------------------------------------------------

get_model_info <- function(file_name) {

  base <- tools::file_path_sans_ext(
    basename(file_name)
  )

  countries <- unique(
    as.character(
      daily[[daily_country_col]]
    )
  )

  country <- countries[
    sapply(
      countries,
      function(x) startsWith(base, x)
    )
  ][1]

  if (is.na(country))
    stop("Pays introuvable : ", base)

  spec_name <- sub(
    paste0("^", country, "_"),
    "",
    base
  )

  if (!spec_name %in% names(specs)) {

    stop(
      "Specification inconnue : ",
      spec_name
    )
  }

  list(
    model_name = base,
    country = country,
    spec = spec_name,
    vars = specs[[spec_name]]
  )
}

# ------------------------------------------------------------
# DIAGNOSTIC
# ------------------------------------------------------------

diagnose_model <- function(rds_file) {

  model <- readRDS(rds_file)

  info <- get_model_info(rds_file)

  country <- info$country
  spec_name <- info$spec
  vars <- info$vars

  # ----------------------------------------------------------
  # PERIODE DU MODELE
  # ----------------------------------------------------------

  period <- model$period

  if (is.null(period) || length(period) < 2)
    stop("period absent")

  # IMPORTANT :
  # ne pas utiliser as.Date() directement sur un objet
  # potentiellement POSIXlt/numerique.
  period <- to_date_safe(period)

  start_date <- period[1]
  end_date <- period[length(period)]

  # ----------------------------------------------------------
  # DAILY
  # ----------------------------------------------------------

  d <- daily[
    as.character(daily[[daily_country_col]]) == country &
      daily[[daily_date_col]] >= start_date &
      daily[[daily_date_col]] <= end_date,
    ,
    drop = FALSE
  ]

  d <- d[
    order(d[[daily_date_col]]),
    ,
    drop = FALSE
  ]

  r <- suppressWarnings(
    as.numeric(d$.__RETURN__)
  )

  ok <- is.finite(r)

  d <- d[ok, , drop = FALSE]
  r <- r[ok]

  if (length(r) < 100)
    stop("Returns insuffisants : ", length(r))

  # ----------------------------------------------------------
  # MONTHLY
  # ----------------------------------------------------------

  m <- monthly[
    as.character(monthly[[monthly_country_col]]) == country,
    ,
    drop = FALSE
  ]

  m <- m[
    order(m[[monthly_date_col]]),
    ,
    drop = FALSE
  ]

  for (v in vars) {

    if (!v %in% names(m))
      stop("Variable MIDAS absente : ", v)

    m[[v]] <- suppressWarnings(
      as.numeric(m[[v]])
    )
  }

  # ----------------------------------------------------------
  # MEME LOGIQUE QUE LE SCRIPT D'ESTIMATION
  # ----------------------------------------------------------

  common_start <- max(
    min(d[[daily_date_col]], na.rm = TRUE),
    min(m[[monthly_date_col]], na.rm = TRUE)
  )

  common_end <- min(
    max(d[[daily_date_col]], na.rm = TRUE),
    max(m[[monthly_date_col]], na.rm = TRUE)
  )

  K <- 3

  common_month <- as.Date(
    format(common_start, "%Y-%m-01")
  )

  required_start <- seq.Date(
    common_month,
    by = "month",
    length.out = K + 2
  )[K + 2]

  end_month <- as.Date(
    format(common_end, "%Y-%m-01")
  )

  end_exclusive <- seq.Date(
    end_month,
    by = "month",
    length.out = 2
  )[2]

  d2 <- d[
    d[[daily_date_col]] >= required_start &
      d[[daily_date_col]] < end_exclusive,
    ,
    drop = FALSE
  ]

  r2 <- suppressWarnings(
    as.numeric(d2$.__RETURN__)
  )

  m2 <- m[
    m[[monthly_date_col]] >= required_start &
      m[[monthly_date_col]] <= common_end,
    ,
    drop = FALSE
  ]

  m2 <- m2[
    order(m2[[monthly_date_col]]),
    ,
    drop = FALSE
  ]

  # ----------------------------------------------------------
  # MIDAS
  # ----------------------------------------------------------

  mv1 <- rumidas::mv_into_mat(
    r2,
    m2[[vars[1]]],
    K = K,
    type = "monthly"
  )

  mv2 <- rumidas::mv_into_mat(
    r2,
    m2[[vars[2]]],
    K = K,
    type = "monthly"
  )

  # ----------------------------------------------------------
  # COEFFICIENTS
  # ----------------------------------------------------------

  coef_mat <- model$rob_coef_mat

  est <- suppressWarnings(
    as.numeric(coef_mat[, "Estimate"])
  )

  names(est) <- rownames(coef_mat)

  get_coef <- function(nm) {

    if (nm %in% names(est))
      return(est[[nm]])

    hit <- grep(
      paste0("^", nm, "$"),
      names(est),
      ignore.case = TRUE,
      value = TRUE
    )

    if (length(hit))
      return(est[[hit[1]]])

    NA_real_
  }

  alpha  <- get_coef("alpha")
  beta   <- get_coef("beta")
  m0     <- get_coef("m")
  theta1 <- get_coef("theta_1")
  w21    <- get_coef("w2_1")
  theta2 <- get_coef("theta_2")
  w22    <- get_coef("w2_2")

  param <- c(
    alpha,
    beta,
    m0,
    theta1,
    w21,
    theta2,
    w22
  )

  if (any(!is.finite(param)))
    stop("Coefficients non finis")

  # ----------------------------------------------------------
  # FONCTION EXACTE RUMIDAS
  # ----------------------------------------------------------

  cond_fun <- get(
    "GM_2M_cond_vol_no_skew",
    envir = asNamespace("rumidas")
  )

  sigma <- cond_fun(
    param = param,
    daily_ret = r2,
    mv_m_1 = mv1,
    mv_m_2 = mv2,
    K_1 = K,
    K_2 = K,
    lag_fun = "Beta"
  )

  sigma <- as.numeric(sigma)

  n <- min(
    length(r2),
    length(sigma)
  )

  rr <- r2[seq_len(n)]
  ss <- sigma[seq_len(n)]

  ok <- is.finite(rr) &
    is.finite(ss) &
    ss > 0

  rr <- rr[ok]
  ss <- ss[ok]

  z <- rr / ss

  z <- z[is.finite(z)]

  if (length(z) < 100)
    stop("Residus insuffisants")

  # ----------------------------------------------------------
  # TESTS
  # ----------------------------------------------------------

  lb1 <- Box.test(
    z,
    lag = 12,
    type = "Ljung-Box"
  )

  lb2 <- Box.test(
    z^2,
    lag = 12,
    type = "Ljung-Box"
  )

  # ARCH-LM
  lag_arch <- 12
  zz <- z^2

  Y <- zz[(lag_arch + 1):length(zz)]

  X <- do.call(
    cbind,
    lapply(
      1:lag_arch,
      function(j)
        zz[
          (lag_arch + 1 - j):
            (length(zz) - j)
        ]
    )
  )

  X <- cbind(
    Intercept = 1,
    X
  )

  fit_arch <- lm(
    Y ~ X - 1
  )

  R2 <- summary(fit_arch)$r.squared

  LM <- length(Y) * R2

  ARCH_p <- pchisq(
    LM,
    df = lag_arch,
    lower.tail = FALSE
  )

  # ----------------------------------------------------------
  # STATISTIQUES
  # ----------------------------------------------------------

  mu <- mean(z)
  sdz <- sd(z)

  skew <- mean(
    (z - mu)^3
  ) / sdz^3

  kurt <- mean(
    (z - mu)^4
  ) / sdz^4

  # ----------------------------------------------------------
  # IC
  # ----------------------------------------------------------

  loglik <- suppressWarnings(
    as.numeric(model$loglik)
  )

  AICv <- NA_real_
  BICv <- NA_real_

  if (!is.null(model$inf_criteria)) {

    ic <- model$inf_criteria

    if ("AIC" %in% names(ic))
      AICv <- as.numeric(ic["AIC"])

    if ("BIC" %in% names(ic))
      BICv <- as.numeric(ic["BIC"])
  }

  # ----------------------------------------------------------
  # DIAGNOSTIC
  # ----------------------------------------------------------

  diagnostic <- if (
    lb2$p.value >= 0.05 &&
    ARCH_p >= 0.05
  ) {

    "Pas de dependance ARCH residuelle detectee"

  } else if (
    lb2$p.value < 0.05 &&
    ARCH_p < 0.05
  ) {

    "Dependance des carres + ARCH residuel"

  } else if (
    lb2$p.value < 0.05
  ) {

    "Dependance residuelle des carres"

  } else {

    "Effet ARCH residuel"
  }

  data.frame(
    Pays = country,
    Specification = spec_name,
    Modele = info$model_name,

    N = length(z),

    LB12_residus_stat =
      as.numeric(lb1$statistic),

    LB12_residus_p =
      as.numeric(lb1$p.value),

    LB12_residus2_stat =
      as.numeric(lb2$statistic),

    LB12_residus2_p =
      as.numeric(lb2$p.value),

    ARCH12_stat = LM,
    ARCH12_p = ARCH_p,

    Mean_residus = mu,
    SD_residus = sdz,
    Min_residus = min(z),
    Median_residus = median(z),
    Max_residus = max(z),
    Skewness_residus = skew,
    Kurtosis_residus = kurt,

    LogLik = loglik,
    AIC = AICv,
    BIC = BICv,

    Diagnostic = diagnostic,

    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# BOUCLE
# ------------------------------------------------------------

results <- list()
errors <- list()

for (i in seq_along(rds_files)) {

  f <- rds_files[i]

  cat(
    sprintf(
      "[%02d/%02d] %s ... ",
      i,
      length(rds_files),
      basename(f)
    )
  )

  ans <- tryCatch(
    diagnose_model(f),
    error = function(e) e
  )

  if (inherits(ans, "error")) {

    cat("ERREUR\n")

    errors[[length(errors) + 1]] <- data.frame(
      Fichier = basename(f),
      Erreur = conditionMessage(ans),
      stringsAsFactors = FALSE
    )

  } else {

    cat("OK\n")

    results[[length(results) + 1]] <- ans
  }
}

# ------------------------------------------------------------
# RESULTATS
# ------------------------------------------------------------

diag <- if (length(results))
  bind_rows(results)
else
  data.frame()

err <- if (length(errors))
  bind_rows(errors)
else
  data.frame(
    Fichier = character(),
    Erreur = character()
  )

# ------------------------------------------------------------
# CSV
# ------------------------------------------------------------

write_csv(
  diag,
  file.path(
    OUT,
    "14_DIAGNOSTICS_POST_ESTIMATION_76.csv"
  )
)

write_csv(
  err,
  file.path(
    OUT,
    "14_DIAGNOSTICS_POST_ESTIMATION_ERREURS.csv"
  )
)

# ------------------------------------------------------------
# CONTROLE
# ------------------------------------------------------------

control <- data.frame(
  Element = c(
    "RDS total",
    "Diagnostics reussis",
    "Erreurs",
    "LB residus calcule",
    "LB residus2 calcule",
    "ARCH-LM calcule",
    "LogLik disponible",
    "AIC disponible",
    "BIC disponible"
  ),

  Valeur = c(
    length(rds_files),
    nrow(diag),
    nrow(err),

    if (nrow(diag)) sum(is.finite(diag$LB12_residus_p)) else 0,
    if (nrow(diag)) sum(is.finite(diag$LB12_residus2_p)) else 0,
    if (nrow(diag)) sum(is.finite(diag$ARCH12_p)) else 0,
    if (nrow(diag)) sum(is.finite(diag$LogLik)) else 0,
    if (nrow(diag)) sum(is.finite(diag$AIC)) else 0,
    if (nrow(diag)) sum(is.finite(diag$BIC)) else 0
  )
)

write_csv(
  control,
  file.path(
    OUT,
    "14_CONTROLE_DIAGNOSTICS.csv"
  )
)

# ------------------------------------------------------------
# RESUME
# ------------------------------------------------------------

if (nrow(diag)) {

  resume <- data.frame(

    Test = c(
      "Ljung-Box residus",
      "Ljung-Box residus2",
      "ARCH-LM"
    ),

    N = c(
      sum(is.finite(diag$LB12_residus_p)),
      sum(is.finite(diag$LB12_residus2_p)),
      sum(is.finite(diag$ARCH12_p))
    ),

    Rejet_5pct = c(
      sum(diag$LB12_residus_p < 0.05, na.rm = TRUE),
      sum(diag$LB12_residus2_p < 0.05, na.rm = TRUE),
      sum(diag$ARCH12_p < 0.05, na.rm = TRUE)
    ),

    Non_rejet_5pct = c(
      sum(diag$LB12_residus_p >= 0.05, na.rm = TRUE),
      sum(diag$LB12_residus2_p >= 0.05, na.rm = TRUE),
      sum(diag$ARCH12_p >= 0.05, na.rm = TRUE)
    )
  )

} else {

  resume <- data.frame(
    Test = character(),
    N = integer(),
    Rejet_5pct = integer(),
    Non_rejet_5pct = integer()
  )
}

write_csv(
  resume,
  file.path(
    OUT,
    "14_RESUME_DIAGNOSTICS.csv"
  )
)

# ------------------------------------------------------------
# EXCEL
# ------------------------------------------------------------

xlsx <- file.path(
  OUT,
  "14_DIAGNOSTICS_POST_ESTIMATION_76.xlsx"
)

wb <- createWorkbook()

addWorksheet(
  wb,
  "Diagnostics_76"
)

writeData(
  wb,
  "Diagnostics_76",
  diag
)

addWorksheet(
  wb,
  "Resume"
)

writeData(
  wb,
  "Resume",
  resume
)

addWorksheet(
  wb,
  "Controle"
)

writeData(
  wb,
  "Controle",
  control
)

addWorksheet(
  wb,
  "Erreurs"
)

writeData(
  wb,
  "Erreurs",
  err
)

saveWorkbook(
  wb,
  xlsx,
  overwrite = TRUE
)

# ------------------------------------------------------------
# RAPPORT
# ------------------------------------------------------------

report <- file.path(
  OUT,
  "14_RAPPORT_DIAGNOSTICS.txt"
)

sink(report)

cat("============================================================\n")
cat(" RIMEH - RAPPORT DIAGNOSTICS POST-ESTIMATION\n")
cat("============================================================\n\n")

cat(
  "RDS total : ",
  length(rds_files),
  "\n",
  sep = ""
)

cat(
  "Diagnostics reussis : ",
  nrow(diag),
  "\n",
  sep = ""
)

cat(
  "Erreurs : ",
  nrow(err),
  "\n\n",
  sep = ""
)

if (nrow(diag)) {

  cat(
    "LB residus rejet 5% : ",
    sum(diag$LB12_residus_p < 0.05, na.rm = TRUE),
    " / ",
    nrow(diag),
    "\n",
    sep = ""
  )

  cat(
    "LB residus2 rejet 5% : ",
    sum(diag$LB12_residus2_p < 0.05, na.rm = TRUE),
    " / ",
    nrow(diag),
    "\n",
    sep = ""
  )

  cat(
    "ARCH-LM rejet 5% : ",
    sum(diag$ARCH12_p < 0.05, na.rm = TRUE),
    " / ",
    nrow(diag),
    "\n\n",
    sep = ""
  )

  cat(
    "Median SD residus : ",
    median(diag$SD_residus, na.rm = TRUE),
    "\n"
  )
}

if (nrow(err)) {

  cat("\nERREURS :\n\n")

  for (i in seq_len(nrow(err))) {

    cat(
      err$Fichier[i],
      " : ",
      err$Erreur[i],
      "\n",
      sep = ""
    )
  }
}

sink()

cat("\n============================================================\n")
cat(" DIAGNOSTICS TERMINES\n")
cat("============================================================\n")
cat("RDS :", length(rds_files), "\n")
cat("OK :", nrow(diag), "\n")
cat("ERREURS :", nrow(err), "\n")
cat("============================================================\n")