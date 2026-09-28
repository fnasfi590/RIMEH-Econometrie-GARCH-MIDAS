options(stringsAsFactors=FALSE)
options(warn=1)

cat("\n====================================================\n")
cat(" RIMEH PART 3 - REPAIR GARCH-MIDAS FINAL\n")
cat("====================================================\n\n")

root <- normalizePath(".", winslash="/")
dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)
dir.create("results/models/garch_midas_repaired", recursive=TRUE, showWarnings=FALSE)
dir.create("results/validation", recursive=TRUE, showWarnings=FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(lubridate)
})

if (!requireNamespace("rugarch", quietly=TRUE)) {
  stop("Package rugarch manquant.")
}

if (!requireNamespace("rumidas", quietly=TRUE)) {
  stop("Package rumidas manquant.")
}

cat("R version :", R.version.string, "\n")
cat("rumidas   :", as.character(packageVersion("rumidas")), "\n")
cat("rugarch   :", as.character(packageVersion("rugarch")), "\n\n")

# ============================================================
# 1. CHARGEMENT DES DONNEES
# ============================================================

model_file <- "data/processed/monthly_model_data_FINAL.csv"
daily_file <- "data/processed/daily_clean.csv"

if (!file.exists(model_file))
  stop("Fichier absent : ", model_file)

if (!file.exists(daily_file))
  stop("Fichier absent : ", daily_file)

monthly <- fread(model_file)
daily   <- fread(daily_file)

cat("Monthly rows :", nrow(monthly), "\n")
cat("Daily rows   :", nrow(daily), "\n\n")

# ------------------------------------------------------------
# normalisation des dates
# ------------------------------------------------------------

find_date_col <- function(x) {
  cc <- names(x)
  z <- cc[tolower(cc) %in% c("date","month","datetime","day")]
  if (length(z)>0) z[1] else NA_character_
}

dc_m <- find_date_col(monthly)
dc_d <- find_date_col(daily)

if (is.na(dc_m)) stop("Colonne date introuvable dans monthly.")
if (is.na(dc_d)) stop("Colonne date introuvable dans daily.")

monthly[, date := as.Date(get(dc_m))]
daily[, date := as.Date(get(dc_d))]

# ============================================================
# 2. VERIFICATION / RECONSTRUCTION DE RV
# ============================================================

if (!"RV" %in% names(monthly)) {
  cat("RV absente du fichier monthly_model_data_FINAL.csv\n")
  cat("Reconstruction de RV depuis daily_clean.csv...\n")

  # chercher la colonne rendement
  ret_candidates <- c(
    "return","returns","ret","daily_return",
    "log_return","logret","R","r"
  )

  rc <- ret_candidates[ret_candidates %in% names(daily)]

  if (length(rc)==0) {
    # recherche automatique
    numeric_cols <- names(daily)[sapply(daily, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, c("year","month","day"))
    if (length(numeric_cols)>0) rc <- numeric_cols[1]
  }

  if (length(rc)==0)
    stop("Impossible de trouver la colonne des rendements dans daily_clean.csv")

  ret_col <- rc[1]

  # trouver pays
  country_col <- names(daily)[tolower(names(daily)) %in%
                                c("country","iso","iso3","country_code")]

  if (length(country_col)==0)
    stop("Colonne country/ISO absente dans daily_clean.csv")

  country_col <- country_col[1]

  dtmp <- daily %>%
    transmute(
      country = as.character(.data[[country_col]]),
      date = as.Date(date),
      ret = as.numeric(.data[[ret_col]])
    ) %>%
    filter(!is.na(country), !is.na(date), is.finite(ret))

  rv <- dtmp %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(country, month) %>%
    summarise(
      RV = sum(ret^2, na.rm=TRUE),
      .groups="drop"
    ) %>%
    rename(date=month)

  monthly <- monthly %>%
    select(-any_of("RV")) %>%
    left_join(rv, by=c("country","date"))
}

monthly$RV <- as.numeric(monthly$RV)

cat("RV valid :", sum(is.finite(monthly$RV)), "/", nrow(monthly), "\n\n")

# ============================================================
# 3. VARIABLES NECESSAIRES
# ============================================================

needed <- c(
  "country","date","GPRT","GPRA",
  "SUP","DEM","PRE","VIX","RV"
)

missing_cols <- setdiff(needed, names(monthly))

if (length(missing_cols)>0) {
  stop(
    "Variables manquantes dans monthly_model_data_FINAL.csv : ",
    paste(missing_cols, collapse=", ")
  )
}

monthly <- monthly %>%
  mutate(
    country=as.character(country),
    GPRT=as.numeric(GPRT),
    GPRA=as.numeric(GPRA),
    SUP=as.numeric(SUP),
    DEM=as.numeric(DEM),
    PRE=as.numeric(PRE),
    VIX=as.numeric(VIX),
    RV=as.numeric(RV)
  ) %>%
  arrange(country,date)

# ============================================================
# 4. VERIFICATION DES PAYS ELIGIBLES
# ============================================================

vars_midas <- c("GPRT","GPRA","SUP","DEM","PRE","VIX")

elig <- lapply(sort(unique(monthly$country)), function(cc) {

  z <- monthly %>%
    filter(country==cc)

  ok <- sapply(vars_midas, function(v) {
    sum(is.finite(z[[v]])) >= 30
  })

  data.frame(
    country=cc,
    n=nrow(z),
    RV=sum(is.finite(z$RV)),
    GPRT=sum(is.finite(z$GPRT)),
    GPRA=sum(is.finite(z$GPRA)),
    SUP=sum(is.finite(z$SUP)),
    DEM=sum(is.finite(z$DEM)),
    PRE=sum(is.finite(z$PRE)),
    VIX=sum(is.finite(z$VIX)),
    eligible=all(ok) &&
             sum(is.finite(z$RV)) >= 30
  )
}) %>% bind_rows()

write.csv(
  elig,
  "results/validation/GARCH_MIDAS_REPAIRED_ELIGIBILITY.csv",
  row.names=FALSE
)

print(elig)

countries <- elig$country[elig$eligible]

cat("\nPays eligibles GARCH-MIDAS :", length(countries), "\n")
cat(paste(countries, collapse=", "), "\n\n")

# ============================================================
# 5. PREPARATION DES RENDEMENTS JOURNALIERS
# ============================================================

country_col_d <- names(daily)[tolower(names(daily)) %in%
                                c("country","iso","iso3","country_code")]

if (length(country_col_d)==0)
  stop("Colonne pays absente du fichier daily.")

country_col_d <- country_col_d[1]

ret_candidates <- c(
  "return","returns","ret","daily_return",
  "log_return","logret","R","r"
)

rc <- ret_candidates[ret_candidates %in% names(daily)]

if (length(rc)==0) {
  numeric_cols <- names(daily)[sapply(daily, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c("year","month","day"))
  if (length(numeric_cols)>0) rc <- numeric_cols[1]
}

if (length(rc)==0)
  stop("Rendement journalier introuvable.")

ret_col <- rc[1]

daily2 <- daily %>%
  transmute(
    country=as.character(.data[[country_col_d]]),
    date=as.Date(date),
    ret=as.numeric(.data[[ret_col]])
  ) %>%
  filter(
    country %in% countries,
    is.finite(ret),
    !is.na(date)
  ) %>%
  arrange(country,date)

# ============================================================
# 6. FONCTION GARCH-MIDAS REPAIREE
# ============================================================

run_one_gm <- function(cc, mv_name, K=3) {

  cat("\n----------------------------------------------------\n")
  cat("Country :", cc, "\n")
  cat("MIDAS   :", mv_name, "\n")
  cat("K       :", K, "\n")
  cat("----------------------------------------------------\n")

  dm <- monthly %>%
    filter(country==cc) %>%
    select(date, all_of(mv_name)) %>%
    arrange(date) %>%
    filter(is.finite(.data[[mv_name]]))

  dd <- daily2 %>%
    filter(country==cc) %>%
    arrange(date) %>%
    filter(is.finite(ret))

  if (nrow(dm) < (K+10)) {
    stop("Pas assez d'observations mensuelles.")
  }

  if (nrow(dd) < 500) {
    stop("Pas assez d'observations journalières.")
  }

  # ----------------------------------------------------------
  # POINT CRITIQUE :
  # rumidas exige que l'historique mensuel commence au moins
  # K+1 périodes avant le premier rendement journalier.
  #
  # On NE FABRIQUE aucune observation.
  # On réduit simplement l'échantillon journalier au début
  # compatible avec l'historique MIDAS disponible.
  # ----------------------------------------------------------

  first_m <- min(dm$date)
  first_d <- min(dd$date)

  required_start <- first_m %m+% months(K+1)

  actual_start <- max(first_d, required_start)

  dd2 <- dd %>%
    filter(date >= actual_start)

  if (nrow(dd2) < 300)
    stop("Après alignement MIDAS, trop peu de rendements journaliers.")

  # rendre les dates mensuelles cohérentes
  dm <- dm %>%
    mutate(month=floor_date(date,"month"))

  dd2 <- dd2 %>%
    mutate(month=floor_date(date,"month"))

  # seulement les mois communs
  common_months <- intersect(
    unique(dd2$month),
    unique(dm$month)
  )

  if (length(common_months) < K+10)
    stop("Pas assez de mois communs après alignement.")

  dm <- dm %>%
    filter(month %in% common_months) %>%
    arrange(month)

  dd2 <- dd2 %>%
    filter(month %in% common_months) %>%
    arrange(date)

  # vector MIDAS
  mv <- dm[[mv_name]]

  if (length(mv) < K+10)
    stop("Vecteur MIDAS trop court.")

  # ----------------------------------------------------------
  # construction matrice MIDAS
  # ----------------------------------------------------------

  mv_mat <- tryCatch(
    rumidas::mv_into_mat(
      x=dd2$ret,
      mv=mv,
      K=K,
      type="monthly"
    ),
    error=function(e) {
      stop("mv_into_mat : ", conditionMessage(e))
    }
  )

  # ----------------------------------------------------------
  # estimation GM
  # ----------------------------------------------------------

  fit <- tryCatch(
    rumidas::ugmfit(
      model="GM",
      skew="NO",
      distribution="std",
      daily_ret=dd2$ret,
      mv_m=mv_mat,
      K=K,
      lag_fun="Beta",
      R=20
    ),
    error=function(e) {
      stop("ugmfit : ", conditionMessage(e))
    }
  )

  outfile <- paste0(
    "results/models/garch_midas_repaired/",
    cc,"_GM_",mv_name,".rds"
  )

  saveRDS(fit,outfile)

  # capture summary
  txt <- capture.output(print(fit))

  txtfile <- paste0(
    "results/tables/GARCH_MIDAS_REPAIRED_",
    cc,"_",mv_name,".txt"
  )

  writeLines(txt,txtfile)

  data.frame(
    country=cc,
    variable=mv_name,
    K=K,
    daily_obs=nrow(dd2),
    monthly_obs=length(mv),
    status="SUCCESS",
    error=""
  )
}

# ============================================================
# 7. LANCEMENT DES GARCH-MIDAS
# ============================================================

audit <- list()

for (cc in countries) {

  # On commence par les variables individuelles.
  # Cela évite les matrices singulières des modèles 2 variables.
  for (v in vars_midas) {

    res <- tryCatch(
      run_one_gm(cc,v,K=3),
      error=function(e) {
        cat(
          "FAILED :",cc,v,"=>",
          conditionMessage(e),"\n"
        )

        data.frame(
          country=cc,
          variable=v,
          K=3,
          daily_obs=NA,
          monthly_obs=NA,
          status="FAILED",
          error=conditionMessage(e)
        )
      }
    )

    audit[[length(audit)+1]] <- res
  }
}

audit_df <- bind_rows(audit)

write.csv(
  audit_df,
  "results/tables/garch_midas_repaired_audit.csv",
  row.names=FALSE
)

cat("\n====================================================\n")
cat(" RESULTATS GARCH-MIDAS\n")
cat("====================================================\n")

print(
  audit_df %>%
    count(status)
)

print(
  audit_df %>%
    filter(status=="SUCCESS") %>%
    select(country,variable,K,daily_obs,monthly_obs)
)

# ============================================================
# 8. PANEL LONG-RUN REPAIR
# ============================================================

cat("\n====================================================\n")
cat(" PANEL LONG-RUN REPAIR\n")
cat("====================================================\n")

panel <- monthly %>%
  filter(
    is.finite(RV),
    is.finite(GPRT),
    is.finite(GPRA),
    is.finite(SUP),
    is.finite(DEM),
    is.finite(PRE),
    is.finite(VIX)
  ) %>%
  mutate(
    country=factor(country),
    year=year(date),
    month_num=month(date)
  )

cat("Panel observations :",nrow(panel),"\n")
cat("Countries          :",nlevels(panel$country),"\n")

# interactions
panel <- panel %>%
  mutate(
    GPRT_SUP=GPRT*SUP,
    GPRT_DEM=GPRT*DEM,
    GPRT_PRE=GPRT*PRE
  )

panel_results <- list()

formulas <- list(
  M0 = RV ~ GPRT + GPRA,
  M1 = RV ~ GPRT + GPRA + SUP + DEM + PRE,
  M2 = RV ~ GPRT + GPRA + SUP + DEM + PRE + VIX,
  M3 = RV ~ GPRT + GPRA + SUP + DEM + PRE + VIX +
             GPRT_SUP + GPRT_DEM + GPRT_PRE
)

for (nm in names(formulas)) {

  cat("Panel",nm,"\n")

  fitp <- tryCatch(
    lm(
      formulas[[nm]],
      data=panel
    ),
    error=function(e) NULL
  )

  if (!is.null(fitp)) {

    co <- summary(fitp)$coefficients

    tmp <- data.frame(
      model=nm,
      term=rownames(co),
      estimate=co[,1],
      std_error=co[,2],
      statistic=co[,3],
      p_value=co[,4],
      row.names=NULL
    )

    panel_results[[length(panel_results)+1]] <- tmp
  }
}

panel_coef <- bind_rows(panel_results)

write.csv(
  panel_coef,
  "results/tables/panel_long_run_REPAIRED_coefficients.csv",
  row.names=FALSE
)

saveRDS(
  panel_results,
  "results/models/panel_long_run_REPAIRED.rds"
)

# ============================================================
# 9. RAPPORT FINAL
# ============================================================

success <- sum(audit_df$status=="SUCCESS")
failed  <- sum(audit_df$status=="FAILED")

report <- c(
  "# RIMEH PART 3 - RAPPORT FINAL REPAIRE",
  "",
  paste0("Date execution : ",Sys.time()),
  paste0("Pays eligibles : ",length(countries)),
  paste0("Modeles GARCH-MIDAS reussis : ",success),
  paste0("Modeles GARCH-MIDAS echoues : ",failed),
  "",
  "## GARCH-MIDAS",
  "",
  "Les estimations individuelles utilisent K=3.",
  "Aucune observation mensuelle artificielle n'a ete creee.",
  "Les rendements journaliers sont tronques au debut uniquement lorsque cela est necessaire pour respecter la contrainte d'historique de rumidas.",
  "",
  "## Panel long-run",
  "",
  paste0("Observations utilisees : ",nrow(panel)),
  paste0("Pays : ",nlevels(panel$country)),
  "",
  "Les coefficients du panel sont disponibles dans :",
  "results/tables/panel_long_run_REPAIRED_coefficients.csv",
  "",
  "Les resultats GARCH-MIDAS sont disponibles dans :",
  "results/tables/garch_midas_repaired_audit.csv"
)

writeLines(
  report,
  "results/validation/RIMEH_PART3_REPAIRED_FINAL_REPORT.md"
)

# ============================================================
# 10. RESUME LISIBLE DANS POWERSHELL
# ============================================================

cat("\n\n====================================================\n")
cat(" FINAL SUMMARY\n")
cat("====================================================\n\n")

cat("GARCH-MIDAS SUCCESS :",success,"\n")
cat("GARCH-MIDAS FAILED  :",failed,"\n")
cat("Panel observations  :",nrow(panel),"\n")
cat("Panel coefficients  :",nrow(panel_coef),"\n\n")

cat("FICHIERS CREES :\n")
cat(" - results/tables/garch_midas_repaired_audit.csv\n")
cat(" - results/tables/panel_long_run_REPAIRED_coefficients.csv\n")
cat(" - results/validation/GARCH_MIDAS_REPAIRED_ELIGIBILITY.csv\n")
cat(" - results/validation/RIMEH_PART3_REPAIRED_FINAL_REPORT.md\n")
cat(" - results/models/garch_midas_repaired/\n\n")

cat("====================================================\n")
cat(" FIN PART 3 REPAIR\n")
cat("====================================================\n")
