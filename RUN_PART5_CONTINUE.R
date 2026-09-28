setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")

options(
  repos=c(CRAN="https://cloud.r-project.org"),
  timeout=180,
  warn=1
)

cat("\n============================================================\n")
cat(" RIMEH - CONTINUATION PARTIE 5\n")
cat("============================================================\n")

# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

pk <- c(
  "tidyverse",
  "lubridate",
  "readr",
  "data.table",
  "xts",
  "zoo",
  "rugarch",
  "tseries",
  "FinTS",
  "moments",
  "rumidas"
)

for(p in pk){
  if(!requireNamespace(p,quietly=TRUE)){
    cat("Installation :",p,"\n")
    install.packages(p,quiet=TRUE)
  }
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
})

# ------------------------------------------------------------
# VERIFICATION DES DONNEES PREPAREES
# ------------------------------------------------------------

f <- "data/processed/monthly_model_data_FINAL.csv"

if(!file.exists(f)){
  stop(
    "monthly_model_data_FINAL.csv introuvable."
  )
}

d <- read_csv(
  f,
  show_col_types=FALSE
)

target <- c(
  "USA","SAU","NOR","MEX",
  "TUR","PAK","IND","KOR",
  "JPN","DEU","GBR"
)

d <- d |>
  mutate(date=as.Date(date)) |>
  filter(country %in% target)

required <- c(
  "country","date","RV",
  "GPRT","GPRA",
  "SUP","DEM","PRE","VIX"
)

missing_cols <- setdiff(
  required,
  names(d)
)

if(length(missing_cols)>0){
  stop(
    paste(
      "Colonnes manquantes :",
      paste(missing_cols,collapse=", ")
    )
  )
}

eligibility <- d |>
  group_by(country) |>
  summarise(
    RV=sum(is.finite(RV)),
    GPRT=sum(is.finite(GPRT)),
    GPRA=sum(is.finite(GPRA)),
    SUP=sum(is.finite(SUP)),
    DEM=sum(is.finite(DEM)),
    PRE=sum(is.finite(PRE)),
    VIX=sum(is.finite(VIX)),
    .groups="drop"
  ) |>
  mutate(
    eligible=
      RV>0 &
      GPRT>0 &
      GPRA>0 &
      SUP>0 &
      DEM>0 &
      PRE>0 &
      VIX>0
  )

valid <- eligibility |>
  filter(eligible) |>
  pull(country)

cat(
  "\nPays eligibles : ",
  length(valid),
  "/11\n",
  sep=""
)

cat(
  paste(valid,collapse=", "),
  "\n"
)

if(length(valid)!=11){
  print(eligibility)
  stop(
    "Les 11 pays ne sont plus tous eligibles."
  )
}

# ------------------------------------------------------------
# REPREPARATION PART3
# ------------------------------------------------------------

dir.create(
  "results/tables",
  recursive=TRUE,
  showWarnings=FALSE
)

dir.create(
  "results/models",
  recursive=TRUE,
  showWarnings=FALSE
)

dir.create(
  "results/validation",
  recursive=TRUE,
  showWarnings=FALSE
)

cat("\n============================================================\n")
cat(" ETAPE 1/5 - PREPARATION\n")
cat("============================================================\n")

source(
  "scripts/06_prepare_part3_data.R",
  local=.GlobalEnv
)

cat("\n>>> ETAPE 1 TERMINEE\n")

# ------------------------------------------------------------
# GARCH FULL
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 2/5 - GARCH COMPLET\n")
cat("============================================================\n")

source(
  "scripts/07_garch_full.R",
  local=.GlobalEnv
)

cat("\n>>> ETAPE 2 TERMINEE\n")

# ------------------------------------------------------------
# GARCH-MIDAS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 3/5 - GARCH-MIDAS\n")
cat("============================================================\n")

source(
  "scripts/08_garch_midas_models.R",
  local=.GlobalEnv
)

cat("\n>>> ETAPE 3 TERMINEE\n")

# ------------------------------------------------------------
# PANEL LONG-RUN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 4/5 - PANEL LONG-RUN\n")
cat("============================================================\n")

source(
  "scripts/09_panel_long_run.R",
  local=.GlobalEnv
)

cat("\n>>> ETAPE 4 TERMINEE\n")

# ------------------------------------------------------------
# ROBUSTESSE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 5/5 - ROBUSTESSE\n")
cat("============================================================\n")

source(
  "scripts/10_robustness.R",
  local=.GlobalEnv
)

cat("\n>>> ETAPE 5 TERMINEE\n")

# ------------------------------------------------------------
# RAPPORT
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" PARTIE 5 TERMINEE\n")
cat("============================================================\n")

report <- c(
  "# RIMEH - PARTIE 5 - RESULTAT FINAL",
  "",
  paste0("Date : ",Sys.time()),
  "",
  "Pays utilisés :",
  paste(valid,collapse=", "),
  "",
  "Nombre de pays : 11",
  "",
  "Echantillon non équilibré.",
  "",
  "Pays exclus : ARE, NGA, KAZ, EGY, POL.",
  "",
  "Aucune observation financière n'a été inventée.",
  "Aucun coefficient n'a été inventé.",
  "Aucune significativité n'a été inventée.",
  "",
  "Etapes exécutées :",
  "1. Préparation des données",
  "2. GARCH",
  "3. GARCH-MIDAS",
  "4. Panel long-run",
  "5. Robustesse"
)

writeLines(
  report,
  "results/validation/PART5_FINAL_REPORT.md"
)

cat("\nRésultats disponibles dans :\n")
cat("results/tables\n")
cat("results/models\n")
cat("results/validation\n")

cat("\n============================================================\n")
cat(" FIN DU TRAITEMENT\n")
cat("============================================================\n")