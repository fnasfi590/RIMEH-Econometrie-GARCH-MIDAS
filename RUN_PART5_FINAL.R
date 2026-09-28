setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")

options(
  repos=c(CRAN="https://cloud.r-project.org"),
  timeout=300,
  warn=1
)

cat("\n============================================================\n")
cat(" RIMEH - PARTIE 5 COMPLETE - EXECUTION FINALE\n")
cat("============================================================\n")

# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

pk <- c(
  "tidyverse","lubridate","readr","data.table",
  "xts","zoo","rugarch","tseries","FinTS",
  "moments","rumidas"
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
  library(data.table)
  library(xts)
  library(zoo)
  library(rugarch)
  library(tseries)
  library(FinTS)
  library(moments)
  library(rumidas)
})

cat("\nR :",R.version.string,"\n")
cat("rumidas :",as.character(packageVersion("rumidas")),"\n")

# ------------------------------------------------------------
# DOSSIERS
# ------------------------------------------------------------

dir.create("results/tables",recursive=TRUE,showWarnings=FALSE)
dir.create("results/models",recursive=TRUE,showWarnings=FALSE)
dir.create("results/validation",recursive=TRUE,showWarnings=FALSE)

# ------------------------------------------------------------
# SAUVEGARDE SCRIPT 08
# ------------------------------------------------------------

f08 <- "scripts/08_garch_midas_models.R"
backup08 <- "scripts/08_garch_midas_models_BACKUP_AUTO.R"

if(file.exists(f08)){
  file.copy(f08,backup08,overwrite=TRUE)
  cat("\nBackup créé :",backup08,"\n")
}

# ------------------------------------------------------------
# PATCH AUTOMATIQUE DE LA PARTIE GM/GM2M
# ------------------------------------------------------------

x <- readLines(f08,warn=FALSE)

start <- which(grepl(
  'args <- list\\(model="GM2M"',
  x,
  fixed=FALSE
))

if(length(start)>0){

  s <- start[1]

  e <- s

  while(e <= length(x)){
    if(grepl(
      "do.call\\(rumidas::ugmfit, args\\)",
      x[e],
      fixed=TRUE
    )){
      break
    }
    e <- e + 1
  }

  if(e <= length(x)){

    replacement <- c(
      "  if (length(mm) == 1) {",
      "",
      "    args <- list(",
      "      model = \"GM\",",
      "      skew = \"NO\",",
      "      distribution = \"std\",",
      "      daily_ret = r,",
      "      mv_m = mm[[1]],",
      "      K = K,",
      "      lag_fun = \"Beta\",",
      "      R = 50",
      "    )",
      "",
      "  } else if (length(mm) == 2) {",
      "",
      "    args <- list(",
      "      model = \"GM2M\",",
      "      skew = \"NO\",",
      "      distribution = \"std\",",
      "      daily_ret = r,",
      "      mv_m = mm[[1]],",
      "      mv_m_2 = mm[[2]],",
      "      K = K,",
      "      K_2 = K,",
      "      lag_fun = \"Beta\",",
      "      R = 50",
      "    )",
      "",
      "  } else {",
      "",
      "    stop(\"Nombre de variables MIDAS invalide : \",length(mm))",
      "",
      "  }",
      "",
      "  do.call(rumidas::ugmfit,args)"
    )

    x <- c(
      x[seq_len(s-1)],
      replacement,
      x[(e+1):length(x)]
    )

    writeLines(x,f08,useBytes=TRUE)

    cat("\n>>> PATCH GM/GM2M APPLIQUE\n")
    cat(">>> GM pour 1 variable\n")
    cat(">>> GM2M + K_2 pour 2 variables\n")

  } else {
    cat("\n>>> Bloc GM2M non trouvé : aucun patch appliqué.\n")
  }

} else {
  cat("\n>>> Bloc GM2M déjà corrigé ou structure différente.\n")
}

# ------------------------------------------------------------
# VERIFICATION DONNEES
# ------------------------------------------------------------

f <- "data/processed/monthly_model_data_FINAL.csv"

if(!file.exists(f)){
  stop("monthly_model_data_FINAL.csv introuvable.")
}

d <- read_csv(f,show_col_types=FALSE)

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

missing_cols <- setdiff(required,names(d))

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

cat("\n============================================================\n")
cat(" ELIGIBILITE\n")
cat("============================================================\n")

print(eligibility)

cat(
  "\nPays eligibles : ",
  length(valid),
  "/11\n",
  sep=""
)

if(length(valid)!=11){
  stop("Les 11 pays ne sont pas tous eligibles.")
}

# ------------------------------------------------------------
# ETAPE 1
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 1/5 - PREPARATION PART3\n")
cat("============================================================\n")

source(
  "scripts/06_prepare_part3_data.R",
  local=.GlobalEnv
)

cat("\n>>> ETAPE 1 TERMINEE\n")

# ------------------------------------------------------------
# ETAPE 2
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
# ETAPE 3
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 3/5 - GARCH-MIDAS CORRIGE\n")
cat("============================================================\n")

tryCatch({

  source(
    "scripts/08_garch_midas_models.R",
    local=.GlobalEnv
  )

  cat("\n>>> ETAPE 3 TERMINEE\n")

},error=function(e){

  cat(
    "\n!!! ERREUR GARCH-MIDAS !!!\n",
    conditionMessage(e),
    "\n"
  )

  cat(
    "\nLe traitement continue vers les étapes suivantes.\n"
  )

})

# ------------------------------------------------------------
# ETAPE 4
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 4/5 - PANEL LONG-RUN\n")
cat("============================================================\n")

tryCatch({

  source(
    "scripts/09_panel_long_run.R",
    local=.GlobalEnv
  )

  cat("\n>>> ETAPE 4 TERMINEE\n")

},error=function(e){

  cat(
    "\n!!! ERREUR PANEL LONG-RUN !!!\n",
    conditionMessage(e),
    "\n"
  )

})

# ------------------------------------------------------------
# ETAPE 5
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" ETAPE 5/5 - ROBUSTESSE\n")
cat("============================================================\n")

tryCatch({

  source(
    "scripts/10_robustness.R",
    local=.GlobalEnv
  )

  cat("\n>>> ETAPE 5 TERMINEE\n")

},error=function(e){

  cat(
    "\n!!! ERREUR ROBUSTESSE !!!\n",
    conditionMessage(e),
    "\n"
  )

})

# ------------------------------------------------------------
# INVENTAIRE DES RESULTATS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" INVENTAIRE DES RESULTATS\n")
cat("============================================================\n")

dirs <- c(
  "results/tables",
  "results/models",
  "results/validation"
)

for(dd in dirs){

  cat("\n--- ",dd," ---\n",sep="")

  if(dir.exists(dd)){

    ff <- list.files(
      dd,
      recursive=TRUE,
      full.names=FALSE
    )

    if(length(ff)>0){
      print(ff)
    } else {
      cat("Aucun fichier.\n")
    }

  }
}

# ------------------------------------------------------------
# RAPPORT FINAL
# ------------------------------------------------------------

report <- c(
  "# RIMEH - PARTIE 5 - RESULTAT FINAL",
  "",
  paste0("Date : ",Sys.time()),
  "",
  "## Echantillon",
  "",
  paste0("Pays utilisés : ",paste(valid,collapse=", ")),
  "",
  "Nombre de pays : 11",
  "",
  "Echantillon non équilibré.",
  "",
  "Pays exclus : ARE, NGA, KAZ, EGY, POL.",
  "",
  "## Méthodologie",
  "",
  "GARCH complet.",
  "GARCH-MIDAS avec rumidas.",
  "Panel long-run.",
  "Tests de robustesse.",
  "",
  "## Integrité",
  "",
  "Aucune observation financière n'a été inventée.",
  "Aucun coefficient n'a été inventé.",
  "Aucune significativité n'a été inventée.",
  "",
  "Les coefficients, erreurs standards et p-values doivent être lus uniquement",
  "dans les fichiers de résultats effectivement produits par les estimations.",
  "",
  "## Fichiers",
  "",
  "results/tables/",
  "results/models/",
  "results/validation/"
)

writeLines(
  report,
  "results/validation/PART5_FINAL_REPORT.md"
)

cat("\n============================================================\n")
cat(" PARTIE 5 - FIN DU TRAITEMENT\n")
cat("============================================================\n")

cat("\nRapport :\n")
cat(
  normalizePath(
    "results/validation/PART5_FINAL_REPORT.md",
    winslash="\\",
    mustWork=FALSE
  ),
  "\n"
)

cat("\nIMPORTANT : les erreurs éventuelles des modèles individuels sont affichées\n")
cat("ci-dessus. Elles ne doivent pas être remplacées par des coefficients inventés.\n")
