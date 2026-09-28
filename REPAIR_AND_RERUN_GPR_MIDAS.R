options(stringsAsFactors=FALSE)
options(warn=1)

cat("\n============================================================\n")
cat(" RIMEH - REPARATION GPR + GARCH-MIDAS\n")
cat("============================================================\n\n")

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(readxl)
  library(xts)
  library(rumidas)
})

dir.create("data/raw", recursive=TRUE, showWarnings=FALSE)
dir.create("data/processed", recursive=TRUE, showWarnings=FALSE)
dir.create("results/models/garch_midas", recursive=TRUE, showWarnings=FALSE)
dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)
dir.create("results/validation", recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. RECUPERATION AUTOMATIQUE DU VRAI LIEN GPR
# ============================================================

cat("\n[1/8] Recherche du fichier GPR officiel...\n")

page_url <- "https://www.matteoiacoviello.com/gpr.htm"

html <- tryCatch(
  paste(readLines(page_url, warn=FALSE, encoding="UTF-8"), collapse="\n"),
  error=function(e) stop("Impossible de lire la page GPR : ", conditionMessage(e))
)

# Cherche tous les fichiers mensuels Excel présents dans la page
hits <- unique(
  regmatches(
    html,
    gregexpr(
      "https?://[^\"'<> ]*data_gpr_export_[0-9]{6}\\.xls",
      html,
      perl=TRUE
    )
  )[[1]]
)

if(length(hits)==0){
  # deuxième méthode : chercher simplement les href contenant data_gpr_export
  hits <- unique(
    regmatches(
      html,
      gregexpr(
        "data_gpr_export_[0-9]{6}\\.xls",
        html,
        perl=TRUE
      )[[1]]
    )
  )

  hits <- paste0(
    "https://www.matteoiacoviello.com/gpr_files/",
    hits
  )
}

hits <- hits[grepl("data_gpr_export_[0-9]{6}\\.xls$",hits)]

if(length(hits)==0){
  stop("Aucun fichier GPR mensuel n'a été trouvé automatiquement sur la page officielle.")
}

versions <- as.integer(
  sub(
    ".*data_gpr_export_([0-9]{6})\\.xls.*",
    "\\1",
    hits
  )
)

ord <- order(versions, decreasing=TRUE)

hits <- hits[ord]
versions <- versions[ord]

gpr_url <- hits[1]

cat("URL GPR sélectionnée :\n")
cat(gpr_url,"\n")
cat("Version :",versions[1],"\n")

gpr_file <- file.path(
  "data/raw",
  basename(gpr_url)
)

if(!file.exists(gpr_file)){
  download.file(
    gpr_url,
    gpr_file,
    mode="wb",
    quiet=FALSE
  )
}

if(!file.exists(gpr_file)){
  stop("Téléchargement GPR impossible.")
}

cat("Fichier GPR OK :",gpr_file,"\n")

# ============================================================
# 2. LECTURE ET DETECTION DES COLONNES
# ============================================================

cat("\n[2/8] Lecture du fichier GPR...\n")

sheets <- excel_sheets(gpr_file)

cat("Sheets :\n")
print(sheets)

gpr_raw <- read_excel(
  gpr_file,
  sheet=sheets[1],
  .name_repair="minimal"
)

nm <- names(gpr_raw)

cat("\nNombre lignes :",nrow(gpr_raw),"\n")
cat("Nombre colonnes :",ncol(gpr_raw),"\n")

cat("\nColonnes :\n")
print(nm)

normname <- function(z){
  toupper(gsub("[^A-Z0-9]","",z))
}

nn <- normname(nm)

find_col <- function(target){

  target <- normname(target)

  idx <- which(nn==target)

  if(length(idx)>0) return(idx[1])

  idx <- which(grepl(target,nn,fixed=TRUE))

  if(length(idx)>0) return(idx[1])

  NA_integer_
}

date_col <- find_col("DATE")

if(is.na(date_col)){
  date_col <- find_col("MONTH")
}

if(is.na(date_col)){
  date_col <- 1
}

gprt_col <- find_col("GPRT")
gpra_col <- find_col("GPRA")

cat("\nDate column :",nm[date_col],"\n")
cat("GPRT column :",ifelse(is.na(gprt_col),"NOT FOUND",nm[gprt_col]),"\n")
cat("GPRA column :",ifelse(is.na(gpra_col),"NOT FOUND",nm[gpra_col]),"\n")

if(is.na(gprt_col) || is.na(gpra_col)){
  stop("Colonnes GPRT/GPRA introuvables.")
}

# ============================================================
# 3. CONSTRUCTION DU GPR MENSUEL
# ============================================================

cat("\n[3/8] Construction du GPR mensuel...\n")

date_raw <- gpr_raw[[date_col]]

date_gpr <- suppressWarnings(as.Date(date_raw))

if(all(is.na(date_gpr))){

  s <- as.character(date_raw)

  # YYYYMM
  ok <- grepl("^[0-9]{6}$",s)

  date_gpr[ok] <- as.Date(
    paste0(
      substr(s[ok],1,4),
      "-",
      substr(s[ok],5,6),
      "-01"
    )
  )
}

if(all(is.na(date_gpr))){

  s <- as.character(date_raw)

  # Excel serial dates
  num <- suppressWarnings(as.numeric(s))

  ok <- is.finite(num)

  date_gpr[ok] <- as.Date(
    num[ok],
    origin="1899-12-30"
  )
}

if(all(is.na(date_gpr))){
  stop("Impossible de convertir les dates GPR.")
}

gpr <- data.frame(
  date=as.Date(floor_date(date_gpr,"month")),
  GPRT=suppressWarnings(as.numeric(gpr_raw[[gprt_col]])),
  GPRA=suppressWarnings(as.numeric(gpr_raw[[gpra_col]]))
)

gpr <- gpr[
  !is.na(gpr$date) &
  gpr$date >= as.Date("2000-01-01") &
  gpr$date <= as.Date("2025-12-01"),
]

gpr <- gpr[
  !duplicated(gpr$date),
]

gpr <- gpr[order(gpr$date),]

write.csv(
  gpr,
  "data/raw/gpr_monthly_2000_2025.csv",
  row.names=FALSE,
  na=""
)

cat("\nGPR observations :",nrow(gpr),"\n")

cat("\nGPRT summary :\n")
print(summary(gpr$GPRT))

cat("\nGPRA summary :\n")
print(summary(gpr$GPRA))

cat("\nGPRT unique :",length(unique(gpr$GPRT[is.finite(gpr$GPRT)])),"\n")
cat("GPRA unique :",length(unique(gpr$GPRA[is.finite(gpr$GPRA)])),"\n")

if(length(unique(gpr$GPRT[is.finite(gpr$GPRT)]))<=1){
  stop("GPRT est constant : problème dans le fichier GPR.")
}

if(length(unique(gpr$GPRA[is.finite(gpr$GPRA)]))<=1){
  stop("GPRA est constant : problème dans le fichier GPR.")
}

# ============================================================
# 4. REPARATION DE MONTHLY_CLEAN
# ============================================================

cat("\n[4/8] Réparation de monthly_clean.csv...\n")

monthly_file <- "data/processed/monthly_clean.csv"

if(!file.exists(monthly_file)){
  stop("monthly_clean.csv introuvable.")
}

monthly <- fread(
  monthly_file,
  data.table=FALSE
)

monthly$date <- as.Date(monthly$date)

monthly <- monthly %>%
  select(-any_of(c("GPRT","GPRA","GPR_SUP","GPRT_SUP","GPRA_SUP",
                   "GPRT_DEM","GPRT_PRE","GPRT_VIX"))) %>%
  left_join(gpr,by="date")

if(any(is.na(monthly$GPRT))){
  cat("Attention : valeurs GPRT manquantes :",sum(is.na(monthly$GPRT)),"\n")
}

if(any(is.na(monthly$GPRA))){
  cat("Attention : valeurs GPRA manquantes :",sum(is.na(monthly$GPRA)),"\n")
}

write.csv(
  monthly,
  monthly_file,
  row.names=FALSE,
  na=""
)

cat("monthly_clean.csv réparé.\n")

cat("\nVérification USA :\n")

usa <- monthly[
  monthly$country=="USA",
]

print(summary(usa$GPRT))
print(summary(usa$GPRA))

# ============================================================
# 5. RECONSTRUCTION DU MODELE FINAL
# ============================================================

cat("\n[5/8] Reconstruction de monthly_model_data_FINAL.csv...\n")

# On utilise monthly_clean comme base fiable
model <- monthly

# Recalcul des interactions
model$GPR_SUP  <- model$GPRT * model$SUP
model$GPRT_SUP <- model$GPRT * model$SUP
model$GPRA_SUP <- model$GPRA * model$SUP
model$GPRT_DEM <- model$GPRT * model$DEM
model$GPRT_PRE <- model$GPRT * model$PRE
model$GPRT_VIX <- model$GPRT * model$VIX

write.csv(
  model,
  "data/processed/monthly_model_data_FINAL.csv",
  row.names=FALSE,
  na=""
)

cat("monthly_model_data_FINAL.csv créé.\n")
cat("Rows :",nrow(model),"\n")
cat("Countries :",length(unique(model$country)),"\n")

# ============================================================
# 6. TEST RUMIDAS CORRECT AVEC XTS
# ============================================================

cat("\n[6/8] Test rumidas avec objets xts...\n")

results <- list()

countries <- sort(unique(model$country))

for(cc in countries){

  cat("\n--------------------------------------------\n")
  cat("Pays :",cc,"\n")
  cat("--------------------------------------------\n")

  dm <- model[
    model$country==cc,
    ,
    drop=FALSE
  ]

  # daily_clean
  daily_file <- "data/processed/daily_clean.csv"

  if(!file.exists(daily_file)){
    cat("daily_clean absent -> skip\n")
    next
  }

  daily <- fread(
    daily_file,
    data.table=FALSE
  )

  daily$date <- as.Date(daily$date)

  dd <- daily[
    daily$country==cc,
    ,
    drop=FALSE
  ]

  if(nrow(dd)<1000){
    cat("Pas assez de données daily -> skip\n")
    next
  }

  # ----------------------------------------------------------
  # Rendements journaliers
  # ----------------------------------------------------------

  ret_col <- NULL

  candidates <- c(
    "return",
    "ret",
    "returns",
    "daily_return"
  )

  for(z in candidates){
    if(z %in% names(dd)){
      ret_col <- z
      break
    }
  }

  if(is.null(ret_col)){
    cat("Colonne rendement introuvable -> skip\n")
    next
  }

  dd$ret <- as.numeric(dd[[ret_col]])

  dd <- dd[
    is.finite(dd$ret),
  ]

  dm$date <- as.Date(dm$date)

  dm <- dm[
    is.finite(dm$GPRT) &
    is.finite(dm$GPRA),
  ]

  if(nrow(dm)<24){
    cat("Pas assez de mois -> skip\n")
    next
  }

  # ----------------------------------------------------------
  # XTS : OBLIGATOIRE POUR RUMIDAS
  # ----------------------------------------------------------

  daily_xts <- xts(
    dd$ret,
    order.by=as.Date(dd$date)
  )

  gprt_xts <- xts(
    dm$GPRT,
    order.by=as.Date(dm$date)
  )

  gpra_xts <- xts(
    dm$GPRA,
    order.by=as.Date(dm$date)
  )

  # élimine les doublons
  daily_xts <- daily_xts[!duplicated(index(daily_xts))]
  gprt_xts <- gprt_xts[!duplicated(index(gprt_xts))]
  gpra_xts <- gpra_xts[!duplicated(index(gpra_xts))]

  # ----------------------------------------------------------
  # GPRT
  # ----------------------------------------------------------

  K <- 12

  fit_one <- function(){

    mv <- rumidas::mv_into_mat(
      daily_xts,
      gprt_xts,
      K=K,
      type="monthly"
    )

    rumidas::ugmfit(
      model="GM",
      skew="NO",
      distribution="std",
      daily_ret=daily_xts,
      mv_m=mv,
      K=K,
      lag_fun="Beta",
      R=30
    )
  }

  fit <- tryCatch(
    fit_one(),
    error=function(e){
      cat("GPRT -> ECHEC :",conditionMessage(e),"\n")
      NULL
    }
  )

  if(!is.null(fit)){

    out <- file.path(
      "results/models/garch_midas",
      paste0("GARCH_MIDAS_",cc,"_GPRT.rds")
    )

    saveRDS(fit,out)

    sink(
      file.path(
        "results/tables",
        paste0("GARCH_MIDAS_",cc,"_GPRT.txt")
      )
    )

    print(summary(fit))

    sink()

    cat("GPRT -> SUCCESS\n")

    results[[length(results)+1]] <- data.frame(
      country=cc,
      variable="GPRT",
      status="SUCCESS",
      file=out,
      stringsAsFactors=FALSE
    )
  }

  # ----------------------------------------------------------
  # GPRA
  # ----------------------------------------------------------

  fit_two <- tryCatch({

    mv <- rumidas::mv_into_mat(
      daily_xts,
      gpra_xts,
      K=K,
      type="monthly"
    )

    rumidas::ugmfit(
      model="GM",
      skew="NO",
      distribution="std",
      daily_ret=daily_xts,
      mv_m=mv,
      K=K,
      lag_fun="Beta",
      R=30
    )

  },error=function(e){

    cat("GPRA -> ECHEC :",conditionMessage(e),"\n")
    NULL
  })

  if(!is.null(fit_two)){

    out <- file.path(
      "results/models/garch_midas",
      paste0("GARCH_MIDAS_",cc,"_GPRA.rds")
    )

    saveRDS(fit_two,out)

    sink(
      file.path(
        "results/tables",
        paste0("GARCH_MIDAS_",cc,"_GPRA.txt")
      )
    )

    print(summary(fit_two))

    sink()

    cat("GPRA -> SUCCESS\n")

    results[[length(results)+1]] <- data.frame(
      country=cc,
      variable="GPRA",
      status="SUCCESS",
      file=out,
      stringsAsFactors=FALSE
    )
  }
}

# ============================================================
# 7. AUDIT
# ============================================================

cat("\n[7/8] Création de l'audit...\n")

if(length(results)>0){

  audit <- bind_rows(results)

} else {

  audit <- data.frame(
    country=character(),
    variable=character(),
    status=character(),
    file=character()
  )
}

write.csv(
  audit,
  "results/tables/garch_midas_repaired_audit.csv",
  row.names=FALSE
)

cat("\nAUDIT :\n")
print(audit)

# ============================================================
# 8. RAPPORT FINAL
# ============================================================

cat("\n[8/8] Rapport final...\n")

report <- c(
  "RIMEH - REPARATION GPR + GARCH-MIDAS",
  "",
  paste("Date :",as.character(Sys.Date())),
  paste("rumidas version :",as.character(packageVersion("rumidas"))),
  "",
  paste("GPR observations :",nrow(gpr)),
  paste("GPRT unique :",length(unique(gpr$GPRT))),
  paste("GPRA unique :",length(unique(gpr$GPRA))),
  "",
  paste("Countries model data :",length(unique(model$country))),
  paste("Rows model data :",nrow(model)),
  "",
  paste("GARCH-MIDAS successful :",nrow(audit))
)

writeLines(
  report,
  "results/validation/RIMEH_GPR_MIDAS_REPAIR_REPORT.md"
)

cat("\n============================================================\n")
cat(" REPARATION TERMINEE\n")
cat("============================================================\n")
cat("\nFichiers principaux :\n")
cat("  data/raw/gpr_monthly_2000_2025.csv\n")
cat("  data/processed/monthly_clean.csv\n")
cat("  data/processed/monthly_model_data_FINAL.csv\n")
cat("  results/tables/garch_midas_repaired_audit.csv\n")
cat("  results/validation/RIMEH_GPR_MIDAS_REPAIR_REPORT.md\n\n")

cat("IMPORTANT : les coefficients doivent être lus dans les fichiers\n")
cat("GARCH_MIDAS_*.txt uniquement après vérification de l'audit.\n\n")