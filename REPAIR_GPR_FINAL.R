options(stringsAsFactors=FALSE)
options(warn=1)

cat("\n============================================================\n")
cat(" RIMEH - REPARATION GPR FINAL\n")
cat("============================================================\n\n")

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(readxl)
})

dir.create("data/raw", recursive=TRUE, showWarnings=FALSE)
dir.create("data/processed", recursive=TRUE, showWarnings=FALSE)
dir.create("results/validation", recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. ESSAI DES VERSIONS GPR
# ============================================================

cat("[1/6] Recherche du fichier GPR...\n\n")

# Plusieurs conventions possibles utilisées par le site
versions <- c(
  "202609",
  "20260901",
  "202608",
  "20260801",
  "202607",
  "202606",
  "202605",
  "202604",
  "202603",
  "202602",
  "202601",
  "202512",
  "202511",
  "202510",
  "202509",
  "202508",
  "202507",
  "202506",
  "202505",
  "202504",
  "202503",
  "202502",
  "202501",
  "202412",
  "202401"
)

base_urls <- c(
  "https://www.matteoiacoviello.com/gpr_files/",
  "https://www.matteoiacoviello.com/gpr_files/data/",
  "https://www.matteoiacoviello.com/"
)

candidate_urls <- c()

for(v in versions){

  for(base in base_urls){

    candidate_urls <- c(
      candidate_urls,
      paste0(base,"data_gpr_export_",v,".xls"),
      paste0(base,"data_gpr_export_",v,".xlsx")
    )

  }
}

# anciens noms connus
candidate_urls <- c(
  candidate_urls,
  "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls",
  "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xlsx"
)

candidate_urls <- unique(candidate_urls)

downloaded <- NULL

for(u in candidate_urls){

  cat("Test :",u,"\n")

  tmp <- tempfile(fileext=".xls")

  ok <- tryCatch({

    res <- download.file(
      u,
      tmp,
      mode="wb",
      quiet=TRUE
    )

    file.exists(tmp) && file.info(tmp)$size > 10000

  }, error=function(e){

    FALSE

  })

  if(ok){

    # Vérification Excel
    valid_excel <- tryCatch({

      ss <- readxl::excel_sheets(tmp)
      length(ss)>0

    }, error=function(e){

      FALSE

    })

    if(valid_excel){

      downloaded <- tmp

      cat("\n*** FICHIER GPR TROUVE ***\n")
      cat(u,"\n\n")

      break
    }
  }
}

if(is.null(downloaded)){

  stop(
    paste0(
      "\nImpossible de trouver automatiquement le fichier GPR officiel.\n",
      "Aucun des noms testés n'est accessible."
    )
  )
}

# sauvegarde locale
gpr_file <- "data/raw/gpr_official_current.xls"

file.copy(
  downloaded,
  gpr_file,
  overwrite=TRUE
)

cat("Fichier sauvegardé :",gpr_file,"\n")

# ============================================================
# 2. LECTURE EXCEL
# ============================================================

cat("\n[2/6] Lecture du fichier GPR...\n")

sheets <- excel_sheets(gpr_file)

cat("Sheets :\n")
print(sheets)

gpr_raw <- read_excel(
  gpr_file,
  sheet=sheets[1],
  .name_repair="minimal"
)

nm <- names(gpr_raw)

cat("\nDimensions :",nrow(gpr_raw),"x",ncol(gpr_raw),"\n")

cat("\nColonnes :\n")
print(nm)

# ============================================================
# 3. DETECTION GPRT / GPRA
# ============================================================

cat("\n[3/6] Détection GPRT / GPRA...\n")

clean_name <- function(x){

  x <- toupper(x)
  x <- gsub("[^A-Z0-9]","",x)

  x
}

nn <- clean_name(nm)

find_col <- function(target){

  target <- clean_name(target)

  exact <- which(nn==target)

  if(length(exact)>0)
    return(exact[1])

  partial <- which(grepl(target,nn,fixed=TRUE))

  if(length(partial)>0)
    return(partial[1])

  NA_integer_
}

gprt_col <- find_col("GPRT")
gpra_col <- find_col("GPRA")

date_col <- find_col("DATE")

if(is.na(date_col))
  date_col <- find_col("MONTH")

if(is.na(date_col))
  date_col <- 1

cat("Date :",nm[date_col],"\n")
cat("GPRT :",ifelse(is.na(gprt_col),"ABSENT",nm[gprt_col]),"\n")
cat("GPRA :",ifelse(is.na(gpra_col),"ABSENT",nm[gpra_col]),"\n")

if(is.na(gprt_col) || is.na(gpra_col)){

  cat("\nLes colonnes disponibles ressemblant à GPR :\n")
  print(
    nm[
      grepl(
        "GPR|THREAT|ACT",
        nm,
        ignore.case=TRUE
      )
    ]
  )

  stop("GPRT ou GPRA introuvable.")
}

# ============================================================
# 4. CONSTRUCTION DU FICHIER PROPRE
# ============================================================

cat("\n[4/6] Construction GPR 2000-2025...\n")

raw_date <- gpr_raw[[date_col]]

# Plusieurs formats possibles
d <- suppressWarnings(as.Date(raw_date))

if(all(is.na(d))){

  s <- as.character(raw_date)

  # YYYYMM
  idx <- grepl("^[0-9]{6}$",s)

  d[idx] <- as.Date(
    paste0(
      substr(s[idx],1,4),
      "-",
      substr(s[idx],5,6),
      "-01"
    )
  )
}

if(all(is.na(d))){

  num <- suppressWarnings(as.numeric(raw_date))

  idx <- is.finite(num)

  d[idx] <- as.Date(
    num[idx],
    origin="1899-12-30"
  )
}

if(all(is.na(d)))
  stop("Impossible de convertir les dates GPR.")

gpr <- data.frame(
  date=as.Date(floor_date(d,"month")),
  GPRT=as.numeric(gpr_raw[[gprt_col]]),
  GPRA=as.numeric(gpr_raw[[gpra_col]])
)

gpr <- gpr[
  !is.na(gpr$date) &
  gpr$date >= as.Date("2000-01-01") &
  gpr$date <= as.Date("2025-12-01"),
]

gpr <- gpr[
  !duplicated(gpr$date),
]

gpr <- gpr[
  order(gpr$date),
]

write.csv(
  gpr,
  "data/raw/gpr_monthly_2000_2025.csv",
  row.names=FALSE,
  na=""
)

cat("\nObservations GPR :",nrow(gpr),"\n")

cat("\nGPRT :\n")
print(summary(gpr$GPRT))

cat("\nGPRA :\n")
print(summary(gpr$GPRA))

cat(
  "\nNombre de valeurs GPRT différentes :",
  length(unique(gpr$GPRT[is.finite(gpr$GPRT)])),
  "\n"
)

cat(
  "Nombre de valeurs GPRA différentes :",
  length(unique(gpr$GPRA[is.finite(gpr$GPRA)])),
  "\n"
)

if(length(unique(gpr$GPRT[is.finite(gpr$GPRT)]))<=1)
  stop("GPRT est constant.")

if(length(unique(gpr$GPRA[is.finite(gpr$GPRA)]))<=1)
  stop("GPRA est constant.")

# ============================================================
# 5. REPARATION MONTHLY CLEAN
# ============================================================

cat("\n[5/6] Réparation monthly_clean.csv...\n")

monthly_file <- "data/processed/monthly_clean.csv"

monthly <- fread(
  monthly_file,
  data.table=FALSE
)

monthly$date <- as.Date(monthly$date)

# Supprimer les anciennes mauvaises colonnes
bad <- intersect(
  c(
    "GPRT",
    "GPRA",
    "GPR_SUP",
    "GPRT_SUP",
    "GPRA_SUP",
    "GPRT_DEM",
    "GPRT_PRE",
    "GPRT_VIX"
  ),
  names(monthly)
)

if(length(bad)>0){

  monthly <- monthly[
    ,
    setdiff(names(monthly),bad),
    drop=FALSE
  ]

}

monthly <- left_join(
  monthly,
  gpr,
  by="date"
)

write.csv(
  monthly,
  monthly_file,
  row.names=FALSE,
  na=""
)

cat("monthly_clean réparé.\n")

# Vérification USA
usa <- monthly[
  monthly$country=="USA",
  ,
  drop=FALSE
]

cat("\n============================================\n")
cat(" VERIFICATION USA\n")
cat("============================================\n")

cat("\nUSA observations :",nrow(usa),"\n")

cat("\nGPRT USA :\n")
print(summary(usa$GPRT))

cat("\nGPRA USA :\n")
print(summary(usa$GPRA))

cat("\nPremières valeurs :\n")
print(
  head(
    usa[
      ,
      c("date","GPRT","GPRA"),
      drop=FALSE
    ],
    12
  )
)

# ============================================================
# 6. RECONSTRUCTION MODEL FINAL
# ============================================================

cat("\n[6/6] Reconstruction monthly_model_data_FINAL...\n")

model <- monthly

# Interactions uniquement si les variables existent
if("SUP" %in% names(model))
  model$GPR_SUP <- model$GPRT * model$SUP

if("SUP" %in% names(model))
  model$GPRT_SUP <- model$GPRT * model$SUP

if("SUP" %in% names(model))
  model$GPRA_SUP <- model$GPRA * model$SUP

if("DEM" %in% names(model))
  model$GPRT_DEM <- model$GPRT * model$DEM

if("PRE" %in% names(model))
  model$GPRT_PRE <- model$GPRT * model$PRE

if("VIX" %in% names(model))
  model$GPRT_VIX <- model$GPRT * model$VIX

write.csv(
  model,
  "data/processed/monthly_model_data_FINAL.csv",
  row.names=FALSE,
  na=""
)

# Audit GPR
audit <- data.frame(
  file=gpr_file,
  observations=nrow(gpr),
  date_min=min(gpr$date,na.rm=TRUE),
  date_max=max(gpr$date,na.rm=TRUE),
  GPRT_unique=length(unique(gpr$GPRT[is.finite(gpr$GPRT)])),
  GPRA_unique=length(unique(gpr$GPRA[is.finite(gpr$GPRA)])),
  USA_rows=nrow(usa),
  USA_GPRT_unique=length(unique(usa$GPRT[is.finite(usa$GPRT)])),
  USA_GPRA_unique=length(unique(usa$GPRA[is.finite(usa$GPRA)]))
)

write.csv(
  audit,
  "results/validation/GPR_FINAL_AUDIT.csv",
  row.names=FALSE
)

cat("\n============================================================\n")
cat(" GPR REPARATION TERMINEE\n")
cat("============================================================\n")

print(audit)

cat("\nFichiers créés :\n")
cat("data/raw/gpr_monthly_2000_2025.csv\n")
cat("data/processed/monthly_clean.csv\n")
cat("data/processed/monthly_model_data_FINAL.csv\n")
cat("results/validation/GPR_FINAL_AUDIT.csv\n")

cat("\nIMPORTANT : GARCH-MIDAS n'est PAS encore relancé.\n")
cat("On vérifie d'abord que le GPR est réellement correct.\n")