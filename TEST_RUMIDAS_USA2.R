options(stringsAsFactors=FALSE)
options(warn=1)

cat("\n========================================\n")
cat(" TEST RUMIDAS USA - VERSION 2\n")
cat("========================================\n\n")

library(data.table)
library(dplyr)
library(lubridate)
library(rumidas)

cat("R        :", R.version.string, "\n")
cat("rumidas  :", as.character(packageVersion("rumidas")), "\n\n")

# ============================================================
# CHARGEMENT
# ============================================================

monthly <- fread(
  "data/processed/monthly_model_data_FINAL.csv",
  data.table=FALSE
)

daily <- fread(
  "data/processed/daily_clean.csv",
  data.table=FALSE
)

cat("Monthly rows :",nrow(monthly),"\n")
cat("Daily rows   :",nrow(daily),"\n\n")

# ============================================================
# DATE EXPLICITE
# ============================================================

monthly <- as.Date(as.character(monthly))
daily   <- as.Date(as.character(daily))

cat("Class monthly :",class(monthly),"\n")
cat("Class daily   :",class(daily),"\n\n")

# ============================================================
# USA
# ============================================================

dm <- monthly[
  monthly=="USA",
  ,
  drop=FALSE
]

dd <- daily[
  daily=="USA",
  ,
  drop=FALSE
]

dm <- dm[order(dm),,drop=FALSE]
dd <- dd[order(dd),,drop=FALSE]

cat("USA monthly :",nrow(dm),"\n")
cat("USA daily   :",nrow(dd),"\n\n")

cat("Premier mois USA :",as.character(dm[1]),"\n")
cat("Dernier mois USA :",as.character(dm[nrow(dm)]),"\n")

cat("Premier jour USA :",as.character(dd[1]),"\n")
cat("Dernier jour USA :",as.character(dd[nrow(dd)]),"\n\n")

# ============================================================
# VARIABLES
# ============================================================

cat("GPRT NA :",sum(is.na(dm)),"\n")
cat("GPRT finite :",sum(is.finite(dm)),"\n")

cat("Return NA :",sum(is.na(dd)),"\n")
cat("Return finite :",sum(is.finite(dd)),"\n\n")

# ============================================================
# PREPARATION MENSUELLE
# ============================================================

dm <- floor_date(dm,"month")
dd <- floor_date(dd,"month")

dm <- as.numeric(dm)
dd  <- as.numeric(dd)

dm <- dm[
  is.finite(dm),
  ,
  drop=FALSE
]

dd <- dd[
  is.finite(dd),
  ,
  drop=FALSE
]

common <- intersect(
  unique(dm),
  unique(dd)
)

cat("Common months :",length(common),"\n\n")

dm <- dm[
  dm %in% common,
  ,
  drop=FALSE
]

dd <- dd[
  dd %in% common,
  ,
  drop=FALSE
]

dm <- dm[order(dm),,drop=FALSE]
dd <- dd[order(dd),,drop=FALSE]

cat("Monthly after alignment :",nrow(dm),"\n")
cat("Daily after alignment   :",nrow(dd),"\n")

# ============================================================
# TEST MATRICE MIDAS
# ============================================================

K <- 3

mv <- as.numeric(dm)
x  <- as.numeric(dd)

cat("\n========================================\n")
cat(" TEST mv_into_mat\n")
cat("========================================\n")

cat("Length x  :",length(x),"\n")
cat("Length mv :",length(mv),"\n")
cat("K         :",K,"\n\n")

mvmat <- tryCatch({

  rumidas::mv_into_mat(
    x=x,
    mv=mv,
    K=K,
    type="monthly"
  )

}, error=function(e){

  cat("\nERREUR mv_into_mat:\n")
  cat(conditionMessage(e),"\n")

  NULL
})

if(is.null(mvmat)){
  stop("mv_into_mat a echoue.")
}

cat("\nSUCCESS mv_into_mat\n")
cat("Class :",class(mvmat),"\n")
cat("Dimensions :\n")
print(dim(mvmat))

# ============================================================
# TEST UGMFIT
# ============================================================

cat("\n========================================\n")
cat(" TEST UGMFIT\n")
cat("========================================\n")

fit <- tryCatch({

  rumidas::ugmfit(
    model="GM",
    skew="NO",
    distribution="std",
    daily_ret=x,
    mv_m=mvmat,
    K=K,
    lag_fun="Beta",
    R=10
  )

}, error=function(e){

  cat("\nERREUR ugmfit:\n")
  cat(conditionMessage(e),"\n")

  NULL
})

if(is.null(fit)){
  stop("ugmfit a echoue.")
}

cat("\n========================================\n")
cat(" SUCCESS COMPLET\n")
cat("========================================\n\n")

print(fit)

saveRDS(
  fit,
  "results/models/TEST_RUMIDAS_USA2.rds"
)

cat("\nFichier : results/models/TEST_RUMIDAS_USA2.rds\n")
cat("TEST TERMINE\n")