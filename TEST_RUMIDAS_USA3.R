options(stringsAsFactors=FALSE)
options(warn=1)

library(data.table)
library(dplyr)
library(lubridate)
library(rumidas)

cat("\n============================================\n")
cat(" RIMEH - TEST RUMIDAS USA 3\n")
cat("============================================\n\n")

monthly <- fread(
  "data/processed/monthly_model_data_FINAL.csv",
  data.table=FALSE
)

daily <- fread(
  "data/processed/daily_clean.csv",
  data.table=FALSE
)

# IMPORTANT :
# Les dates sont deja correctes dans les CSV.
# On ne les reconvertit PAS.

dm <- monthly[
  monthly$country=="USA",
  ,
  drop=FALSE
]

dd <- daily[
  daily$country=="USA",
  ,
  drop=FALSE
]

dm <- dm[order(dm$date),,drop=FALSE]
dd <- dd[order(dd$date),,drop=FALSE]

cat("USA monthly :",nrow(dm),"\n")
cat("USA daily   :",nrow(dd),"\n\n")

cat("Date monthly debut :",as.character(dm$date[1]),"\n")
cat("Date monthly fin    :",as.character(dm$date[nrow(dm)]),"\n")

cat("Date daily debut    :",as.character(dd$date[1]),"\n")
cat("Date daily fin      :",as.character(dd$date[nrow(dd)]),"\n\n")

# ============================================================
# PREPARATION
# ============================================================

dm$month <- floor_date(dm$date,"month")
dd$month <- floor_date(dd$date,"month")

dm$GPRT <- as.numeric(dm$GPRT)
dd$ret  <- as.numeric(dd$return)

dm <- dm[
  is.finite(dm$GPRT),
  ,
  drop=FALSE
]

dd <- dd[
  is.finite(dd$ret),
  ,
  drop=FALSE
]

common <- intersect(
  unique(dm$month),
  unique(dd$month)
)

dm <- dm[
  dm$month %in% common,
  ,
  drop=FALSE
]

dd <- dd[
  dd$month %in% common,
  ,
  drop=FALSE
]

dm <- dm[order(dm$month),,drop=FALSE]
dd <- dd[order(dd$date),,drop=FALSE]

cat("Common months :",length(common),"\n")
cat("Monthly usable:",nrow(dm),"\n")
cat("Daily usable  :",nrow(dd),"\n\n")

# ============================================================
# VARIABLES NUMERIQUES
# ============================================================

mv <- as.numeric(dm$GPRT)
x  <- as.numeric(dd$ret)

cat("GPRT length :",length(mv),"\n")
cat("Return length:",length(x),"\n")

cat("GPRT range :\n")
print(range(mv,na.rm=TRUE))

cat("\nReturn range:\n")
print(range(x,na.rm=TRUE))

# ============================================================
# TEST mv_into_mat
# ============================================================

K <- 3

cat("\n============================================\n")
cat(" TEST 1 : mv_into_mat\n")
cat("============================================\n")

mvmat <- tryCatch({

  rumidas::mv_into_mat(
    x=x,
    mv=mv,
    K=K,
    type="monthly"
  )

}, error=function(e){

  cat("\nERREUR mv_into_mat\n")
  cat("Message :",conditionMessage(e),"\n")

  NULL
})

if(is.null(mvmat)){
  stop("ECHEC TEST 1")
}

cat("\nTEST 1 = SUCCESS\n")
cat("Class :",class(mvmat),"\n")
cat("Dimensions :\n")
print(dim(mvmat))

cat("\nPremieres lignes de mvmat :\n")
print(head(mvmat))

# ============================================================
# TEST UGMFIT
# ============================================================

cat("\n============================================\n")
cat(" TEST 2 : ugmfit\n")
cat("============================================\n")

cat("model        = GM\n")
cat("distribution = std\n")
cat("skew         = NO\n")
cat("K            =",K,"\n")
cat("R            = 10\n\n")

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

  cat("\nERREUR UGMFIT\n")
  cat("Message :",conditionMessage(e),"\n")

  NULL
})

if(is.null(fit)){
  stop("ECHEC TEST 2")
}

cat("\n============================================\n")
cat(" TEST RUMIDAS COMPLET = SUCCESS\n")
cat("============================================\n\n")

print(fit)

saveRDS(
  fit,
  "results/models/TEST_RUMIDAS_USA3.rds"
)

cat("\nFichier sauvegarde :\n")
cat("results/models/TEST_RUMIDAS_USA3.rds\n")