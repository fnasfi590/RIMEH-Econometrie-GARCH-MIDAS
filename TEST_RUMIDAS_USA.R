options(stringsAsFactors=FALSE)
options(warn=1)

cat("\n========================================\n")
cat(" TEST RUMIDAS USA\n")
cat("========================================\n")

library(data.table)
library(dplyr)
library(lubridate)
library(rumidas)

cat("rumidas version :", as.character(packageVersion("rumidas")), "\n")

monthly <- fread("data/processed/monthly_model_data_FINAL.csv")
daily   <- fread("data/processed/daily_clean.csv")

cat("Monthly :", nrow(monthly), "rows\n")
cat("Daily   :", nrow(daily), "rows\n")

cat("\nColonnes monthly:\n")
print(names(monthly))

cat("\nColonnes daily:\n")
print(names(daily))

# dates
monthly[, date := as.Date(date)]
daily[, date := as.Date(date)]

# chercher colonne pays
ccm <- names(monthly)[tolower(names(monthly)) %in%
                       c("country","iso","iso3","country_code")][1]

ccd <- names(daily)[tolower(names(daily)) %in%
                    c("country","iso","iso3","country_code")][1]

cat("\nCountry monthly :",ccm,"\n")
cat("Country daily   :",ccd,"\n")

# chercher rendement
rc <- c(
 "return","returns","ret","daily_return",
 "log_return","logret","R","r"
)

rc <- rc[rc %in% names(daily)]

if(length(rc)==0){
  nums <- names(daily)[sapply(daily,is.numeric)]
  nums <- setdiff(nums,c("year","month","day"))
  rc <- nums[1]
}

retcol <- rc[1]

cat("Return column :",retcol,"\n")

# USA
dm <- monthly %>%
  filter(.data[[ccm]]=="USA") %>%
  arrange(date)

dd <- daily %>%
  filter(.data[[ccd]]=="USA") %>%
  arrange(date)

cat("\nUSA monthly :",nrow(dm),"\n")
cat("USA daily   :",nrow(dd),"\n")

cat("\nMonthly date range:\n")
print(range(dm,na.rm=TRUE))

cat("\nDaily date range:\n")
print(range(dd,na.rm=TRUE))

cat("\nGPRT summary:\n")
print(summary(dm))

# -------------------------------------------------
# preparation
# -------------------------------------------------

dm <- dm %>%
  mutate(
    month=floor_date(date,"month"),
    GPRT=as.numeric(GPRT)
  ) %>%
  filter(is.finite(GPRT))

dd <- dd %>%
  mutate(
    month=floor_date(date,"month"),
    ret=as.numeric(.data[[retcol]])
  ) %>%
  filter(is.finite(ret))

common <- intersect(unique(dm),unique(dd))

dm <- dm %>%
  filter(month %in% common) %>%
  arrange(month)

dd <- dd %>%
  filter(month %in% common) %>%
  arrange(date)

cat("\nCommon months :",length(common),"\n")

# -------------------------------------------------
# TEST 1 : mv_into_mat
# -------------------------------------------------

K <- 3

cat("\nTEST mv_into_mat...\n")

mv <- dm

cat("Length mv :",length(mv),"\n")
cat("Length daily :",nrow(dd),"\n")

mvmat <- tryCatch({

  rumidas::mv_into_mat(
    x=dd,
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
  stop("TEST 1 ECHEC")
}

cat("\nTEST 1 SUCCESS\n")
cat("mvmat dimensions :")
print(dim(mvmat))

# -------------------------------------------------
# TEST 2 : ugmfit
# -------------------------------------------------

cat("\nTEST ugmfit...\n")
cat("Model = GM\n")
cat("Distribution = std\n")
cat("K =",K,"\n")

fit <- tryCatch({

  rumidas::ugmfit(
    model="GM",
    skew="NO",
    distribution="std",
    daily_ret=dd,
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
  stop("TEST 2 ECHEC")
}

cat("\n========================================\n")
cat(" TEST RUMIDAS USA : SUCCESS\n")
cat("========================================\n")

cat("\nObjet estime :\n")
print(fit)

saveRDS(
 fit,
 "results/models/TEST_RUMIDAS_USA.rds"
)

cat("\nFichier créé : results/models/TEST_RUMIDAS_USA.rds\n")