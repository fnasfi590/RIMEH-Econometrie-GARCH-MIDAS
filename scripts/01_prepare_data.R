library(readr)
library(dplyr)
library(lubridate)

cat("\n============================================================\n")
cat(" PREPARATION DES DONNEES RIMEH - RV CORRIGE\n")
cat("============================================================\n")

daily <- read_csv(
  "data/raw/daily_data.csv",
  show_col_types=FALSE
)

monthly <- read_csv(
  "data/raw/monthly_data.csv",
  show_col_types=FALSE
)

classification <- read_csv(
  "data/raw/country_classification.csv",
  show_col_types=FALSE
)

# ============================================================
# DAILY
# ============================================================

daily <- daily %>%
  mutate(date=as.Date(date)) %>%
  left_join(
    classification,
    by="country",
    suffix=c("", "_class")
  ) %>%
  mutate(
    exporter=coalesce(exporter, exporter_class)
  ) %>%
  dplyr::select(-exporter_class) %>%
  arrange(country,date) %>%
  group_by(country) %>%
  mutate(
    return=100*(log(stock_price)-log(lag(stock_price)))
  ) %>%
  ungroup() %>%
  mutate(
    month=floor_date(date,"month")
  )

# ============================================================
# RV MENSUEL : CALCUL UNIQUE A PARTIR DU DAILY
# ============================================================

rv_monthly <- daily %>%
  group_by(country,month) %>%
  summarise(
    RV=sum(return^2,na.rm=TRUE),
    n_obs=sum(!is.na(return)),
    .groups="drop"
  ) %>%
  rename(date=month)

# ============================================================
# MONTHLY
# ============================================================

monthly <- monthly %>%
  mutate(date=as.Date(date)) %>%
  # Supprimer les anciennes colonnes RV avant le nouveau join
  dplyr::select(
    -any_of(c("RV","n_obs","n_daily"))
  ) %>%
  left_join(
    classification,
    by="country",
    suffix=c("", "_class")
  ) %>%
  mutate(
    exporter=coalesce(exporter, exporter_class)
  ) %>%
  dplyr::select(-exporter_class) %>%
  left_join(
    rv_monthly,
    by=c("country","date")
  ) %>%
  arrange(country,date)

# ============================================================
# EXPORT
# ============================================================

dir.create("data/processed",showWarnings=FALSE,recursive=TRUE)

write_csv(
  daily,
  "data/processed/daily_clean.csv"
)

write_csv(
  monthly,
  "data/processed/monthly_clean.csv"
)

cat("\n============================================================\n")
cat(" VERIFICATION RV\n")
cat("============================================================\n")

cat("Dimensions monthly :",nrow(monthly),"x",ncol(monthly),"\n")
cat("Colonnes RV :",paste(names(monthly)[grepl("^RV",names(monthly))],collapse=", "),"\n")
cat("RV NA :",sum(is.na(monthly$RV)),"\n")
cat("RV finite :",sum(is.finite(monthly$RV)),"\n")

cat("\nRésumé RV :\n")
print(summary(monthly$RV))

cat("\nSAU contrôle :\n")
print(
  monthly %>%
    filter(
      country=="SAU",
      date %in% as.Date(c(
        "2006-05-01",
        "2008-10-01",
        "2009-03-01",
        "2012-12-01"
      ))
    ) %>%
    select(date,country,RV,n_obs)
)

cat("\n============================================================\n")
cat(" PREPARATION TERMINEE\n")
cat("============================================================\n")
