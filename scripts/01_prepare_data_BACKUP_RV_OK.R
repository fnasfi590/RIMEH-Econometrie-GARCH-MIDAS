# ============================================================
# 01_prepare_data.R
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

dir.create("data/processed", recursive=TRUE, showWarnings=FALSE)
dir.create("results/figures", recursive=TRUE, showWarnings=FALSE)

daily <- read_csv("data/raw/daily_data.csv", show_col_types=FALSE)
monthly <- read_csv("data/raw/monthly_data.csv", show_col_types=FALSE)
classification <- read_csv("data/raw/country_classification.csv", show_col_types=FALSE)

if (nrow(daily) == 0) stop("daily_data.csv is empty. Fill it with stock-index prices first.")
if (nrow(monthly) == 0) stop("monthly_data.csv is empty. Fill it with GPR and control variables first.")

daily <- daily %>%
  mutate(date=as.Date(date)) %>%
  left_join(classification, by="country", suffix=c("", "_class")) %>%
  mutate(exporter=coalesce(exporter, exporter_class)) %>%
  dplyr::select(-exporter_class) %>%
  arrange(country,date) %>%
  group_by(country) %>%
  mutate(return = 100*(log(stock_price)-log(lag(stock_price)))) %>%
  ungroup() %>%
  mutate(month=floor_date(date,"month"))

rv_monthly <- daily %>%
  group_by(country,month) %>%
  summarise(
    RV=sum(return^2,na.rm=TRUE),
    n_obs=sum(!is.na(return)),
    .groups="drop"
  ) %>%
  rename(date=month)

monthly <- monthly %>%
  mutate(date=as.Date(date)) %>%
  left_join(classification, by="country", suffix=c("", "_class")) %>%
  mutate(exporter=coalesce(exporter, exporter_class)) %>%
  dplyr::select(-exporter_class) %>%
  left_join(rv_monthly, by=c("country","date")) %>%
  mutate(
    GPR_SUP=GPRT*SUP,
    SUP_EXPORTER=SUP*exporter,
    SUP_positive=pmax(SUP,0),
    SUP_negative=pmin(SUP,0)
  ) %>%
  arrange(country,date)

write_csv(daily, "data/processed/daily_clean.csv")
write_csv(monthly, "data/processed/monthly_clean.csv")

# Descriptive statistics
desc <- daily %>%
  filter(!is.na(return)) %>%
  group_by(country) %>%
  summarise(
    N=n(),
    mean=mean(return),
    sd=sd(return),
    min=min(return),
    max=max(return),
    skewness=moments::skewness(return),
    kurtosis=moments::kurtosis(return),
    .groups="drop"
  )

write_csv(desc, "results/tables/descriptive_daily.csv")
