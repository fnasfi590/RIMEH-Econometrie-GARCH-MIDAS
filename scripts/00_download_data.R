# ============================================================
# 00_download_data.R
# AUTOMATISATION DES DONNEES RIMEH 2000-2025
# ============================================================
#
# Sources:
# - GPR: Caldara & Iacoviello official Excel
# - Global activity: Dallas Fed/FRED IGREA
# - WTI: FRED DCOILWTICO (daily), aggregated monthly
# - CPI US: FRED CPIAUCSL
# - GDP/inflation: World Bank WDI
# - Stock indices: Yahoo Finance via quantmod
#
# IMPORTANT:
# 1) Yahoo tickers are not guaranteed for every exchange. The script
#    reports failures instead of fabricating observations.
# 2) World Bank GDP is annual/quarterly depending on source; for the
#    first executable version we use annual GDP growth repeated within
#    the corresponding year. For the final GARCH-MIDAS specification,
#    replace this with quarterly GDP converted to monthly according to
#    the methodology adopted in the thesis.
# 3) SUP/DEM/PRE are NOT invented here. They are generated after the
#    validated SVAR identification.
# ============================================================

rm(list=ls())
source("scripts/00_packages.R")

library(WDI)
library(readxl)
library(httr)
library(quantmod)

dir.create("data/raw", recursive=TRUE, showWarnings=FALSE)
dir.create("data/oil", recursive=TRUE, showWarnings=FALSE)
dir.create("results", recursive=TRUE, showWarnings=FALSE)

START <- as.Date("2000-01-01")
END   <- as.Date("2025-12-31")

# ------------------------------------------------------------
# 1. COUNTRY MAP
# ------------------------------------------------------------
country_map <- tibble::tribble(
  ~country, ~wb,  ~ticker,
  "USA", "USA", "^GSPC",
  "SAU", "SAU", "^TASI.SR",
  "UAE", "ARE", "^ADI",
  "NGA", "NGA", "NGSEINDEX",
  "NOR", "NOR", "^OSEAX",
  "MEX", "MEX", "^MXX",
  "KAZ", "KAZ", "KASE",
  "TUR", "TUR", "XU100.IS",
  "EGY", "EGY", "EGX30.CA",
  "PAK", "PAK", "^KSE",
  "IND", "IND", "^NSEI",
  "KOR", "KOR", "^KS11",
  "JPN", "JPN", "^N225",
  "POL", "POL", "^WIG20",
  "DEU", "DEU", "^GDAXI",
  "GBR", "GBR", "^FTSE"
)

exporter <- c(
  USA=1, SAU=1, UAE=1, NGA=1, NOR=1, MEX=1, KAZ=1,
  TUR=0, EGY=0, PAK=0, IND=0, KOR=0, JPN=0, POL=0, DEU=0, GBR=0
)

write_csv(country_map, "data/raw/country_tickers.csv")

# ------------------------------------------------------------
# 2. GPR OFFICIAL DATA
# ------------------------------------------------------------
GPR_URL <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"
gpr_file <- "data/raw/data_gpr_export.xls"

message("Downloading official GPR data...")
tryCatch({
  download.file(GPR_URL, gpr_file, mode="wb", quiet=FALSE)
}, error=function(e) {
  stop("GPR download failed: ", e$message)
})

gpr_raw <- readxl::read_excel(gpr_file)

names(gpr_raw) <- make.names(names(gpr_raw), unique=TRUE)

# Detect month/date column
date_col <- names(gpr_raw)[grepl("month|date", names(gpr_raw), ignore.case=TRUE)][1]
if (is.na(date_col)) stop("Cannot identify GPR month column.")

gpr <- gpr_raw %>%
  rename(date = all_of(date_col)) %>%
  mutate(date=as.Date(date)) %>%
  filter(date >= START, date <= END)

# The official current dataset uses GPRT and GPRA.
# If names differ because of a vintage, stop rather than silently substitute.
if (!all(c("GPRT","GPRA") %in% names(gpr))) {
  stop(
    "GPRT/GPRA not found. Inspect data/raw/data_gpr_export.xls. ",
    "Available names: ", paste(names(gpr), collapse=", ")
  )
}

gpr <- gpr %>%
  dplyr::select(date,GPRT,GPRA)

# ------------------------------------------------------------
# 3. WORLD BANK MACRO DATA
# ------------------------------------------------------------
message("Downloading World Bank data...")

# NY.GDP.MKTP.KD.ZG = GDP growth annual %
# FP.CPI.TOTL.ZG = inflation annual %
wdi_raw <- WDI::WDI(
  country = unique(country_map$wb),
  indicator = c(
    GDP="NY.GDP.MKTP.KD.ZG",
    inflation="FP.CPI.TOTL.ZG"
  ),
  start = 2000,
  end = 2025,
  extra = FALSE
)

wdi <- wdi_raw %>%
  transmute(
    country = iso2c,
    year = year,
    GDP = GDP,
    inflation = inflation
  )

country_map2 <- country_map %>%
  dplyr::select(country,wb)

macro <- country_map2 %>%
  left_join(wdi, by=c("wb"="country")) %>%
  dplyr::select(country,year,GDP,inflation) %>%
  mutate(date=as.Date(paste0(year,"-01-01")))

# ------------------------------------------------------------
# 4. FRED GLOBAL ACTIVITY / OIL PRICE / US CPI / VIX
# ------------------------------------------------------------
fred_csv <- function(series) {
  url <- paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
    series
  )
  x <- read.csv(url, stringsAsFactors=FALSE)
  names(x) <- c("date","value")
  x$date <- as.Date(x$date)
  x
}

message("Downloading FRED series...")

igrea <- fred_csv("IGREA") %>%
  rename(global_activity=value)

cpi <- fred_csv("CPIAUCSL") %>%
  rename(us_cpi=value)

vix <- fred_csv("VIXCLS") %>%
  rename(VIX=value)

wti <- fred_csv("DCOILWTICO") %>%
  rename(WTI_nominal=value) %>%
  filter(date >= START, date <= END) %>%
  mutate(
    WTI_nominal=ifelse(WTI_nominal==".",NA,as.numeric(WTI_nominal))
  )

monthly_oil <- wti %>%
  mutate(month=as.Date(format(date,"%Y-%m-01"))) %>%
  group_by(month) %>%
  summarise(WTI_nominal=mean(WTI_nominal,na.rm=TRUE),.groups="drop")

monthly_cpi <- cpi %>%
  mutate(month=as.Date(format(date,"%Y-%m-01"))) %>%
  group_by(month) %>%
  summarise(us_cpi=mean(value,na.rm=TRUE),.groups="drop")

monthly_vix <- vix %>%
  mutate(month=as.Date(format(date,"%Y-%m-01"))) %>%
  group_by(month) %>%
  summarise(VIX=mean(VIX,na.rm=TRUE),.groups="drop")

oil_base <- monthly_oil %>%
  left_join(monthly_cpi, by="month") %>%
  mutate(
    real_oil_price = WTI_nominal / us_cpi * 100
  ) %>%
  dplyr::select(date=month,WTI_nominal,real_oil_price)

# ------------------------------------------------------------
# 5. STOCK PRICES FROM YAHOO
# ------------------------------------------------------------
message("Downloading stock indices...")

download_one_stock <- function(cc,ticker) {

  out <- tryCatch({

    x <- suppressWarnings(
      quantmod::getSymbols(
        Symbols=ticker,
        src="yahoo",
        from=START,
        to=END,
        auto.assign=FALSE
      )
    )

    px <- quantmod::Ad(x)

    tibble(
      date=as.Date(index(px)),
      stock_price=as.numeric(px),
      country=cc
    )

  }, error=function(e) {

    message("FAILED ",cc," / ",ticker," : ",e$message)
    NULL
  })

  out
}

stock_list <- purrr::map2(
  country_map$country,
  country_map$ticker,
  download_one_stock
)

stock_data <- purrr::compact(stock_list) %>%
  bind_rows()

if (nrow(stock_data)==0) {
  stop("No stock series downloaded. Check internet/Yahoo tickers.")
}

stock_data <- stock_data %>%
  mutate(
    exporter=exporter[country],
    return=NA_real_
  ) %>%
  arrange(country,date) %>%
  group_by(country) %>%
  mutate(
    return=100*(log(stock_price)-log(lag(stock_price)))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 6. DAILY BASE
# ------------------------------------------------------------
daily <- stock_data %>%
  dplyr::select(date,country,stock_price,return,exporter)

write_csv(
  daily,
  "data/raw/daily_data.csv"
)

# ------------------------------------------------------------
# 7. MONTHLY STOCK RV
# ------------------------------------------------------------
rv <- daily %>%
  mutate(month=as.Date(format(date,"%Y-%m-01"))) %>%
  group_by(country,month) %>%
  summarise(
    RV=sum(return^2,na.rm=TRUE),
    n_obs=sum(!is.na(return)),
    .groups="drop"
  ) %>%
  rename(date=month)

# ------------------------------------------------------------
# 8. MONTHLY BASE
# ------------------------------------------------------------
months <- seq(
  from=as.Date("2000-01-01"),
  to=as.Date("2025-12-01"),
  by="month"
)

monthly <- tidyr::expand_grid(
  date=months,
  country=country_map$country
) %>%
  left_join(country_map %>% dplyr::select(country),by="country") %>%
  mutate(
    exporter=exporter[country],
    year=lubridate::year(date)
  ) %>%
  left_join(gpr,by="date") %>%
  left_join(rv,by=c("country","date")) %>%
  left_join(monthly_vix %>% rename(date=month),by="date") %>%
  left_join(
    macro %>% mutate(date=as.Date(paste0(year,"-01-01"))),
    by=c("country","year")
  )

# For now, annual macro values are attached to January only.
# Do NOT forward-fill in the final paper without specifying the
# information-timing/interpolation method.
monthly <- monthly %>%
  dplyr::select(
    date,country,GPRT,GPRA,RV,n_obs,VIX,
    GDP,inflation,exporter
  ) %>%
  mutate(
    SUP=NA_real_,
    DEM=NA_real_,
    PRE=NA_real_,
    interest_rate=NA_real_,
    exchange_rate=NA_real_
  ) %>%
  dplyr::select(
    date,country,GPRT,GPRA,SUP,DEM,PRE,VIX,
    interest_rate,exchange_rate,GDP,inflation,
    RV,n_obs,exporter
  )

write_csv(
  monthly,
  "data/raw/monthly_data.csv"
)

# ------------------------------------------------------------
# 9. OIL SVAR INPUT
# ------------------------------------------------------------
oil_svar <- oil_base %>%
  left_join(
    igrea %>%
      mutate(date=as.Date(format(date,"%Y-%m-01"))) %>%
      group_by(date) %>%
      summarise(global_activity=mean(global_activity,na.rm=TRUE),.groups="drop"),
    by="date"
  ) %>%
  dplyr::select(
    date,
    global_activity,
    global_oil_production=NA_real_,
    real_oil_price
  )

write_csv(
  oil_svar,
  "data/oil/oil_svar_input.csv"
)

message("================================================")
message("DOWNLOAD STAGE FINISHED")
message("================================================")
message("Created:")
message("data/raw/daily_data.csv")
message("data/raw/monthly_data.csv")
message("data/oil/oil_svar_input.csv")
message("")
message("Still required:")
message("1) Global monthly oil production")
message("2) Policy interest rate")
message("3) Exchange rate")
message("4) Validated Kilian SVAR identification")
message("5) SUP / DEM / PRE")
message("")
message("No missing structural shocks were fabricated.")
