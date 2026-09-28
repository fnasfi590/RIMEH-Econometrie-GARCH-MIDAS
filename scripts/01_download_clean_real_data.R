# ============================================================
# RIMEH - PARTIE 1
# TÃ©lÃ©chargement automatique + nettoyage des donnÃ©es rÃ©elles
# PÃ©riode: 2000-01-01 Ã  2025-12-31
# ============================================================
# Sources:
# - Yahoo Finance: indices boursiers quotidiens
# - Matteo Iacoviello/Caldara: GPR mensuel
# - FRED: VIX, WTI, IGREA
# - World Bank WDI: croissance PIB + inflation annuelle
#
# IMPORTANT:
# Ce script ne fabrique aucune donnÃ©e manquante. Les sÃ©ries non
# disponibles sont signalÃ©es dans data/raw/download_audit.csv.
# ============================================================

options(stringsAsFactors = FALSE)

# ---------- 0. PARAMETRES ----------
START <- as.Date("2000-01-01")
END   <- as.Date("2025-12-31")

ROOT <- tryCatch(normalizePath(file.path(getwd()), winslash = "/"), error=function(e) getwd())
if (basename(ROOT) == "scripts") ROOT <- dirname(ROOT)
DIR_RAW <- file.path(ROOT, "data", "raw")
DIR_OIL <- file.path(ROOT, "data", "oil")
dir.create(DIR_RAW, recursive=TRUE, showWarnings=FALSE)
dir.create(DIR_OIL, recursive=TRUE, showWarnings=FALSE)

# ---------- 1. PACKAGES ----------
pkgs <- c("data.table","dplyr","tidyr","lubridate","readxl","quantmod","WDI","zoo","xts","jsonlite")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE))

# ---------- 2. PAYS / TICKERS ----------
# Les tickers sont des candidats Yahoo Finance et doivent Ãªtre contrÃ´lÃ©s
# dans l'audit aprÃ¨s tÃ©lÃ©chargement.
countries <- data.frame(
  country=c("USA","Saudi Arabia","UAE","Nigeria","Norway","Mexico","Kazakhstan",
            "Turkey","Egypt","Pakistan","India","South Korea","Japan","Poland",
            "Germany","United Kingdom"),
  iso3=c("USA","SAU","ARE","NGA","NOR","MEX","KAZ","TUR","EGY","PAK","IND","KOR","JPN","POL","DEU","GBR"),
  group=c("exporter","exporter","exporter","exporter","exporter","exporter","exporter",
          "importer","importer","importer","importer","importer","importer","importer","importer","importer"),
  yahoo_ticker=c("^GSPC","^TASI.SR","^ADI","NGSEINDEX","^OSEAX","^MXX","KASE",
                 "XU100.IS","EGX30.CA","^KSE","^NSEI","^KS11","^N225","^WIG20","^GDAXI","^FTSE"),
  stringsAsFactors=FALSE
)
write.csv(countries, file.path(DIR_RAW,"country_tickers.csv"), row.names=FALSE, na="")

# ---------- 3. FONCTION: YAHOO ----------
download_yahoo <- function(ticker, symbol_name, start=START, end=END) {
  out <- tryCatch({
    x <- suppressWarnings(getSymbols(ticker, src="yahoo", from=start, to=end,
                                     auto.assign=FALSE, warnings=FALSE))
    x <- x[paste0(start,"/",end)]
    if (NROW(x) < 50) stop("moins de 50 observations")
    close <- tryCatch(Ad(x), error=function(e) Cl(x))
    data.frame(date=as.Date(index(close)), price=as.numeric(close), stringsAsFactors=FALSE)
  }, error=function(e) {
    message("[ECHEC Yahoo] ", symbol_name, " / ", ticker, " : ", conditionMessage(e))
    NULL
  })
  out
}

# ---------- 4. INDICES BOURSIERS QUOTIDIENS + RETURNS + RV ----------
audit <- list()
daily_list <- list()
monthly_rv_list <- list()

for (i in seq_len(nrow(countries))) {
  cc <- countries[i,]
  d <- download_yahoo(cc$yahoo_ticker, cc$country)
  if (is.null(d)) {
    audit[[length(audit)+1]] <- data.frame(source="Yahoo", country=cc$country, iso3=cc$iso3,
      ticker=cc$yahoo_ticker, status="FAILED", n=0, first=NA, last=NA, note="ticker indisponible ou refus Yahoo")
    next
  }
  d <- d %>%
    mutate(country=cc$country, iso3=cc$iso3, group=cc$group,
           return=100*(log(price)-log(dplyr::lag(price)))) %>%
    filter(is.finite(return) | !is.na(price))
  # Une observation par date, prix > 0
  d <- d %>% filter(!is.na(price), price > 0) %>% distinct(date, .keep_all=TRUE)
  daily_list[[cc$iso3]] <- d

  rv <- d %>%
    mutate(month=as.Date(floor_date(date,"month"))) %>%
    group_by(country, iso3, group, month) %>%
    summarise(RV=sum(return^2, na.rm=TRUE),
              n_daily=sum(!is.na(return)), .groups="drop")
  monthly_rv_list[[cc$iso3]] <- rv

  audit[[length(audit)+1]] <- data.frame(source="Yahoo", country=cc$country, iso3=cc$iso3,
    ticker=cc$yahoo_ticker, status="OK", n=nrow(d), first=min(d$date), last=max(d$date), note="")
}

daily <- bind_rows(daily_list)
monthly_rv <- bind_rows(monthly_rv_list)
write.csv(daily, file.path(DIR_RAW,"daily_stock_data.csv"), row.names=FALSE, na="")
write.csv(monthly_rv, file.path(DIR_RAW,"monthly_realized_volatility.csv"), row.names=FALSE, na="")

# ---------- 5. FRED SANS CLE API: CSV PUBLIC ----------
# FRED propose des tÃ©lÃ©chargements CSV de graphiques; l'API REST classique
# nÃ©cessite une clÃ©. Ici on utilise le flux CSV public.
fred_csv <- function(series, name) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series)
  z <- tryCatch({
    x <- fread(url, na.strings=c(".","NA",""))
    names(x)[1:2] <- c("date", name)
    x$date <- as.Date(x$date)
    x[[name]] <- suppressWarnings(as.numeric(x[[name]]))
    x %>% filter(date >= START, date <= END)
  }, error=function(e) {
    message("[ECHEC FRED] ", series, " : ", conditionMessage(e)); NULL
  })
  z
}

vix <- fred_csv("VIXCLS", "VIX")
wti <- fred_csv("DCOILWTICO", "WTI")
igrea <- fred_csv("IGREA", "IGREA")

if (!is.null(vix)) write.csv(vix, file.path(DIR_RAW,"fred_vix_daily.csv"), row.names=FALSE, na="")
if (!is.null(wti)) write.csv(wti, file.path(DIR_RAW,"fred_wti_daily.csv"), row.names=FALSE, na="")
if (!is.null(igrea)) write.csv(igrea, file.path(DIR_RAW,"fred_igrea_monthly.csv"), row.names=FALSE, na="")

# ---------- 6. GPR MENSUEL OFFICIEL ----------
GPR_URL <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"
gpr_file <- file.path(DIR_RAW,"data_gpr_export.xls")
tryCatch({
  download.file(GPR_URL, gpr_file, mode="wb", quiet=TRUE)
  message("GPR tÃ©lÃ©chargÃ©: ", gpr_file)
}, error=function(e) message("[ECHEC GPR] ", conditionMessage(e)))

gpr <- NULL
if (file.exists(gpr_file)) {
  sheets <- readxl::excel_sheets(gpr_file)
  gpr_raw <- readxl::read_excel(gpr_file, sheet=sheets[1], .name_repair="minimal")
  # Recherche robuste de la colonne date.
  nm <- names(gpr_raw)
  date_col <- which(tolower(nm) %in% c("date","month","monthdate","yyyymm"))[1]
  if (is.na(date_col)) date_col <- 1
  names(gpr_raw)[date_col] <- "date_raw"
  gpr_raw$date <- suppressWarnings(as.Date(gpr_raw$date_raw))
  if (all(is.na(gpr_raw$date))) {
    # Essai si YYYYMM numÃ©rique / texte
    s <- as.character(gpr_raw$date_raw)
    gpr_raw$date <- suppressWarnings(as.Date(paste0(substr(s,1,4),"-",substr(s,5,6),"-01")))
  }
  # Cherche les noms exacts GPRT / GPRA, sinon variantes insensibles Ã  la casse.
  pick <- function(target) {
    j <- which(toupper(gsub("[^A-Z0-9]","",nm)) == toupper(gsub("[^A-Z0-9]","",target)))
    if (length(j)) nm[j[1]] else NULL
  }
  gprt_col <- pick("GPRT")
  gpra_col <- pick("GPRA")
  if (!is.null(gprt_col) && !is.null(gpra_col)) {
    gpr <- data.frame(date=as.Date(floor_date(gpr_raw$date,"month")),
                      GPRT=as.numeric(gpr_raw[[gprt_col]]),
                      GPRA=as.numeric(gpr_raw[[gpra_col]])) %>%
      filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-01")) %>%
      distinct(date,.keep_all=TRUE)
    write.csv(gpr, file.path(DIR_RAW,"gpr_monthly_2000_2025.csv"), row.names=FALSE, na="")
  } else {
    message("[GPR] Colonnes GPRT/GPRA non trouvÃ©es. Voir les noms de colonnes du fichier Excel.")
  }
}

# ---------- 7. WORLD BANK: PIB + INFLATION ANNUELS ----------
# Ces sÃ©ries sont annuelles. On les conserve annuelles et on ne les rÃ©pÃ¨te
# PAS artificiellement dans les mois. Pour GARCH-MIDAS, une mÃ©thode de
# timing/interpolation documentÃ©e devra Ãªtre choisie dans l'Ã©tape suivante.
wdi_ind <- c(GDP="NY.GDP.MKTP.KD.ZG", inflation="FP.CPI.TOTL.ZG")
wdi <- tryCatch({
  WDI(country=countries$iso3, indicator=wdi_ind, start=2000, end=2025, extra=FALSE)
}, error=function(e) { message("[ECHEC WDI] ",conditionMessage(e)); NULL })
if (!is.null(wdi)) {
  wdi <- wdi %>%
    transmute(iso3=country, year=as.integer(year), GDP=as.numeric(GDP), inflation=as.numeric(inflation)) %>%
    filter(year>=2000, year<=2025)
  write.csv(wdi, file.path(DIR_RAW,"worldbank_annual_macro.csv"), row.names=FALSE, na="")
}

# ---------- 8. OIL REAL PRICE MENSUEL ----------
if (!is.null(wti)) {
  # CPI US mensuel: tÃ©lÃ©chargement FRED public.
  cpi <- fred_csv("CPIAUCSL", "CPI_US")
  if (!is.null(cpi)) {
    oil_month <- wti %>%
      mutate(month=as.Date(floor_date(date,"month"))) %>%
      group_by(month) %>% summarise(WTI_nominal=mean(WTI, na.rm=TRUE), .groups="drop") %>%
      left_join(cpi %>% mutate(month=as.Date(floor_date(date,"month"))) %>%
                  group_by(month) %>% summarise(CPI_US=mean(CPI_US, na.rm=TRUE), .groups="drop"), by="month")
    # base CPI Jan-2000 = 100 pour obtenir un prix rÃ©el comparable dans l'Ã©chantillon.
    base <- oil_month$CPI_US[oil_month$month==as.Date("2000-01-01")][1]
    oil_month <- oil_month %>% mutate(real_WTI=WTI_nominal/(CPI_US/base))
    write.csv(oil_month, file.path(DIR_OIL,"real_wti_monthly_2000_2025.csv"), row.names=FALSE, na="")
  }
}

# ---------- 9. CONTROLE DE COUVERTURE ----------
coverage <- countries %>%
  left_join(bind_rows(audit) %>% dplyr::select(iso3,status,n,first,last,note), by="iso3") %>%
  mutate(expected_start=START, expected_end=END,
         coverage_days=as.integer(last-first)+1,
         status=ifelse(is.na(status),"NOT_DOWNLOADED",status))
write.csv(coverage, file.path(DIR_RAW,"download_audit.csv"), row.names=FALSE, na="")

# ---------- 10. BASE MONTHLY PRINCIPALE ----------
# AgrÃ©gation VIX + WTI + RV. GPR sera ajoutÃ© si disponible.
monthly <- monthly_rv %>% rename(date=month)
if (!is.null(vix)) {
  vix_m <- vix %>% mutate(date=as.Date(floor_date(date,"month"))) %>%
    group_by(date) %>% summarise(VIX=mean(VIX,na.rm=TRUE), .groups="drop")
  monthly <- monthly %>% left_join(vix_m, by="date")
}
if (!is.null(oil_month)) monthly <- monthly %>% left_join(oil_month %>% dplyr::select(date=month, WTI_nominal, real_WTI), by="date")
if (!is.null(gpr)) monthly <- monthly %>% left_join(gpr, by="date")
write.csv(monthly, file.path(DIR_RAW,"monthly_market_gpr_data.csv"), row.names=FALSE, na="")

# ---------- 11. DICTIONNAIRE ----------
dict <- data.frame(
  variable=c("price","return","RV","VIX","WTI_nominal","real_WTI","IGREA","GPRT","GPRA","GDP","inflation"),
  definition=c(
    "Cours de clÃ´ture/Adjusted close de l'indice boursier",
    "100*log(P_t/P_{t-1})",
    "Somme mensuelle des rendements quotidiens au carrÃ©",
    "CBOE Volatility Index, moyenne mensuelle du VIX quotidien",
    "Prix WTI nominal, moyenne mensuelle",
    "Prix WTI dÃ©flatÃ© par CPI US, base janvier 2000=100",
    "Index of Global Real Economic Activity (Kilian/FRED), mensuel",
    "Geopolitical Threats, GPR global mensuel",
    "Geopolitical Acts, GPR global mensuel",
    "Croissance annuelle du PIB rÃ©el, World Bank",
    "Inflation annuelle CPI, World Bank"
  ), stringsAsFactors=FALSE)
write.csv(dict, file.path(ROOT,"data","data_dictionary_part1.csv"), row.names=FALSE)

cat("\n============================================================\n")
cat("PARTIE 1 TERMINEE\n")
cat("Fichiers principaux:\n")
cat(" - data/raw/daily_stock_data.csv\n")
cat(" - data/raw/monthly_realized_volatility.csv\n")
cat(" - data/raw/monthly_market_gpr_data.csv\n")
cat(" - data/raw/gpr_monthly_2000_2025.csv\n")
cat(" - data/raw/fred_vix_daily.csv\n")
cat(" - data/raw/fred_wti_daily.csv\n")
cat(" - data/raw/fred_igrea_monthly.csv\n")
cat(" - data/raw/worldbank_annual_macro.csv\n")
cat(" - data/oil/real_wti_monthly_2000_2025.csv\n")
cat(" - data/raw/download_audit.csv\n")
cat("============================================================\n")
