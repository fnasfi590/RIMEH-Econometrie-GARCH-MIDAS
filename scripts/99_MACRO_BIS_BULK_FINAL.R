options(stringsAsFactors=FALSE)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

pkgs <- c("readr","dplyr","tidyr","lubridate","httr2")
for(p in pkgs)
  if(!requireNamespace(p,quietly=TRUE))
    install.packages(p,repos="https://cloud.r-project.org")

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(httr2)

dir.create(file.path(root,"data/raw"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(root,"results/tables"),recursive=TRUE,showWarnings=FALSE)

countries <- c(
 "DEU","GBR","IND","JPN","KOR","MEX",
 "NOR","PAK","SAU","TUR","USA"
)

calendar <- expand.grid(
 country=countries,
 date=seq(as.Date("2000-01-01"),as.Date("2025-12-01"),by="month")
) |>
 arrange(country,date)

cat("\n============================================================\n")
cat(" RIMEH - BIS BULK CPI + POLICY RATES\n")
cat("============================================================\n")

# ============================================================
# 1. DOWNLOAD BIS POLICY RATES BULK
# ============================================================

cat("\n[1/6] TELECHARGEMENT BIS POLICY RATES...\n")

bis_rate_zip <- file.path(root,"data/raw/BIS_WS_CBPOL.zip")

download.file(
 "https://data.bis.org/static/bulk/WS_CBPOL_csv_col.zip",
 bis_rate_zip,
 mode="wb",
 quiet=FALSE
)

unz_rate <- tempfile("bis_rate_")
dir.create(unz_rate)

unzip(
 bis_rate_zip,
 exdir=unz_rate
)

rate_files <- list.files(
 unz_rate,
 pattern="\\.csv$",
 recursive=TRUE,
 full.names=TRUE,
 ignore.case=TRUE
)

cat("Fichiers BIS taux :",length(rate_files),"\n")

if(length(rate_files)==0)
 stop("Aucun CSV BIS taux trouve.")

rate_file <- rate_files[1]

cat("Lecture :",basename(rate_file),"\n")

rates <- read_csv(
 rate_file,
 show_col_types=FALSE,
 guess_max=1000000
)

names(rates) <- toupper(names(rates))

cat("Colonnes taux BIS :\n")
print(names(rates))

# ============================================================
# Identification automatique des colonnes
# ============================================================

ref_col <- names(rates)[
 grepl("REF_AREA|COUNTRY",names(rates))
][1]

time_col <- names(rates)[
 grepl("TIME_PERIOD|DATE|PERIOD",names(rates))
][1]

value_col <- names(rates)[
 grepl("^OBS_VALUE$|OBS_VALUE|VALUE",names(rates))
][1]

freq_col <- names(rates)[
 grepl("^FREQ$",names(rates))
][1]

if(is.na(ref_col) || is.na(time_col) || is.na(value_col))
 stop("Colonnes BIS policy rates non reconnues.")

bis_rates <- rates |>
 mutate(
  country=as.character(.data[[ref_col]]),
  date=as.Date(
   paste0(substr(as.character(.data[[time_col]]),1,7),"-01")
  ),
  interest_rate=as.numeric(.data[[value_col]])
 ) |>
 filter(
  country %in% c(
   "DE","GB","IN","JP","KR","MX",
   "NO","PK","SA","TR","US","XM"
  ),
  date>=as.Date("2000-01-01"),
  date<=as.Date("2025-12-01")
 )

# mapping
map_country <- c(
 DE="DEU",
 GB="GBR",
 IN="IND",
 JP="JPN",
 KR="KOR",
 MX="MEX",
 NO="NOR",
 PK="PAK",
 SA="SAU",
 TR="TUR",
 US="USA",
 XM="DEU"
)

bis_rates <- bis_rates |>
 mutate(
  country=unname(map_country[country])
 ) |>
 filter(!is.na(country))

# Germany:
# after 1999, Germany belongs to the euro area.
# BIS euro-area policy rate is used for DEU.
# Prefer XM if available.
deu_xm <- rates |>
 mutate(
  ref=as.character(.data[[ref_col]]),
  date=as.Date(
   paste0(substr(as.character(.data[[time_col]]),1,7),"-01")
  ),
  interest_rate=as.numeric(.data[[value_col]])
 ) |>
 filter(
  ref=="XM",
  date>=as.Date("2000-01-01"),
  date<=as.Date("2025-12-01")
 ) |>
 transmute(
  country="DEU",
  date,
  interest_rate
 )

bis_rates <- bis_rates |>
 filter(country!="DEU") |>
 bind_rows(deu_xm) |>
 distinct(country,date,.keep_all=TRUE)

write_csv(
 bis_rates,
 file.path(
  root,
  "data/raw/BIS_POLICY_RATES_BULK_2000_2025.csv"
 )
)

cat("BIS policy observations :",nrow(bis_rates),"\n")

# ============================================================
# 2. TAUX EXISTANT / COMPLEMENT
# ============================================================

cat("\n[2/6] COMPLEMENT DES TAUX...\n")

old_file <- file.path(
 root,
 "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv"
)

if(file.exists(old_file)){

 old <- read_csv(old_file,show_col_types=FALSE)
 names(old) <- tolower(names(old))

 if("period" %in% names(old) && !"date" %in% names(old))
  old <- rename(old,date=period)

 if("value" %in% names(old) && !"interest_rate" %in% names(old))
  old <- rename(old,interest_rate=value)

 old <- old |>
 mutate(
  date=as.Date(
   paste0(substr(as.character(date),1,7),"-01")
  ),
  interest_rate=as.numeric(interest_rate)
 ) |>
 select(country,date,interest_rate) |>
 distinct(country,date,.keep_all=TRUE)

} else {

 old <- tibble(
  country=character(),
  date=as.Date(character()),
  interest_rate=numeric()
 )
}

interest_final <- calendar |>
 left_join(
  bis_rates,
  by=c("country","date")
 ) |>
 left_join(
  old |> rename(old_rate=interest_rate),
  by=c("country","date")
 ) |>
 mutate(
  interest_rate=coalesce(
   interest_rate,
   old_rate
  )
 ) |>
 select(country,date,interest_rate)

write_csv(
 interest_final,
 file.path(
  root,
  "data/raw/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_2000_2025.csv"
 )
)

# ============================================================
# 3. DOWNLOAD BIS CPI BULK
# ============================================================

cat("\n[3/6] TELECHARGEMENT BIS CPI...\n")

cpi_zip <- file.path(
 root,
 "data/raw/BIS_WS_LONG_CPI.zip"
)

download.file(
 "https://data.bis.org/static/bulk/WS_LONG_CPI_csv_col.zip",
 cpi_zip,
 mode="wb",
 quiet=FALSE
)

unz_cpi <- tempfile("bis_cpi_")
dir.create(unz_cpi)

unzip(
 cpi_zip,
 exdir=unz_cpi
)

cpi_files <- list.files(
 unz_cpi,
 pattern="\\.csv$",
 recursive=TRUE,
 full.names=TRUE,
 ignore.case=TRUE
)

cat("Fichiers BIS CPI :",length(cpi_files),"\n")

if(length(cpi_files)==0)
 stop("Aucun CSV BIS CPI trouve.")

cpi_file <- cpi_files[1]

cat("Lecture :",basename(cpi_file),"\n")

cpi <- read_csv(
 cpi_file,
 show_col_types=FALSE,
 guess_max=2000000
)

names(cpi) <- toupper(names(cpi))

cat("Colonnes CPI :\n")
print(names(cpi))

# ============================================================
# Identification CPI
# ============================================================

ref_col2 <- names(cpi)[
 grepl("REF_AREA|COUNTRY",names(cpi))
][1]

time_col2 <- names(cpi)[
 grepl("TIME_PERIOD|DATE|PERIOD",names(cpi))
][1]

value_col2 <- names(cpi)[
 grepl("^OBS_VALUE$|OBS_VALUE|VALUE",names(cpi))
][1]

if(is.na(ref_col2) ||
   is.na(time_col2) ||
   is.na(value_col2))
 stop("Colonnes BIS CPI non reconnues.")

# CPI all-items :
# garder la serie principale lorsqu'une dimension de
# composante existe.
cpi2 <- cpi |>
 mutate(
  ref=as.character(.data[[ref_col2]]),
  date=as.Date(
   paste0(substr(as.character(.data[[time_col2]]),1,7),"-01")
  ),
  cpi_value=as.numeric(.data[[value_col2]])
 ) |>
 filter(
  ref %in% c(
   "DE","GB","IN","JP","KR","MX",
   "NO","PK","SA","TR","US"
  ),
  date>=as.Date("1999-12-01"),
  date<=as.Date("2025-12-01")
 )

# ============================================================
# D?duplication :
# si plusieurs composantes existent, s?lectionner la s?rie
# all-items en utilisant les dimensions disponibles.
# ============================================================

# Recherche des colonnes de composante ?ventuelles
component_candidates <- names(cpi2)[
 grepl(
  "COICOP|ITEM|COMPONENT|CATEGORY|INDICATOR",
  names(cpi2)
 )
]

if(length(component_candidates)>0){

 cat(
  "Dimensions composante detectees :",
  paste(component_candidates,collapse=", "),
  "\n"
 )

 # garder les lignes o? la composante ressemble ? All items
 keep <- rep(FALSE,nrow(cpi2))

 for(cc in component_candidates){

  z <- tolower(as.character(cpi2[[cc]]))

  keep <- keep |
   grepl(
    "all|total|headline|overall|all items|general",
    z
   )
 }

 if(any(keep))
  cpi2 <- cpi2[keep,,drop=FALSE]
}

map_cpi <- c(
 DE="DEU",
 GB="GBR",
 IN="IND",
 JP="JPN",
 KR="KOR",
 MX="MEX",
 NO="NOR",
 PK="PAK",
 SA="SAU",
 TR="TUR",
 US="USA"
)

cpi2 <- cpi2 |>
 mutate(
  country=unname(map_cpi[ref])
 ) |>
 filter(!is.na(country)) |>
 select(country,date,cpi_value)

# ============================================================
# Un seul CPI par pays/mois
# ============================================================

cpi2 <- cpi2 |>
 group_by(country,date) |>
 summarise(
  cpi_value=mean(cpi_value,na.rm=TRUE),
  .groups="drop"
 ) |>
 mutate(
  cpi_value=ifelse(
   is.nan(cpi_value),
   NA_real_,
   cpi_value
  )
 )

write_csv(
 cpi2,
 file.path(
  root,
  "data/raw/BIS_CPI_ALL_ITEMS_MONTHLY_2000_2025.csv"
 )
)

cat("CPI observations :",nrow(cpi2),"\n")

# ============================================================
# 4. CALCUL INFLATION MENSUELLE
# ============================================================

cat("\n[4/6] CALCUL INFLATION MENSUELLE...\n")

inflation_final <- cpi2 |>
 group_by(country) |>
 arrange(date,.by_group=TRUE) |>
 mutate(
  inflation=100*(
   cpi_value/lag(cpi_value)-1
  )
 ) |>
 ungroup() |>
 filter(
  date>=as.Date("2000-01-01"),
  date<=as.Date("2025-12-01")
 ) |>
 select(country,date,inflation)

# Remettre les 312 mois m?me si CPI absent
inflation_final <- calendar |>
 left_join(
  inflation_final,
  by=c("country","date")
 )

write_csv(
 inflation_final,
 file.path(
  root,
  "data/raw/INFLATION_MENSUELLE_FINAL_11_PAYS_2000_2025.csv"
 )
)

# ============================================================
# 5. AUDITS
# ============================================================

cat("\n[5/6] AUDIT...\n")

audit_interest <- interest_final |>
 group_by(country) |>
 summarise(
  total=n(),
  valid=sum(is.finite(interest_rate)),
  missing=sum(!is.finite(interest_rate)),
  first_missing=ifelse(
   any(!is.finite(interest_rate)),
   as.character(min(date[!is.finite(interest_rate)])),
   NA_character_
  ),
  last_missing=ifelse(
   any(!is.finite(interest_rate)),
   as.character(max(date[!is.finite(interest_rate)])),
   NA_character_
  ),
  .groups="drop"
 )

audit_infl <- inflation_final |>
 group_by(country) |>
 summarise(
  total=n(),
  valid=sum(is.finite(inflation)),
  missing=sum(!is.finite(inflation)),
  first_missing=ifelse(
   any(!is.finite(inflation)),
   as.character(min(date[!is.finite(inflation)])),
   NA_character_
  ),
  last_missing=ifelse(
   any(!is.finite(inflation)),
   as.character(max(date[!is.finite(inflation)])),
   NA_character_
  ),
  .groups="drop"
 )

matrix_final <- calendar |>
 left_join(
  inflation_final,
  by=c("country","date")
 ) |>
 left_join(
  interest_final,
  by=c("country","date")
 ) |>
 group_by(country) |>
 summarise(
  inflation_valid=sum(is.finite(inflation)),
  inflation_missing=sum(!is.finite(inflation)),
  interest_valid=sum(is.finite(interest_rate)),
  interest_missing=sum(!is.finite(interest_rate)),
  .groups="drop"
 )

write_csv(
 audit_interest,
 file.path(
  root,
  "results/tables/AUDIT_FINAL_TAUX_INTERET.csv"
 )
)

write_csv(
 audit_infl,
 file.path(
  root,
  "results/tables/AUDIT_FINAL_INFLATION.csv"
 )
)

write_csv(
 matrix_final,
 file.path(
  root,
  "results/tables/AUDIT_FINAL_MATRICE_MACRO.csv"
 )

# ============================================================
# 6. AFFICHAGE FINAL
# ============================================================

cat("\n============================================================\n")
cat(" AUDIT TAUX D'INTERET\n")
cat("============================================================\n")
print(audit_interest,n=20)

cat("\n============================================================\n")
cat(" AUDIT INFLATION\n")
cat("============================================================\n")
print(audit_infl,n=20)

cat("\n============================================================\n")
cat(" MATRICE FINALE\n")
cat("============================================================\n")
print(matrix_final,n=20)

cat("\n============================================================\n")
cat(" IMPORTANT\n")
cat("============================================================\n")
cat("monthly_model_data.csv NON MODIFIE.\n")
cat("Aucune interpolation n'a ete effectuee.\n")
cat("Inflation = variation mensuelle du CPI All Items.\n")
cat("Taux = taux de politique monetaire BIS.\n")
cat("============================================================\n")
