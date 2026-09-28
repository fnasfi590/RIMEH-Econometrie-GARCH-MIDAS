options(stringsAsFactors=FALSE,warn=1)

library(readr)
library(dplyr)
library(tidyr)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

dir.create(file.path(root,"results","macro_final"),recursive=TRUE,showWarnings=FALSE)

# ============================================================
# 1. LECTURE BIS
# ============================================================

tmp <- tempfile("BIS_")
dir.create(tmp)

unzip(
  file.path(root,"data/raw/BIS_WS_CBPOL.zip"),
  exdir=tmp
)

bis_file <- file.path(tmp,"WS_CBPOL_csv_col.csv")

bis <- read.csv(
  bis_file,
  stringsAsFactors=FALSE,
  check.names=FALSE,
  na.strings=c("","NA",".")
)

monthly_cols <- names(bis)[
  grepl("^[0-9]{4}-[0-9]{2}$",names(bis))
]

# ============================================================
# 2. EXTRACTION D'UNE SERIE BIS
# ============================================================

extract_bis <- function(ref_area,country){

  z <- bis |>
    filter(
      FREQ=="M",
      REF_AREA==ref_area
    )

  if(nrow(z)==0)
    return(
      data.frame(
        country=character(),
        date=as.Date(character()),
        interest_rate=numeric(),
        source=character(),
        stringsAsFactors=FALSE
      )
    )

  z |>
    select(all_of(monthly_cols)) |>
    pivot_longer(
      everything(),
      names_to="date",
      values_to="interest_rate"
    ) |>
    mutate(
      country=country,
      date=as.Date(paste0(date,"-01")),
      interest_rate=as.numeric(interest_rate),
      source=paste0("BIS_",ref_area)
    ) |>
    filter(
      date>=as.Date("2000-01-01"),
      date<=as.Date("2025-12-01")
    ) |>
    select(country,date,interest_rate,source)
}

# ============================================================
# 3. BIS : 10 PAYS + XM POUR DEU
# ============================================================

bis_list <- list(
  extract_bis("GB","GBR"),
  extract_bis("IN","IND"),
  extract_bis("JP","JPN"),
  extract_bis("KR","KOR"),
  extract_bis("MX","MEX"),
  extract_bis("NO","NOR"),
  extract_bis("SA","SAU"),
  extract_bis("TR","TUR"),
  extract_bis("US","USA")
)

# Allemagne = zone euro XM
deu <- extract_bis("XM","DEU") |>
  mutate(
    source="BIS_XM_EURO_AREA_PROXY_DEU"
  )

bis_all <- bind_rows(
  deu,
  bind_rows(bis_list)
)

# ============================================================
# 4. ANCIEN FICHIER FINAL
# ============================================================

old_file <- file.path(
  root,
  "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv"
)

old <- read_csv(
  old_file,
  show_col_types=FALSE
)

names(old) <- tolower(names(old))

if("period" %in% names(old))
  old <- rename(old,date=period)

if("value" %in% names(old))
  old <- rename(old,interest_rate=value)

old <- old |>
  mutate(
    date=as.Date(as.character(date)),
    interest_rate=as.numeric(interest_rate)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  )

# ============================================================
# 5. CONSTRUCTION DES 11 PAYS
# ============================================================

countries <- c(
  "DEU","GBR","IND","JPN","KOR",
  "MEX","NOR","PAK","SAU","TUR","USA"
)

calendar <- expand_grid(
  country=countries,
  date=seq(
    as.Date("2000-01-01"),
    as.Date("2025-12-01"),
    by="month"
  )
)

final <- calendar |>
  left_join(
    bis_all,
    by=c("country","date")
  ) |>
  left_join(
    old |>
      select(
        country,
        date,
        old_rate=interest_rate
      ),
    by=c("country","date")
  ) |>
  mutate(
    interest_rate=case_when(

      # BIS prioritaire
      is.finite(interest_rate) ~ interest_rate,

      # PAK : s?rie reconstruite pr?c?dente
      country=="PAK" &
        is.finite(old_rate) ~ old_rate,

      # TUR : seulement les trous BIS
      country=="TUR" &
        is.finite(old_rate) ~ old_rate,

      TRUE ~ NA_real_
    ),

    source_final=case_when(

      country=="DEU" &
        is.finite(interest_rate) &
        source=="BIS_XM_EURO_AREA_PROXY_DEU" ~
        "BIS_XM_EURO_AREA_PROXY",

      is.finite(interest_rate) &
        !is.na(source) ~ source,

      country %in% c("PAK","TUR") &
        is.finite(old_rate) ~
        "PREVIOUS_VALIDATED_SERIES",

      TRUE ~ "NA_METHODOLOGICAL_OR_UNAVAILABLE"
    )
  ) |>
  select(
    country,
    date,
    interest_rate,
    source_final
  ) |>
  arrange(country,date)

# ============================================================
# 6. AUDIT
# ============================================================

audit <- final |>
  group_by(country) |>
  summarise(
    total=n(),
    valid=sum(is.finite(interest_rate)),
    missing=sum(!is.finite(interest_rate)),
    first_valid=ifelse(
      valid>0,
      as.character(min(date[is.finite(interest_rate)])),
      NA
    ),
    last_valid=ifelse(
      valid>0,
      as.character(max(date[is.finite(interest_rate)])),
      NA
    ),
    .groups="drop"
  )

cat("\n============================================================\n")
cat(" RIMEH - TAUX D'INTERET FINAL 11 PAYS\n")
cat("============================================================\n\n")

print(audit,n=20)

cat("\n============================================================\n")
cat(" JPN : MOIS MANQUANTS\n")
cat("============================================================\n")

print(
  final |>
    filter(
      country=="JPN",
      !is.finite(interest_rate)
    ) |>
    select(date,source_final),
  n=150
)

cat("\n============================================================\n")
cat(" SOURCES FINALES\n")
cat("============================================================\n")

print(
  final |>
    count(country,source_final),
  n=100
)

# ============================================================
# 7. SAUVEGARDES
# ============================================================

write_csv(
  final,
  file.path(
    root,
    "results/macro_final/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_BIS_COHERENT.csv"
  )
)

write_csv(
  audit,
  file.path(
    root,
    "results/macro_final/AUDIT_TAUX_INTERET_FINAL_11_PAYS.csv"
  )
)

cat("\nFichiers produits :\n")
cat(" - results/macro_final/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_BIS_COHERENT.csv\n")
cat(" - results/macro_final/AUDIT_TAUX_INTERET_FINAL_11_PAYS.csv\n")

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
