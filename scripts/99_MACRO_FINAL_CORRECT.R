options(stringsAsFactors=FALSE)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

pkgs <- c("readr","dplyr","tidyr","lubridate")
for(p in pkgs){
  if(!requireNamespace(p,quietly=TRUE))
    install.packages(p,repos="https://cloud.r-project.org")
}

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

countries <- c(
  "DEU","GBR","IND","JPN","KOR","MEX",
  "NOR","PAK","SAU","TUR","USA"
)

calendar <- expand.grid(
  country=countries,
  date=seq(
    as.Date("2000-01-01"),
    as.Date("2025-12-01"),
    by="month"
  )
) |>
  arrange(country,date)

cat("\n============================================================\n")
cat(" RIMEH - MACRO FINAL : BIS + IMF\n")
cat("============================================================\n")

# ============================================================
# 1. BIS POLICY RATES
# ============================================================

cat("\n[1/5] Lecture BIS policy rates...\n")

zipfile <- file.path(
  root,
  "data/raw/BIS_WS_CBPOL.zip"
)

if(!file.exists(zipfile))
  stop("BIS_WS_CBPOL.zip introuvable.")

tmp <- tempfile("BIS_CBPOL_")
dir.create(tmp)

unzip(
  zipfile,
  exdir=tmp
)

bis_file <- file.path(
  tmp,
  "WS_CBPOL_csv_col.csv"
)

if(!file.exists(bis_file))
  stop("WS_CBPOL_csv_col.csv introuvable dans le ZIP.")

bis <- read.csv(
  bis_file,
  stringsAsFactors=FALSE,
  check.names=FALSE
)

names(bis) <- toupper(names(bis))

cat("Colonnes BIS detectees :\n")
print(names(bis))

# ------------------------------------------------------------
# Colonnes BIS
# ------------------------------------------------------------

required <- c(
  "FREQ",
  "REF_AREA",
  "TIME_PERIOD",
  "OBS_VALUE"
)

missing_cols <- setdiff(required,names(bis))

if(length(missing_cols)>0){

  cat("\nColonnes manquantes :\n")
  print(missing_cols)

  stop(
    "Structure BIS inattendue."
  )
}

# ------------------------------------------------------------
# Mapping
# ------------------------------------------------------------

bis_rates <- bis |>
  filter(
    FREQ=="M",
    REF_AREA %in% c(
      "DE","GB","IN","JP","KR","MX",
      "NO","PK","SA","TR","US","XM"
    )
  ) |>
  mutate(
    date=as.Date(
      paste0(
        substr(as.character(TIME_PERIOD),1,7),
        "-01"
      )
    ),
    interest_rate=as.numeric(OBS_VALUE)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  )

# ------------------------------------------------------------
# Allemagne :
# XM = zone euro
# ------------------------------------------------------------

bis_rates <- bis_rates |>
  mutate(
    country=case_when(
      REF_AREA=="DE" ~ "DEU",
      REF_AREA=="XM" ~ "DEU",
      REF_AREA=="GB" ~ "GBR",
      REF_AREA=="IN" ~ "IND",
      REF_AREA=="JP" ~ "JPN",
      REF_AREA=="KR" ~ "KOR",
      REF_AREA=="MX" ~ "MEX",
      REF_AREA=="NO" ~ "NOR",
      REF_AREA=="PK" ~ "PAK",
      REF_AREA=="SA" ~ "SAU",
      REF_AREA=="TR" ~ "TUR",
      REF_AREA=="US" ~ "USA",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(country))

# Pour DEU, priorit? ? XM.
deu_xm <- bis_rates |>
  filter(
    REF_AREA=="XM",
    country=="DEU"
  ) |>
  select(
    country,
    date,
    interest_rate
  )

other_rates <- bis_rates |>
  filter(
    country!="DEU"
  ) |>
  select(
    country,
    date,
    interest_rate
  )

bis_final <- bind_rows(
  deu_xm,
  other_rates
) |>
  distinct(
    country,
    date,
    .keep_all=TRUE
  )

write_csv(
  bis_final,
  file.path(
    root,
    "data/raw/BIS_POLICY_RATES_FINAL_11_PAYS.csv"
  )
)

cat(
  "Observations BIS retenues :",
  nrow(bis_final),
  "\n"
)

# ============================================================
# 2. ANCIEN TAUX IMF / PAK
# ============================================================

cat("\n[2/5] Complement avec le fichier historique existant...\n")

old_file <- file.path(
  root,
  "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv"
)

if(file.exists(old_file)){

  old <- read_csv(
    old_file,
    show_col_types=FALSE
  )

  names(old) <- tolower(names(old))

  if("period" %in% names(old) &&
     !"date" %in% names(old)){
    old <- rename(old,date=period)
  }

  if("value" %in% names(old) &&
     !"interest_rate" %in% names(old)){
    old <- rename(old,interest_rate=value)
  }

  old <- old |>
    mutate(
      country=as.character(country),
      date=as.Date(
        paste0(
          substr(as.character(date),1,7),
          "-01"
        )
      ),
      interest_rate=as.numeric(interest_rate)
    ) |>
    select(
      country,
      date,
      interest_rate
    ) |>
    filter(
      country %in% countries,
      date>=as.Date("2000-01-01"),
      date<=as.Date("2025-12-01")
    ) |>
    distinct(
      country,
      date,
      .keep_all=TRUE
    )

} else {

  old <- tibble(
    country=character(),
    date=as.Date(character()),
    interest_rate=numeric()
  )
}

# BIS prioritaire.
# Ancienne source utilis?e uniquement si BIS est absent.

interest_final <- calendar |>
  left_join(
    old |>
      rename(
        old_interest_rate=interest_rate
      ),
    by=c("country","date")
  ) |>
  left_join(
    bis_final |>
      rename(
        bis_interest_rate=interest_rate
      ),
    by=c("country","date")
  ) |>
  mutate(
    interest_rate=coalesce(
      bis_interest_rate,
      old_interest_rate
    )
  ) |>
  select(
    country,
    date,
    interest_rate
  )

write_csv(
  interest_final,
  file.path(
    root,
    "data/raw/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_2000_2025.csv"
  )
)

# ============================================================
# 3. IMF INFLATION
# ============================================================

cat("\n[3/5] Lecture inflation IMF...\n")

infl_file <- file.path(
  root,
  "data/raw/IMF_CPI_monthly.csv"
)

if(!file.exists(infl_file))
  stop("IMF_CPI_monthly.csv introuvable.")

infl <- read_csv(
  infl_file,
  show_col_types=FALSE,
  guess_max=1000000
)

names(infl) <- tolower(names(infl))

cat("Colonnes IMF CPI detectees :\n")
print(names(infl))

# IMF file poss?de d?j? une colonne inflation.
if(!"inflation" %in% names(infl))
  stop("La colonne inflation est absente du fichier IMF CPI.")

infl_final <- infl |>
  filter(
    country %in% countries,
    frequency=="M",
    coicop_1999=="_T"
  ) |>
  mutate(
    date=as.Date(
      paste0(
        gsub(
          "-M",
          "-",
          as.character(time_period)
        ),
        "-01"
      )
    ),
    inflation=as.numeric(inflation)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  ) |>
  select(
    country,
    date,
    inflation
  ) |>
  distinct(
    country,
    date,
    .keep_all=TRUE
  )

# Garantir les 312 mois
inflation_final <- calendar |>
  left_join(
    infl_final,
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
# 4. AUDIT
# ============================================================

cat("\n[4/5] Audit final...\n")

audit_interest <- interest_final |>
  group_by(country) |>
  summarise(
    total=n(),
    valid=sum(is.finite(interest_rate)),
    missing=sum(!is.finite(interest_rate)),
    first_missing=ifelse(
      any(!is.finite(interest_rate)),
      as.character(
        min(
          date[
            !is.finite(interest_rate)
          ]
        )
      ),
      NA_character_
    ),
    last_missing=ifelse(
      any(!is.finite(interest_rate)),
      as.character(
        max(
          date[
            !is.finite(interest_rate)
          ]
        )
      ),
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
      as.character(
        min(
          date[
            !is.finite(inflation)
          ]
        )
      ),
      NA_character_
    ),
    last_missing=ifelse(
      any(!is.finite(inflation)),
      as.character(
        max(
          date[
            !is.finite(inflation)
          ]
        )
      ),
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
)

# ============================================================
# 5. RESULTATS
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
cat(" FICHIERS CREES\n")
cat("============================================================\n")
cat("BIS_POLICY_RATES_FINAL_11_PAYS.csv\n")
cat("TAUX_INTERET_MENSUEL_FINAL_11_PAYS_2000_2025.csv\n")
cat("INFLATION_MENSUELLE_FINAL_11_PAYS_2000_2025.csv\n")
cat("AUDIT_FINAL_TAUX_INTERET.csv\n")
cat("AUDIT_FINAL_INFLATION.csv\n")
cat("AUDIT_FINAL_MATRICE_MACRO.csv\n")

cat("\nmonthly_model_data.csv : NON MODIFIE\n")
cat("Aucune interpolation : NON\n")

cat("\n============================================================\n")
cat(" TERMINE\n")
cat("============================================================\n")
