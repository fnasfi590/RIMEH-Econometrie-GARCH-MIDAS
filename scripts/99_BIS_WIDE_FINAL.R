options(stringsAsFactors=FALSE)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

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
cat(" RIMEH - BIS WIDE -> MENSUEL\n")
cat("============================================================\n")

# ============================================================
# 1. EXTRACTION DU ZIP BIS
# ============================================================

zipfile <- file.path(
  root,
  "data/raw/BIS_WS_CBPOL.zip"
)

tmp <- tempfile("BIS_CBPOL_")
dir.create(tmp)

unzip(zipfile,exdir=tmp)

bis_file <- file.path(
  tmp,
  "WS_CBPOL_csv_col.csv"
)

if(!file.exists(bis_file))
  stop("WS_CBPOL_csv_col.csv introuvable.")

# ============================================================
# 2. LECTURE DU FORMAT WIDE
# ============================================================

cat("\n[1/6] Lecture du fichier BIS wide...\n")

bis <- read.csv(
  bis_file,
  stringsAsFactors=FALSE,
  check.names=FALSE,
  na.strings=c("","NA",".")
)

cat("Dimensions :",nrow(bis),"x",ncol(bis),"\n")

cat("\nColonnes descriptives :\n")
print(names(bis)[1:min(13,ncol(bis))])

# ============================================================
# 3. IDENTIFICATION DES COLONNES MENSUELLES
# ============================================================

all_names <- names(bis)

monthly_cols <- all_names[
  grepl(
    "^[0-9]{4}-[0-9]{2}$",
    all_names
  )
]

cat(
  "\nColonnes mensuelles detectees :",
  length(monthly_cols),
  "\n"
)

if(length(monthly_cols)==0)
  stop("Aucune colonne mensuelle BIS detectee.")

# ============================================================
# 4. IDENTIFICATION DES SERIES DE POLICY RATE
# ============================================================

cat("\n[2/6] Identification des series...\n")

# Series metadata
meta_cols <- intersect(
  c(
    "FREQ",
    "Frequency",
    "REF_AREA",
    "Reference area",
    "TIME_FORMAT",
    "Time Format",
    "COMPILATION",
    "DECIMALS",
    "Decimals",
    "SOURCE_REF",
    "SUPP_INFO_BREAKS",
    "TITLE",
    "Series"
  ),
  names(bis)
)

cat("\nColonnes metadata :\n")
print(meta_cols)

# ------------------------------------------------------------
# Afficher les series BIS disponibles
# ------------------------------------------------------------

if("REF_AREA" %in% names(bis)){
  cat("\nREF_AREA disponibles :\n")
  print(
    sort(
      unique(
        bis$REF_AREA
      )
    )
  )
}

if("Series" %in% names(bis)){
  cat("\nSeries disponibles - extrait :\n")
  print(
    unique(
      bis$Series
    )[1:min(30,length(unique(bis$Series)))]
  )
}

# ============================================================
# 5. SELECTION DES PAYS
# ============================================================

# BIS utilise normalement des codes pays diff?rents
# de ceux du projet RIMEH.
#
# Mapping :
# DEU -> DE
# GBR -> GB
# IND -> IN
# JPN -> JP
# KOR -> KR
# MEX -> MX
# NOR -> NO
# PAK -> PK
# SAU -> SA
# TUR -> TR
# USA -> US

bis_codes <- c(
  DEU="DE",
  GBR="GB",
  IND="IN",
  JPN="JP",
  KOR="KR",
  MEX="MX",
  NOR="NO",
  PAK="PK",
  SAU="SA",
  TUR="TR",
  USA="US"
)

# ============================================================
# 6. FILTRE POLICY RATE
# ============================================================

cat("\n[3/6] Filtrage des policy rates...\n")

if(!"REF_AREA" %in% names(bis))
  stop("REF_AREA absent du fichier BIS.")

# FREQ devrait ?tre M pour mensuel
if("FREQ" %in% names(bis)){
  bis_sub <- bis[
    bis$FREQ=="M" &
    bis$REF_AREA %in% unname(bis_codes),
    ,
    drop=FALSE
  ]
} else {
  bis_sub <- bis[
    bis$REF_AREA %in% unname(bis_codes),
    ,
    drop=FALSE
  ]
}

cat(
  "Lignes BIS apres filtre pays/FREQ :",
  nrow(bis_sub),
  "\n"
)

if(nrow(bis_sub)==0){

  cat("\nAUCUNE LIGNE TROUVEE.\n")
  cat("Codes REF_AREA presents :\n")
  print(sort(unique(bis$REF_AREA)))

  stop("Impossible de trouver les pays BIS.")

}

# ============================================================
# 7. CONVERSION WIDE -> LONG
# ============================================================

cat("\n[4/6] Conversion wide -> long...\n")

bis_long <- bis_sub |>
  select(
    REF_AREA,
    any_of(c("TITLE","Series")),
    all_of(monthly_cols)
  ) |>
  pivot_longer(
    cols=all_of(monthly_cols),
    names_to="date",
    values_to="interest_rate"
  ) |>
  mutate(
    date=as.Date(
      paste0(
        date,
        "-01"
      )
    ),
    interest_rate=as.numeric(interest_rate)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  )

cat(
  "Observations long format :",
  nrow(bis_long),
  "\n"
)

# ============================================================
# 8. MAPPING BIS -> RIMEH
# ============================================================

bis_long <- bis_long |>
  mutate(
    country=case_when(
      REF_AREA=="DE" ~ "DEU",
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

# ============================================================
# 9. AUDIT DES SERIES
# ============================================================

cat("\n============================================================\n")
cat(" SERIES BIS TROUVEES\n")
cat("============================================================\n")

audit_bis <- bis_long |>
  group_by(country,REF_AREA) |>
  summarise(
    rows=n(),
    valid=sum(is.finite(interest_rate)),
    first_date=min(date),
    last_date=max(date),
    .groups="drop"
  )

print(audit_bis,n=100)

# ============================================================
# 10. SELECTION UNE SERIE PAR PAYS
# ============================================================

# S'il existe plusieurs lignes pour un pays,
# on prend la serie avec le plus grand nombre
# d'observations valides.

best_series <- bis_long |>
  group_by(country,REF_AREA) |>
  summarise(
    valid=sum(is.finite(interest_rate)),
    .groups="drop"
  ) |>
  arrange(
    country,
    desc(valid)
  ) |>
  group_by(country) |>
  slice(1) |>
  ungroup()

cat("\nSerie selectionnee par pays :\n")
print(best_series,n=100)

bis_selected <- bis_long |>
  inner_join(
    best_series |>
      select(country,REF_AREA),
    by=c("country","REF_AREA")
  ) |>
  select(
    country,
    date,
    interest_rate
  ) |>
  distinct(
    country,
    date,
    .keep_all=TRUE
  )

# ============================================================
# 11. CALENDRIER COMPLET 312 MOIS
# ============================================================

cat("\n[5/6] Construction du calendrier 2000-01 / 2025-12...\n")

bis_complete <- calendar |>
  left_join(
    bis_selected,
    by=c("country","date")
  )

# ============================================================
# 12. COMPLEMENT PAR ANCIEN FICHIER
# ============================================================

cat("\nComplement avec la serie historique existante uniquement\n")
cat("pour les observations BIS absentes...\n")

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
     !"date" %in% names(old))
    old <- rename(old,date=period)

  if("value" %in% names(old) &&
     !"interest_rate" %in% names(old))
    old <- rename(old,interest_rate=value)

  old <- old |>
    mutate(
      date=as.Date(
        paste0(
          substr(as.character(date),1,7),
          "-01"
        )
      ),
      interest_rate=as.numeric(interest_rate)
    ) |>
    filter(
      country %in% countries,
      date>=as.Date("2000-01-01"),
      date<=as.Date("2025-12-01")
    ) |>
    select(
      country,
      date,
      interest_rate
    ) |>
    distinct(
      country,
      date,
      .keep_all=TRUE
    )

  bis_complete <- bis_complete |>
    rename(
      bis_interest_rate=interest_rate
    ) |>
    left_join(
      old |>
        rename(
          old_interest_rate=interest_rate
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

} else {

  cat("Ancien fichier absent : aucun complement.\n")

}

# ============================================================
# 13. EXPORT FINAL
# ============================================================

write_csv(
  bis_complete,
  file.path(
    root,
    "data/raw/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_2000_2025.csv"
  )
)

# ============================================================
# 14. AUDIT FINAL
# ============================================================

cat("\n[6/6] AUDIT FINAL\n")

audit_final <- bis_complete |>
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
    min=ifelse(
      any(is.finite(interest_rate)),
      min(
        interest_rate[
          is.finite(interest_rate)
        ]
      ),
      NA_real_
    ),
    mean=ifelse(
      any(is.finite(interest_rate)),
      mean(
        interest_rate[
          is.finite(interest_rate)
        ]
      ),
      NA_real_
    ),
    max=ifelse(
      any(is.finite(interest_rate)),
      max(
        interest_rate[
          is.finite(interest_rate)
        ]
      ),
      NA_real_
    ),
    .groups="drop"
  )

print(audit_final,n=20)

write_csv(
  audit_final,
  file.path(
    root,
    "results/tables/AUDIT_FINAL_TAUX_INTERET_BIS.csv"
  )
)

cat("\n============================================================\n")
cat(" TERMINE\n")
cat("============================================================\n")

cat("\nFichier cree :\n")
cat(
  "data/raw/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_2000_2025.csv\n"
)

cat("\nAucune interpolation.\n")
cat("monthly_model_data.csv NON MODIFIE.\n")

