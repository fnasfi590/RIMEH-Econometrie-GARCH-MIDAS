library(readr)
library(dplyr)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

# ============================================================
# 1. LIRE LE FICHIER BIS DEJA GENERE
# ============================================================

bis <- read_csv(
  "data/raw/BIS_WS_CBPOL.csv",
  show_col_types=FALSE
)

# Si ce fichier n'existe pas, reconstruire depuis le CSV final
if(!file.exists("data/raw/BIS_WS_CBPOL.csv")){

  zipfile <- "data/raw/BIS_WS_CBPOL.zip"

  tmp <- tempfile("BIS_")
  dir.create(tmp)

  unzip(zipfile,exdir=tmp)

  f <- file.path(
    tmp,
    "WS_CBPOL_csv_col.csv"
  )

  x <- read.csv(
    f,
    stringsAsFactors=FALSE,
    check.names=FALSE,
    na.strings=c("","NA",".")
  )

  monthly_cols <- names(x)[
    grepl(
      "^[0-9]{4}-[0-9]{2}$",
      names(x)
    )
  ]

  bis <- x |>
    filter(
      FREQ=="M",
      REF_AREA %in% c(
        "DE","GB","IN","JP","KR","MX",
        "NO","SA","TR","US","XM"
      )
    ) |>
    select(
      REF_AREA,
      all_of(monthly_cols)
    ) |>
    pivot_longer(
      cols=all_of(monthly_cols),
      names_to="date",
      values_to="interest_rate"
    ) |>
    mutate(
      date=as.Date(
        paste0(date,"-01")
      ),
      interest_rate=as.numeric(interest_rate)
    ) |>
    filter(
      date>=as.Date("2000-01-01"),
      date<=as.Date("2025-12-01")
    )
}

# ============================================================
# 2. RELIRE DIRECTEMENT LE BIS WIDE POUR DEU + JPN
# ============================================================

tmp <- tempfile("BIS_")
dir.create(tmp)

unzip(
  "data/raw/BIS_WS_CBPOL.zip",
  exdir=tmp
)

f <- file.path(
  tmp,
  "WS_CBPOL_csv_col.csv"
)

x <- read.csv(
  f,
  stringsAsFactors=FALSE,
  check.names=FALSE,
  na.strings=c("","NA",".")
)

monthly_cols <- names(x)[
  grepl(
    "^[0-9]{4}-[0-9]{2}$",
    names(x)
  )
]

bis_target <- x |>
  filter(
    FREQ=="M",
    REF_AREA %in% c("XM","JP")
  ) |>
  select(
    REF_AREA,
    all_of(monthly_cols)
  ) |>
  pivot_longer(
    cols=all_of(monthly_cols),
    names_to="date",
    values_to="interest_rate"
  ) |>
  mutate(
    date=as.Date(
      paste0(date,"-01")
    ),
    interest_rate=as.numeric(interest_rate)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  )

cat("\n============================================================\n")
cat(" BIS DEU / XM\n")
cat("============================================================\n")

deu_bis <- bis_target |>
  filter(REF_AREA=="XM")

print(
  deu_bis |>
    summarise(
      total=n(),
      valid=sum(is.finite(interest_rate)),
      min=min(date),
      max=max(date)
    )
)

cat("\n============================================================\n")
cat(" BIS JPN\n")
cat("============================================================\n")

jpn_bis <- bis_target |>
  filter(REF_AREA=="JP")

print(
  jpn_bis |>
    summarise(
      total=n(),
      valid=sum(is.finite(interest_rate)),
      first_valid=min(date[is.finite(interest_rate)]),
      last_valid=max(date[is.finite(interest_rate)])
    )
)

# ============================================================
# 3. ANCIEN FICHIER IMF
# ============================================================

cat("\n============================================================\n")
cat(" ANCIEN FICHIER IMF\n")
cat("============================================================\n")

old <- read_csv(
  "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv",
  show_col_types=FALSE
)

names(old) <- tolower(names(old))

print(names(old))

if("period" %in% names(old))
  old <- rename(old,date=period)

if("value" %in% names(old))
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
  )

cat("\nJPN IMF :\n")

print(
  old |>
    filter(country=="JPN") |>
    summarise(
      total=n(),
      valid=sum(is.finite(interest_rate)),
      first_valid=min(date[is.finite(interest_rate)]),
      last_valid=max(date[is.finite(interest_rate)])
    )
)

cat("\nDEU IMF :\n")

print(
  old |>
    filter(country=="DEU") |>
    summarise(
      total=n(),
      valid=sum(is.finite(interest_rate))
    )
)

# ============================================================
# 4. TEST DE COMPLEMENT JPN
# ============================================================

jpn_old <- old |>
  filter(country=="JPN") |>
  select(date,old_rate=interest_rate)

jpn_check <- jpn_bis |>
  select(date,bis_rate=interest_rate) |>
  left_join(
    jpn_old,
    by="date"
  ) |>
  mutate(
    final_rate=coalesce(
      bis_rate,
      old_rate
    )
  )

cat("\n============================================================\n")
cat(" JPN APRES FUSION BIS + IMF\n")
cat("============================================================\n")

print(
  jpn_check |>
    summarise(
      total=n(),
      bis_valid=sum(is.finite(bis_rate)),
      imf_valid=sum(is.finite(old_rate)),
      final_valid=sum(is.finite(final_rate)),
      final_missing=sum(!is.finite(final_rate)),
      first_missing=ifelse(
        any(!is.finite(final_rate)),
        as.character(
          min(
            date[
              !is.finite(final_rate)
            ]
          )
        ),
        NA_character_
      ),
      last_missing=ifelse(
        any(!is.finite(final_rate)),
        as.character(
          max(
            date[
              !is.finite(final_rate)
            ]
          )
        ),
        NA_character_
      )
    )
)

cat("\n============================================================\n")
cat(" FIN DIAGNOSTIC\n")
cat("============================================================\n")
