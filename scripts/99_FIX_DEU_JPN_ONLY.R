options(stringsAsFactors=FALSE)

library(readr)
library(dplyr)
library(tidyr)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

# ============================================================
# EXTRACTION BIS
# ============================================================

tmp <- tempfile("BIS_")
dir.create(tmp)

unzip(
  file.path(root,"data/raw/BIS_WS_CBPOL.zip"),
  exdir=tmp
)

f <- file.path(
  tmp,
  "WS_CBPOL_csv_col.csv"
)

if(!file.exists(f))
  stop("WS_CBPOL_csv_col.csv introuvable.")

cat("\n============================================================\n")
cat(" LECTURE BIS\n")
cat("============================================================\n")

x <- read.csv(
  f,
  stringsAsFactors=FALSE,
  check.names=FALSE,
  na.strings=c("","NA",".")
)

cat("Dimensions :",nrow(x),"x",ncol(x),"\n")

# ============================================================
# COLONNES MENSUELLES
# ============================================================

monthly_cols <- names(x)[
  grepl(
    "^[0-9]{4}-[0-9]{2}$",
    names(x)
  )
]

cat(
  "Colonnes mensuelles :",
  length(monthly_cols),
  "\n"
)

# ============================================================
# DEU : ZONE EURO XM
# ============================================================

cat("\n============================================================\n")
cat(" DEU : SERIE XM\n")
cat("============================================================\n")

xm <- x |>
  filter(
    FREQ=="M",
    REF_AREA=="XM"
  )

cat("Lignes XM :",nrow(xm),"\n")

if(nrow(xm)>0){

  print(
    xm |>
      select(
        REF_AREA,
        any_of(c("TITLE","Series"))
      )
  )

  xm_long <- xm |>
    select(
      REF_AREA,
      all_of(monthly_cols)
    ) |>
    pivot_longer(
      cols=all_of(monthly_cols),
      names_to="date",
      values_to="rate"
    ) |>
    mutate(
      date=as.Date(
        paste0(date,"-01")
      ),
      rate=as.numeric(rate)
    ) |>
    filter(
      date>=as.Date("2000-01-01"),
      date<=as.Date("2025-12-01")
    )

  cat("\nXM valid :",sum(is.finite(xm_long$rate)),"\n")
  cat("XM total :",nrow(xm_long),"\n")

} else {

  cat("AUCUNE SERIE XM\n")

}

# ============================================================
# JPN
# ============================================================

cat("\n============================================================\n")
cat(" JPN : SERIE JP\n")
cat("============================================================\n")

jp <- x |>
  filter(
    FREQ=="M",
    REF_AREA=="JP"
  )

cat("Lignes JP :",nrow(jp),"\n")

if(nrow(jp)>0){

  print(
    jp |>
      select(
        REF_AREA,
        any_of(c("TITLE","Series"))
      )
  )

  jp_long <- jp |>
    select(
      REF_AREA,
      all_of(monthly_cols)
    ) |>
    pivot_longer(
      cols=all_of(monthly_cols),
      names_to="date",
      values_to="rate"
    ) |>
    mutate(
      date=as.Date(
        paste0(date,"-01")
      ),
      rate=as.numeric(rate)
    ) |>
    filter(
      date>=as.Date("2000-01-01"),
      date<=as.Date("2025-12-01")
    )

  cat("\nJP valid :",sum(is.finite(jp_long$rate)),"\n")
  cat("JP total :",nrow(jp_long),"\n")

  cat("\nPremiere date valide :\n")
  print(min(jp_long$date[is.finite(jp_long$rate)]))

  cat("\nDerniere date valide :\n")
  print(max(jp_long$date[is.finite(jp_long$rate)]))

} else {

  cat("AUCUNE SERIE JP\n")

}

# ============================================================
# ANCIEN FICHIER JPN
# ============================================================

cat("\n============================================================\n")
cat(" ANCIEN FICHIER IMF - JPN\n")
cat("============================================================\n")

old <- read_csv(
  file.path(
    root,
    "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv"
  ),
  show_col_types=FALSE
)

names(old) <- tolower(names(old))

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

jpn_old <- old |>
  filter(country=="JPN")

cat("JPN total IMF :",nrow(jpn_old),"\n")
cat(
  "JPN valide IMF :",
  sum(is.finite(jpn_old$interest_rate)),
  "\n"
)

if(any(is.finite(jpn_old$interest_rate))){

  cat(
    "Premiere date IMF :",
    as.character(
      min(
        jpn_old$date[
          is.finite(jpn_old$interest_rate)
        ]
      )
    ),
    "\n"
  )

  cat(
    "Derniere date IMF :",
    as.character(
      max(
        jpn_old$date[
          is.finite(jpn_old$interest_rate)
        ]
      )
    ),
    "\n"
  )

}

# ============================================================
# FUSION JPN
# ============================================================

if(exists("jp_long")){

  jpn_merge <- jp_long |>
    select(
      date,
      bis_rate=rate
    ) |>
    left_join(
      jpn_old |>
        select(
          date,
          imf_rate=interest_rate
        ),
      by="date"
    ) |>
    mutate(
      final_rate=coalesce(
        bis_rate,
        imf_rate
      )
    )

  cat("\n============================================================\n")
  cat(" JPN : BIS + IMF\n")
  cat("============================================================\n")

  print(
    jpn_merge |>
      summarise(
        total=n(),
        bis_valid=sum(is.finite(bis_rate)),
        imf_valid=sum(is.finite(imf_rate)),
        final_valid=sum(is.finite(final_rate)),
        final_missing=sum(!is.finite(final_rate))
      )
  )

  cat("\nMois encore manquants :\n")

  print(
    jpn_merge |>
      filter(!is.finite(final_rate)) |>
      select(date)
  )

}

cat("\n============================================================\n")
cat(" FIN DIAGNOSTIC\n")
cat("============================================================\n")
