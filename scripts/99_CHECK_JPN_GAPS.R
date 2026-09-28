library(readr)
library(dplyr)
library(tidyr)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

tmp <- tempfile("BIS_")
dir.create(tmp)

unzip(
  file.path(root,"data/raw/BIS_WS_CBPOL.zip"),
  exdir=tmp
)

x <- read.csv(
  file.path(tmp,"WS_CBPOL_csv_col.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE,
  na.strings=c("","NA",".")
)

monthly_cols <- names(x)[grepl("^[0-9]{4}-[0-9]{2}$",names(x))]

jp <- x |>
  filter(FREQ=="M",REF_AREA=="JP") |>
  select(all_of(monthly_cols)) |>
  pivot_longer(
    everything(),
    names_to="date",
    values_to="rate"
  ) |>
  mutate(
    date=as.Date(paste0(date,"-01")),
    rate=as.numeric(rate)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  )

cat("\n============================================================\n")
cat(" JPN - PERIODES BIS DISPONIBLES / MANQUANTES\n")
cat("============================================================\n\n")

jp |>
  mutate(
    status=ifelse(is.finite(rate),"OK","MANQUANT")
  ) |>
  group_by(status) |>
  summarise(
    n=n(),
    premiere=min(date),
    derniere=max(date),
    .groups="drop"
  ) |>
  print()

cat("\n--- BLOCS DE MOIS MANQUANTS ---\n")

missing <- jp |>
  filter(!is.finite(rate)) |>
  arrange(date)

missing |>
  mutate(
    gap=c(
      TRUE,
      diff(date)>31
    ),
    bloc=cumsum(gap)
  ) |>
  group_by(bloc) |>
  summarise(
    debut=min(date),
    fin=max(date),
    n=n(),
    .groups="drop"
  ) |>
  select(-bloc) |>
  print(n=100)

cat("\n--- OBSERVATIONS BIS AUTOUR DES PREMIERS TROUS ---\n")

jp |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2007-01-01")
  ) |>
  print(n=100)

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
