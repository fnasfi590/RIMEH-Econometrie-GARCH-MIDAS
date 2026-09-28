options(stringsAsFactors=FALSE)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

pkgs <- c("readr","dplyr","tidyr","lubridate","httr2","jsonlite")
for(p in pkgs) if(!requireNamespace(p,quietly=TRUE)) install.packages(p,repos="https://cloud.r-project.org")

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(httr2)
library(jsonlite)

dir.create(file.path(root,"data/raw"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(root,"results/tables"),recursive=TRUE,showWarnings=FALSE)

countries <- c("DEU","GBR","IND","JPN","KOR","MEX","NOR","PAK","SAU","TUR","USA")

bis_map <- c(
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

calendar <- expand.grid(
 country=countries,
 date=seq(as.Date("2000-01-01"),as.Date("2025-12-01"),by="month")
) |>
 arrange(country,date)

cat("\n============================================================\n")
cat(" RIMEH - BIS + MACRO AUDIT CORRIGE\n")
cat("============================================================\n")

# ============================================================
# BIS CENTRAL BANK POLICY RATES
# ============================================================

get_bis <- function(cc,iso2){

  url <- paste0(
    "https://stats.bis.org/api/v1/data/WS_CBPOL/M.",
    iso2,
    "?startPeriod=2000-01&endPeriod=2025-12"
  )

  cat("   ",cc," -> ",url,"\n")

  ans <- tryCatch({

    r <- request(url) |>
      req_headers(
        Accept="application/vnd.sdmx.data+json;version=1.0.0"
      ) |>
      req_perform()

    js <- resp_body_json(r,simplifyVector=FALSE)

    ds <- js$data$dataSets[[1]]

    if(is.null(ds)) stop("dataset BIS vide")

    obs <- ds$series

    if(length(obs)==0) stop("aucune serie")

    # Pour WS_CBPOL la structure est simple:
    # observations sont dans les series.
    out <- list()

    series_keys <- names(obs)

    for(sk in series_keys){

      x <- obs[[sk]]

      if(is.null(x$observations)) next

      vals <- x$observations

      for(i in seq_along(vals)){

        # BIS SDMX JSON stocke les periodes dans structure dimensions.
        # On r?cup?re les dates depuis les dimensions temporelles
        # lorsqu'elles sont disponibles.
        out[[length(out)+1]] <- list(
          value=as.numeric(vals[[i]][[1]]),
          index=i
        )
      }
    }

    if(length(out)==0) stop("observations BIS introuvables")

    # r?cup?ration via structure temporelle
    time_values <- NULL

    if(!is.null(js$structure$dimensions$observation)){
      od <- js$structure$dimensions$observation
      td <- od[[which(vapply(
        od,
        function(z) identical(z$id,"TIME_PERIOD"),
        logical(1)
      ))]]

      if(!is.null(td) && !is.null(td$values)){
        time_values <- vapply(
          td$values,
          function(z) z$id,
          character(1)
        )
      }
    }

    if(is.null(time_values)){

      # fallback : essayer les dates dans les observations
      time_values <- seq(
        as.Date("2000-01-01"),
        by="month",
        length.out=length(out)
      )
    }

    n <- min(length(out),length(time_values))

    tibble(
      country=cc,
      date=as.Date(
        paste0(substr(as.character(time_values[seq_len(n)]),1,7),"-01")
      ),
      interest_rate=vapply(
        out[seq_len(n)],
        function(z) z$value,
        numeric(1)
      )
    ) |>
      filter(
        date>=as.Date("2000-01-01"),
        date<=as.Date("2025-12-01")
      ) |>
      distinct(country,date,.keep_all=TRUE)

  },error=function(e){
    cat("      ECHEC :",conditionMessage(e),"\n")
    tibble(
      country=character(),
      date=as.Date(character()),
      interest_rate=numeric()
    )
  })

  cat("      ",nrow(ans),"observations\n")
  ans
}

cat("\n[1] RECUPERATION BIS\n\n")

bis_list <- lapply(
  countries,
  function(cc) get_bis(cc,bis_map[[cc]])
)

bis_interest <- bind_rows(bis_list)

write_csv(
  bis_interest,
  file.path(
    root,
    "data/raw/BIS_TAUX_INTERET_MENSUEL_2000_2025.csv"
  )
)

cat("\nTOTAL BIS :",nrow(bis_interest),"observations\n")

# ============================================================
# TAUX EXISTANT
# ============================================================

cat("\n[2] FUSION BIS + TAUX EXISTANT\n\n")

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

 old$date <- as.Date(
   paste0(substr(as.character(old$date),1,7),"-01")
 )

 old <- old |>
   mutate(
     country=as.character(country),
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
   old |> rename(old_interest=interest_rate),
   by=c("country","date")
 ) |>
 left_join(
   bis_interest |> rename(bis_interest=interest_rate),
   by=c("country","date")
 ) |>
 mutate(
   interest_rate=coalesce(bis_interest,old_interest)
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
# INFLATION EXISTANTE
# ============================================================

cat("\n[3] INFLATION\n\n")

infl_file <- file.path(
 root,
 "data/raw/Inflation_Mensuelle_3027_Observations.csv"
)

if(file.exists(infl_file)){

 infl <- read_csv(infl_file,show_col_types=FALSE)
 names(infl) <- tolower(names(infl))

 dcol <- names(infl)[grepl("^date$|period|month",names(infl))][1]
 vcol <- names(infl)[grepl("inflation|value|rate",names(infl))][1]
 ccol <- names(infl)[grepl("country|iso",names(infl))][1]

 infl2 <- tibble(
  country=as.character(infl[[ccol]]),
  date=as.Date(
   paste0(substr(as.character(infl[[dcol]]),1,7),"-01")
  ),
  inflation=as.numeric(infl[[vcol]])
 ) |>
 filter(
  country %in% countries,
  date>=as.Date("2000-01-01"),
  date<=as.Date("2025-12-01")
 ) |>
 distinct(country,date,.keep_all=TRUE)

} else {

 infl2 <- tibble(
  country=character(),
  date=as.Date(character()),
  inflation=numeric()
 )
}

infl_final <- calendar |>
 left_join(infl2,by=c("country","date"))

write_csv(
 infl_final,
 file.path(
  root,
  "data/raw/INFLATION_MENSUELLE_FINAL_11_PAYS_2000_2025.csv"
 )
)

# ============================================================
# AUDIT TAUX
# ============================================================

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

# ============================================================
# AUDIT INFLATION
# ============================================================

audit_infl <- infl_final |>
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

# ============================================================
# MATRICE
# ============================================================

matrix_final <- calendar |>
 left_join(
  infl_final,
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
 matrix_final,
 file.path(
  root,
  "results/tables/AUDIT_FINAL_MATRICE_MACRO.csv"
 )
)

# ============================================================
# AFFICHAGE
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
cat(" TERMINE\n")
cat("============================================================\n")
cat("monthly_model_data.csv NON MODIFIE.\n")
cat("============================================================\n")
