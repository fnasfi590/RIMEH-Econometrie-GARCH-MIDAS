options(stringsAsFactors=FALSE,warn=1)

library(readr)
library(dplyr)

root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

outdir <- file.path(root,"results","chapter_IV_final")
dir.create(outdir,recursive=TRUE,showWarnings=FALSE)

# ============================================================
# 1. BASE PRINCIPALE
# ============================================================

monthly <- read_csv(
  file.path(root,"data/processed/monthly_clean.csv"),
  show_col_types=FALSE
)

monthly$date <- as.Date(monthly$date)

# Supprimer les anciennes variables macro vides
monthly <- monthly |>
  select(
    -any_of(c(
      "inflation",
      "interest_rate",
      "interest_rate.x",
      "interest_rate.y"
    ))
  )

# ============================================================
# 2. INFLATION CORRIGEE
# ============================================================

infl_file <- file.path(
  root,
  "data/processed/monthly_model_data_INFLATION_FIX.csv"
)

if(!file.exists(infl_file)){
  stop("Fichier inflation introuvable : ",infl_file)
}

infl <- read_csv(
  infl_file,
  show_col_types=FALSE
)

infl$date <- as.Date(infl$date)

cat("\nColonnes fichier inflation :\n")
print(names(infl))

# garder uniquement la s?rie inflation
if(!"inflation" %in% names(infl)){
  stop("La colonne inflation est absente du fichier INFLATION_FIX.")
}

infl <- infl |>
  select(country,date,inflation)

# ?viter les doublons
infl <- infl |>
  distinct(country,date,.keep_all=TRUE)

# ============================================================
# 3. TAUX D'INTERET FINAL
# ============================================================

interest <- read_csv(
  file.path(
    root,
    "results/macro_final/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_BIS_COHERENT.csv"
  ),
  show_col_types=FALSE
)

interest$date <- as.Date(interest$date)

interest <- interest |>
  select(country,date,interest_rate,source_final) |>
  distinct(country,date,.keep_all=TRUE)

# ============================================================
# 4. MERGES
# ============================================================

final <- monthly |>
  left_join(
    infl,
    by=c("country","date")
  ) |>
  left_join(
    interest,
    by=c("country","date")
  ) |>
  arrange(country,date)

# ============================================================
# 5. AUDIT VARIABLES
# ============================================================

vars <- c(
  "RV",
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX",
  "inflation",
  "interest_rate",
  "exchange_rate",
  "GDP"
)

audit <- do.call(
  rbind,
  lapply(vars,function(v){

    x <- suppressWarnings(as.numeric(final[[v]]))
    ok <- is.finite(x)

    data.frame(
      variable=v,
      N=length(x),
      valid=sum(ok),
      missing=sum(!ok),
      min=if(sum(ok)>0) min(x[ok]) else NA_real_,
      median=if(sum(ok)>0) median(x[ok]) else NA_real_,
      mean=if(sum(ok)>0) mean(x[ok]) else NA_real_,
      max=if(sum(ok)>0) max(x[ok]) else NA_real_,
      sd=if(sum(ok)>1) sd(x[ok]) else NA_real_
    )
  })
)

# ============================================================
# 6. AUDIT PAYS
# ============================================================

audit_country <- final |>
  group_by(country) |>
  summarise(
    observations=n(),
    RV=sum(is.finite(.data$RV)),
    GPRT=sum(is.finite(.data$GPRT)),
    GPRA=sum(is.finite(.data$GPRA)),
    SUP=sum(is.finite(.data$SUP)),
    DEM=sum(is.finite(.data$DEM)),
    PRE=sum(is.finite(.data$PRE)),
    VIX=sum(is.finite(.data$VIX)),
    inflation=sum(is.finite(.data$inflation)),
    interest_rate=sum(is.finite(.data$interest_rate)),
    exchange_rate=sum(is.finite(.data$exchange_rate)),
    GDP=sum(is.finite(.data$GDP)),
    .groups="drop"
  )

# ============================================================
# 7. CORRELATIONS
# ============================================================

cor_vars <- c(
  "RV",
  "GPRT",
  "GPRA",
  "SUP",
  "DEM",
  "PRE",
  "VIX",
  "inflation"
)

pearson <- cor(
  final |> select(all_of(cor_vars)),
  use="pairwise.complete.obs",
  method="pearson"
)

spearman <- cor(
  final |> select(all_of(cor_vars)),
  use="pairwise.complete.obs",
  method="spearman"
)

# ============================================================
# 8. SAUVEGARDES
# ============================================================

write_csv(
  final,
  file.path(outdir,"monthly_model_data_CHAPTER_IV_FINAL.csv")
)

write_csv(
  audit,
  file.path(outdir,"AUDIT_VARIABLES_CHAPTER_IV_FINAL.csv")
)

write_csv(
  audit_country,
  file.path(outdir,"AUDIT_PAYS_CHAPTER_IV_FINAL.csv")
)

write.csv(
  pearson,
  file.path(outdir,"MATRICE_CORRELATION_PEARSON_FINAL.csv")
)

write.csv(
  spearman,
  file.path(outdir,"MATRICE_CORRELATION_SPEARMAN_FINAL.csv")
)

# ============================================================
# 9. RESULTATS
# ============================================================

cat("\n============================================================\n")
cat(" RIMEH - BASE CHAPITRE IV CORRIGEE\n")
cat("============================================================\n")

cat("\nDimensions :",nrow(final),"x",ncol(final),"\n")

cat("\n--------------- AUDIT VARIABLES ----------------\n")
print(as.data.frame(audit),row.names=FALSE)

cat("\n--------------- AUDIT PAYS ----------------\n")
print(as.data.frame(audit_country),row.names=FALSE)

cat("\n--------------- CORRELATION PEARSON RV ----------------\n")
print(round(pearson["RV",],4))

cat("\n--------------- INFLATION ----------------\n")
print(
  final |>
    group_by(country) |>
    summarise(
      N=n(),
      valid=sum(is.finite(inflation)),
      missing=sum(!is.finite(inflation)),
      .groups="drop"
    ) |>
    as.data.frame()
)

cat("\n--------------- TAUX INTERET ----------------\n")
print(
  final |>
    group_by(country) |>
    summarise(
      N=n(),
      valid=sum(is.finite(interest_rate)),
      missing=sum(!is.finite(interest_rate)),
      .groups="drop"
    ) |>
    as.data.frame()
)

cat("\n============================================================\n")
cat(" BASE CHAPITRE IV CORRIGEE TERMINEE\n")
cat("============================================================\n")

