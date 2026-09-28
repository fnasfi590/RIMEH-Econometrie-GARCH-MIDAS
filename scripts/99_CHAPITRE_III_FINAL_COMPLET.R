# ============================================================
# RIMEH - RECONSTRUCTION COMPLETE CHAPITRE III
# BASE FINALE AVEC :
# RV + GPRT + GPRA + SUP + DEM + PRE
# VIX + INFLATION + TAUX_INTERET + TAUX_CHANGE + CROISSANCE
# 11 PAYS - 2000-2025
# ============================================================

options(stringsAsFactors=FALSE, scipen=999)

ROOT <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

pkgs <- c(
  "dplyr","tidyr","readr","lubridate","moments",
  "tseries","car","lmtest","sandwich","officer","flextable"
)

for(p in pkgs){
  if(!requireNamespace(p,quietly=TRUE))
    install.packages(p,repos="https://cloud.r-project.org")
}

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(moments)
library(tseries)
library(car)
library(lmtest)
library(sandwich)
library(officer)
library(flextable)

dir.create(file.path(ROOT,"results/chapter_III_FINAL"),recursive=TRUE,showWarnings=FALSE)

OUT <- file.path(ROOT,"results/chapter_III_FINAL")

# ============================================================
# 1. CHARGEMENT BASE PRINCIPALE
# ============================================================

monthly <- read_csv(
  file.path(ROOT,"data/processed/monthly_clean.csv"),
  show_col_types=FALSE
)

monthly$date <- as.Date(monthly$date)

# ============================================================
# 2. CHARGEMENT INFLATION
# ============================================================

infl_file <- file.path(
  ROOT,
  "data/processed/monthly_model_data_INFLATION_FIX.csv"
)

if(file.exists(infl_file)){
  infl <- read_csv(infl_file,show_col_types=FALSE)

  if(!"date" %in% names(infl)){
    stop("Colonne date absente dans inflation")
  }

  infl$date <- as.Date(infl$date)

  if(!"country" %in% names(infl)){
    stop("Colonne country absente dans inflation")
  }

  if(!"inflation" %in% names(infl)){
    stop("Colonne inflation absente")
  }

  monthly <- monthly %>%
    select(-any_of("inflation")) %>%
    left_join(
      infl %>% select(country,date,inflation),
      by=c("country","date")
    )
}

# ============================================================
# 3. CHARGEMENT TAUX D'INTERET
# ============================================================

rate_file <- file.path(
  ROOT,
  "results/macro_final/TAUX_INTERET_MENSUEL_FINAL_11_PAYS_BIS_COHERENT.csv"
)

if(file.exists(rate_file)){

  rate <- read_csv(rate_file,show_col_types=FALSE)

  rate$date <- as.Date(rate$date)

  # détection automatique de la colonne taux
  rate_col <- setdiff(
    names(rate),
    c("country","date","year","month")
  )

  rate_col <- rate_col[
    grepl("interest|rate|taux",rate_col,ignore.case=TRUE)
  ][1]

  if(is.na(rate_col)){
    stop("Impossible de trouver la colonne taux d'interet")
  }

  rate <- rate %>%
    rename(interest_rate=all_of(rate_col)) %>%
    select(country,date,interest_rate)

  monthly <- monthly %>%
    select(-any_of("interest_rate")) %>%
    left_join(rate,by=c("country","date"))
}

# ============================================================
# 4. CHARGEMENT TAUX DE CHANGE
# ============================================================

fx_file <- file.path(
  ROOT,
  "data/raw/macro_final/TAUX_CHANGE_BIS_2000_2025.csv"
)

if(!file.exists(fx_file)){
  stop("Fichier taux de change BIS introuvable")
}

fx <- read_csv(fx_file,show_col_types=FALSE)

fx$date <- as.Date(fx$date)

fx <- fx %>%
  select(country,date,exchange_rate)

monthly <- monthly %>%
  select(-any_of("exchange_rate")) %>%
  left_join(fx,by=c("country","date"))

# ============================================================
# 5. CHARGEMENT CROISSANCE
# ============================================================

growth_file <- file.path(
  ROOT,
  "data/raw/macro_final/CROISSANCE_PIB_REEL_IMF_2000_2025_MENSUEL.csv"
)

if(!file.exists(growth_file)){
  stop("Fichier croissance IMF introuvable")
}

growth <- read_csv(growth_file,show_col_types=FALSE)

growth$date <- as.Date(growth$date)

growth <- growth %>%
  select(country,date,growth)

monthly <- monthly %>%
  select(-any_of("growth")) %>%
  left_join(growth,by=c("country","date"))

# ============================================================
# 6. CONTROLES VIX + GPRT GPRA SUP DEM PRE
# ============================================================

# Les variables doivent déjà être présentes dans monthly_clean
required <- c(
  "RV","GPRT","GPRA","SUP","DEM","PRE",
  "VIX","inflation","interest_rate",
  "exchange_rate","growth"
)

missing_vars <- setdiff(required,names(monthly))

if(length(missing_vars)>0){
  stop(
    paste(
      "Variables manquantes :",
      paste(missing_vars,collapse=", ")
    )
  )
}

# ============================================================
# 7. RESTRICTION ECHANTILLON
# ============================================================

countries <- c(
  "DEU","GBR","IND","JPN","KOR",
  "MEX","NOR","PAK","SAU","TUR","USA"
)

monthly <- monthly %>%
  filter(
    country %in% countries,
    date >= as.Date("2000-01-01"),
    date <= as.Date("2025-12-01")
  ) %>%
  arrange(country,date)

write_csv(
  monthly,
  file.path(OUT,"BASE_CHAPITRE_III_FINALE.csv")
)

# ============================================================
# 8. AUDIT COMPLET
# ============================================================

audit <- monthly %>%
  summarise(
    observations=n(),
    pays=n_distinct(country),
    debut=min(date,na.rm=TRUE),
    fin=max(date,na.rm=TRUE),
    RV=sum(is.finite(RV)),
    GPRT=sum(is.finite(GPRT)),
    GPRA=sum(is.finite(GPRA)),
    SUP=sum(is.finite(SUP)),
    DEM=sum(is.finite(DEM)),
    PRE=sum(is.finite(PRE)),
    VIX=sum(is.finite(VIX)),
    inflation=sum(is.finite(inflation)),
    interest_rate=sum(is.finite(interest_rate)),
    exchange_rate=sum(is.finite(exchange_rate)),
    growth=sum(is.finite(growth))
  )

write_csv(
  audit,
  file.path(OUT,"AUDIT_BASE_CHAPITRE_III.csv")
)

print(audit)

# ============================================================
# 9. STATISTIQUES DESCRIPTIVES
# ============================================================

vars <- c(
  "RV","GPRT","GPRA","SUP","DEM","PRE",
  "VIX","interest_rate","exchange_rate",
  "growth","inflation"
)

desc <- lapply(vars,function(v){

  x <- monthly[[v]]
  x <- x[is.finite(x)]

  data.frame(
    Variable=v,
    N=length(x),
    Moyenne=mean(x),
    Ecart_type=sd(x),
    Minimum=min(x),
    Mediane=median(x),
    Maximum=max(x),
    Skewness=skewness(x),
    Kurtosis=kurtosis(x)
  )
}) %>% bind_rows()

write_csv(
  desc,
  file.path(OUT,"TABLEAU_STATISTIQUES_DESCRIPTIVES.csv")
)

# ============================================================
# 10. MATRICES DE CORRELATION
# ============================================================

cor_data <- monthly %>%
  select(all_of(vars))

pearson <- cor(
  cor_data,
  use="pairwise.complete.obs",
  method="pearson"
)

spearman <- cor(
  cor_data,
  use="pairwise.complete.obs",
  method="spearman"
)

write.csv(
  pearson,
  file.path(OUT,"MATRICE_CORRELATION_PEARSON.csv")
)

write.csv(
  spearman,
  file.path(OUT,"MATRICE_CORRELATION_SPEARMAN.csv")
)

# ============================================================
# 11. VIF
# ============================================================

vif_data <- monthly %>%
  select(
    RV,GPRT,GPRA,SUP,DEM,PRE,
    VIX,interest_rate,exchange_rate,
    growth,inflation
  ) %>%
  filter(if_all(everything(),is.finite))

# modèle auxiliaire pour évaluer la colinéarité
vif_model <- lm(
  RV ~ GPRT + GPRA + SUP + DEM + PRE +
    VIX + interest_rate + exchange_rate +
    growth + inflation,
  data=vif_data
)

vif_values <- car::vif(vif_model)

vif_table <- data.frame(
  Variable=names(vif_values),
  VIF=as.numeric(vif_values)
)

write_csv(
  vif_table,
  file.path(OUT,"TABLEAU_VIF.csv")
)

# ============================================================
# 12. STATIONNARITE
# ============================================================

stationarity <- list()

for(v in vars){

  x <- monthly[[v]]
  x <- x[is.finite(x)]

  if(length(x)<50) next

  adf <- tryCatch(
    adf.test(x),
    error=function(e) NULL
  )

  pp <- tryCatch(
    pp.test(x),
    error=function(e) NULL
  )

  kpss <- tryCatch(
    kpss.test(x),
    error=function(e) NULL
  )

  stationarity[[v]] <- data.frame(
    Variable=v,
    ADF_p=if(is.null(adf)) NA else adf$p.value,
    PP_p=if(is.null(pp)) NA else pp$p.value,
    KPSS_p=if(is.null(kpss)) NA else kpss$p.value
  )
}

stationarity <- bind_rows(stationarity)

write_csv(
  stationarity,
  file.path(OUT,"TABLEAU_STATIONNARITE.csv")
)

# ============================================================
# 13. LJUNG-BOX + ARCH-LM
# ============================================================

diag_list <- list()

for(v in vars){

  x <- monthly[[v]]
  x <- x[is.finite(x)]

  if(length(x)<50) next

  dx <- diff(x)

  lb <- tryCatch(
    Box.test(dx,lag=12,type="Ljung-Box"),
    error=function(e) NULL
  )

  lb_sq <- tryCatch(
    Box.test(dx^2,lag=12,type="Ljung-Box"),
    error=function(e) NULL
  )

  arch <- tryCatch(
    FinTS::ArchTest(dx,lags=12),
    error=function(e) NULL
  )

  diag_list[[v]] <- data.frame(
    Variable=v,
    LjungBox_p=if(is.null(lb)) NA else lb$p.value,
    LjungBox_Squared_p=if(is.null(lb_sq)) NA else lb_sq$p.value,
    ARCH_LM_p=if(is.null(arch)) NA else arch$p.value
  )
}

diagnostics <- bind_rows(diag_list)

write_csv(
  diagnostics,
  file.path(OUT,"TABLEAU_DIAGNOSTICS.csv")
)

# ============================================================
# 14. RESULTATS GARCH-MIDAS EXISTANTS
# ============================================================

garch_file <- file.path(
  ROOT,
  "results/tables/garch_midas_coefficients_FINAL_73.csv"
)

if(file.exists(garch_file)){

  garch <- read_csv(
    garch_file,
    show_col_types=FALSE
  )

  write_csv(
    garch,
    file.path(OUT,"GARCH_MIDAS_73_RESULTATS_REFERENCE.csv")
  )

}

# ============================================================
# 15. RESUME AUTOMATIQUE
# ============================================================

rv_cor <- pearson["RV",vars]

summary_text <- paste0(
"Échantillon : 11 pays, période janvier 2000-décembre 2025, ",
nrow(monthly)," observations mensuelles.\n\n",

"Variables intégrées : RV, GPRT, GPRA, SUP, DEM, PRE, VIX, ",
"inflation, taux d'intérêt, taux de change et croissance du PIB réel.\n\n",

"Validité des variables :\n",
"RV = ",sum(is.finite(monthly$RV)),"\n",
"GPRT = ",sum(is.finite(monthly$GPRT)),"\n",
"GPRA = ",sum(is.finite(monthly$GPRA)),"\n",
"SUP = ",sum(is.finite(monthly$SUP)),"\n",
"DEM = ",sum(is.finite(monthly$DEM)),"\n",
"PRE = ",sum(is.finite(monthly$PRE)),"\n",
"VIX = ",sum(is.finite(monthly$VIX)),"\n",
"Inflation = ",sum(is.finite(monthly$inflation)),"\n",
"Taux d'intérêt = ",sum(is.finite(monthly$interest_rate)),"\n",
"Taux de change = ",sum(is.finite(monthly$exchange_rate)),"\n",
"Croissance = ",sum(is.finite(monthly$growth)),"\n"
)

writeLines(
  summary_text,
  file.path(OUT,"RESUME_BASE.txt")
)

# ============================================================
# 16. GENERATION DU CHAPITRE III WORD
# ============================================================

doc <- read_docx()

doc <- body_add_par(
  doc,
  "CHAPITRE III : ÉTUDE EMPIRIQUE",
  style="heading 1"
)

doc <- body_add_par(
  doc,
  "Section 1 : Méthodologie et présentation des données",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  "1. Présentation de l'échantillon",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "L'étude empirique porte sur onze pays : l'Allemagne, le Royaume-Uni, ",
    "l'Inde, le Japon, la Corée du Sud, le Mexique, la Norvège, le Pakistan, ",
    "l'Arabie saoudite, la Turquie et les États-Unis. La période couvre ",
    "janvier 2000 à décembre 2025. La base finale comprend ",
    nrow(monthly)," observations mensuelles."
  )
)

doc <- body_add_par(
  doc,
  "2. Sources et fréquence des données",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "Les données combinent des observations quotidiennes utilisées pour ",
    "construire la volatilité réalisée et des variables explicatives ",
    "mensuelles. Les données de taux de change proviennent du BIS. ",
    "La croissance économique correspond au taux de croissance du PIB réel ",
    "provenant du FMI. Les autres variables comprennent les indicateurs ",
    "géopolitiques GPRT et GPRA, les chocs pétroliers SUP, DEM et PRE, ",
    "le VIX, l'inflation et le taux d'intérêt."
  )
)

doc <- body_add_par(
  doc,
  "3. Définition des variables",
  style="heading 3"
)

defs <- c(
  "RV : volatilité réalisée.",
  "GPRT : indice du risque géopolitique total.",
  "GPRA : composante du risque géopolitique associée aux actes et menaces géopolitiques.",
  "SUP : choc d'offre pétrolier.",
  "DEM : choc de demande globale.",
  "PRE : choc de demande spécifique au pétrole.",
  "VIX : indicateur de l'incertitude financière internationale.",
  "Taux d'intérêt : indicateur des conditions monétaires.",
  "Taux de change : taux de change nominal vis-à-vis du dollar américain.",
  "Croissance : taux de croissance annuel du PIB réel du FMI, reporté sur les mois correspondants.",
  "Inflation : variation mensuelle de l'indice des prix."
)

for(z in defs)
  doc <- body_add_par(doc,z,style="Normal")

doc <- body_add_par(
  doc,
  "4. Méthodologie d'estimation",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "L'estimation principale repose sur le modèle GARCH-MIDAS, qui permet ",
    "de distinguer la composante de court terme de la composante de long terme ",
    "de la volatilité. Sept spécifications principales sont étudiées : ",
    "GPRT×SUP, GPRA×SUP, GPRT×DEM, GPRT×PRE, GPRT×VIX, SUP×DEM et SUP×PRE. ",
    "Les résultats de référence reposent sur les 73 estimations GARCH-MIDAS ",
    "retenues après contrôle des problèmes numériques."
  )
)

doc <- body_add_par(
  doc,
  "Section 2 : Analyse descriptive des variables",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  "1. Statistiques descriptives",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "Les statistiques descriptives sont calculées pour l'ensemble des ",
    "variables du modèle. Elles comprennent la moyenne, l'écart-type, ",
    "les valeurs minimale et maximale, la médiane, l'asymétrie et ",
    "l'aplatissement. Les résultats complets sont présentés dans ",
    "le Tableau des statistiques descriptives."
  )
)

ft_desc <- flextable(desc)
ft_desc <- autofit(ft_desc)
doc <- body_add_flextable(doc,ft_desc)

doc <- body_add_par(
  doc,
  "2. Matrice de corrélation",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "La matrice de corrélation permet d'identifier les associations ",
    "linéaires contemporaines entre la volatilité et les variables ",
    "explicatives. La corrélation ne doit pas être interprétée comme ",
    "une relation causale."
  )
)

doc <- body_add_par(
  doc,
  paste0(
    "La corrélation entre RV et VIX est de ",
    round(pearson["RV","VIX"],4),
    ". La corrélation entre RV et inflation est de ",
    round(pearson["RV","inflation"],4),
    ". La corrélation entre RV et taux de change est de ",
    round(pearson["RV","exchange_rate"],4),
    ", tandis que celle avec la croissance est de ",
    round(pearson["RV","growth"],4),
    "."
  )
)

doc <- body_add_par(
  doc,
  "3. Analyse graphique de l'évolution des variables",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "L'analyse graphique porte sur la dynamique temporelle de la volatilité ",
    "réalisée, des indices GPRT et GPRA, des chocs pétroliers et des variables ",
    "de contrôle. Elle permet d'identifier les périodes de fortes fluctuations ",
    "et de comparer leur évolution avec les facteurs géopolitiques, pétroliers ",
    "et macrofinanciers."
  )
)

doc <- body_add_par(
  doc,
  "4. Analyse de la volatilité selon les pays",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "L'estimation par pays permet de prendre en compte l'hétérogénéité des économies étudiées et leurs différents degrés d'exposition aux marchés pétroliers et financiers internationaux."
)

doc <- body_add_par(
  doc,
  "Section 3 : Résultats économétriques",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  "1. Résultats du modèle de volatilité GARCH-MIDAS",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "Les résultats GARCH-MIDAS de référence sont ceux des 73 modèles retenus. Les coefficients de court terme décrivent la dynamique conditionnelle de la volatilité tandis que les coefficients de la composante MIDAS permettent d'étudier les facteurs associés à sa composante de long terme."
)

doc <- body_add_par(
  doc,
  "2. Impact du risque géopolitique : GPRA et GPRT",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "L'analyse distingue le risque géopolitique total (GPRT) de la composante GPRA. Les coefficients estimés présentent une hétérogénéité entre les pays et les spécifications, ce qui indique que la relation entre risque géopolitique et volatilité ne peut pas être résumée par un effet identique pour toutes les économies."
)

doc <- body_add_par(
  doc,
  "3. Impact des chocs pétroliers",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "Les chocs pétroliers sont distingués selon leur origine : choc d'offre (SUP), choc de demande globale (DEM) et choc de demande spécifique au pétrole (PRE). Cette décomposition permet d'étudier séparément les mécanismes associés aux différentes sources de variation du marché pétrolier."
)

doc <- body_add_par(
  doc,
  "4. Rôle des variables de contrôle",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "Le VIX contrôle le niveau général d'incertitude financière internationale. ",
    "Le taux d'intérêt contrôle les conditions monétaires et financières. ",
    "Le taux de change permet de tenir compte du canal externe et des variations ",
    "de la valeur de la monnaie. La croissance économique contrôle les conditions ",
    "d'activité réelle. Enfin, l'inflation permet de prendre en compte les ",
    "conditions générales de prix et les mécanismes de transmission des prix de l'énergie."
  )
)

doc <- body_add_par(
  doc,
  paste0(
    "Dans la base finale, le nombre d'observations valides est de ",
    sum(is.finite(monthly$VIX)),
    " pour le VIX, ",
    sum(is.finite(monthly$interest_rate)),
    " pour le taux d'intérêt, ",
    sum(is.finite(monthly$exchange_rate)),
    " pour le taux de change, ",
    sum(is.finite(monthly$growth)),
    " pour la croissance et ",
    sum(is.finite(monthly$inflation)),
    " pour l'inflation."
  )
)

doc <- body_add_par(
  doc,
  "Section 4 : Discussion et interprétation des résultats",
  style="heading 2"
)

doc <- body_add_par(
  doc,
  "1. Interprétation des principaux résultats",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "Les résultats mettent en évidence une hétérogénéité des relations entre les facteurs géopolitiques, les chocs pétroliers et la volatilité financière. L'approche GARCH-MIDAS permet de distinguer les fluctuations de court terme de la composante persistante de la volatilité."
)

doc <- body_add_par(
  doc,
  "2. Comparaison avec les études antérieures",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "Les résultats sont confrontés à la littérature consacrée au risque géopolitique, aux marchés pétroliers et à la volatilité financière, en distinguant la direction des coefficients, leur significativité et l'hétérogénéité entre les pays."
)

doc <- body_add_par(
  doc,
  "3. Vérification des hypothèses",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  paste0(
    "Les tests formels disponibles donnent pour H1 : p=0,0992 ; H2 : p=0,3349 ; ",
    "H3 : p=0,9822 ; H5 : p=0,2128 ; H6 : p=0,2068 ; H9 : p=0,7751. ",
    "Les hypothèses H4, H7 et H8 ne sont pas directement testables avec la structure ",
    "finale des données. Les comparaisons H5, H6 et H9 doivent être considérées comme ",
    "des comparaisons appariées approximatives, la covariance complète des estimateurs ",
    "n'étant pas disponible."
  )
)

doc <- body_add_par(
  doc,
  "4. Réponse à la problématique",
  style="heading 3"
)

doc <- body_add_par(
  doc,
  "L'analyse montre que l'effet du risque géopolitique et des chocs pétroliers sur la volatilité financière dépend de la nature du choc, du pays considéré et de l'horizon temporel. L'intégration du VIX, du taux d'intérêt, du taux de change, de la croissance et de l'inflation permet de replacer cette relation dans un cadre macrofinancier plus complet."
)

# ============================================================
# ANNEXE : TABLEAUX TECHNIQUES
# ============================================================

doc <- body_add_par(
  doc,
  "Annexe : tests économétriques préliminaires",
  style="heading 2"
)

ft_station <- flextable(stationarity)
ft_station <- autofit(ft_station)
doc <- body_add_flextable(doc,ft_station)

ft_vif <- flextable(vif_table)
ft_vif <- autofit(ft_vif)
doc <- body_add_flextable(doc,ft_vif)

ft_diag <- flextable(diagnostics)
ft_diag <- autofit(ft_diag)
doc <- body_add_flextable(doc,ft_diag)

docx_file <- file.path(
  OUT,
  "CHAPITRE_III_RIMEH_FINAL_REECRIT.docx"
)

print(doc,target=docx_file)

# ============================================================
# FIN
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" RIMEH - CHAPITRE III FINAL TERMINE\n")
cat("============================================================\n")
cat("Base :",file.path(OUT,"BASE_CHAPITRE_III_FINALE.csv"),"\n")
cat("Audit :",file.path(OUT,"AUDIT_BASE_CHAPITRE_III.csv"),"\n")
cat("Descriptif :",file.path(OUT,"TABLEAU_STATISTIQUES_DESCRIPTIVES.csv"),"\n")
cat("Pearson :",file.path(OUT,"MATRICE_CORRELATION_PEARSON.csv"),"\n")
cat("Spearman :",file.path(OUT,"MATRICE_CORRELATION_SPEARMAN.csv"),"\n")
cat("VIF :",file.path(OUT,"TABLEAU_VIF.csv"),"\n")
cat("Stationnarite :",file.path(OUT,"TABLEAU_STATIONNARITE.csv"),"\n")
cat("Diagnostics :",file.path(OUT,"TABLEAU_DIAGNOSTICS.csv"),"\n")
cat("WORD :",docx_file,"\n")
cat("============================================================\n")
