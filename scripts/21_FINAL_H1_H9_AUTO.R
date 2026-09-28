options(stringsAsFactors=FALSE)
options(warn=1)

project_dir <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"
setwd(project_dir)

cat("\n============================================================\n")
cat(" RIMEH - ANALYSE FINALE AUTOMATIQUE H1-H9\n")
cat("============================================================\n\n")

dir.create("results/analysis", recursive=TRUE, showWarnings=FALSE)
dir.create("results/figures", recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. LECTURE DES RESULTATS
# ============================================================

csv <- "results/tables/garch_midas_all_results.csv"

if(!file.exists(csv))
  stop("Fichier introuvable : ", csv)

x <- read.csv(csv, check.names=FALSE)

cat("[1] Données chargées :", nrow(x), "lignes,", ncol(x), "colonnes\n")

# ============================================================
# 2. NORMALISATION DES COLONNES
# ============================================================

find_col <- function(patterns, nms) {
  for(pat in patterns) {
    z <- grep(pat,nms,ignore.case=TRUE,value=TRUE)
    if(length(z)>0) return(z[1])
  }
  return(NA_character_)
}

cc <- find_col(c("^country$","country","pays"),names(x))
mc <- find_col(c("^model$","model","spec"),names(x))
pc <- find_col(c("^parameter$","^param$","parameter","param"),names(x))
ec <- find_col(c("^estimate$","estimate","coef","coefficient"),names(x))
vc <- find_col(c("^p.value$","^pvalue$","p.value","pvalue","prob"),names(x))

if(any(is.na(c(cc,mc,pc,ec,vc)))) {
  print(names(x))
  stop("Colonnes nécessaires non détectées.")
}

x$COUNTRY <- as.character(x[[cc]])
x$MODEL <- as.character(x[[mc]])
x$PARAMETER <- as.character(x[[pc]])
x$ESTIMATE <- suppressWarnings(as.numeric(x[[ec]]))
x$P_VALUE <- suppressWarnings(as.numeric(x[[vc]]))

x$SIG5 <- !is.na(x$P_VALUE) & x$P_VALUE < .05
x$SIG10 <- !is.na(x$P_VALUE) & x$P_VALUE < .10

x$SIGN <- ifelse(
  is.na(x$ESTIMATE),"NA",
  ifelse(x$ESTIMATE>0,"POSITIVE",
         ifelse(x$ESTIMATE<0,"NEGATIVE","ZERO"))
)

# ============================================================
# 3. AUDIT GENERAL
# ============================================================

countries <- sort(unique(x$COUNTRY))
models <- sort(unique(x$MODEL))

theta <- x[grepl("^theta",x$PARAMETER,ignore.case=TRUE),]

cat("[2] Pays :",length(countries),"\n")
cat("[2] Modèles :",length(models),"\n")
cat("[2] Theta :",nrow(theta),"\n")

write.csv(
  x,
  "results/analysis/FINAL_ALL_COEFFICIENTS_AUDIT.csv",
  row.names=FALSE
)

write.csv(
  theta,
  "results/analysis/FINAL_THETA_DETAIL.csv",
  row.names=FALSE
)

# ============================================================
# 4. IDENTIFICATION DU TYPE DE MODELE
# ============================================================

theta$SHOCK <- NA_character_
theta$TARGET <- NA_character_

theta$SHOCK[grepl("^GPRT",theta$MODEL,ignore.case=TRUE)] <- "GPRT"
theta$SHOCK[grepl("^GPRA",theta$MODEL,ignore.case=TRUE)] <- "GPRA"
theta$SHOCK[grepl("^SUP",theta$MODEL,ignore.case=TRUE)] <- "SUP"

theta$TARGET[grepl("PRE$",theta$MODEL,ignore.case=TRUE)] <- "PRICE"
theta$TARGET[grepl("DEM$",theta$MODEL,ignore.case=TRUE)] <- "DEMAND"
theta$TARGET[grepl("SUP$",theta$MODEL,ignore.case=TRUE)] <- "SUPPLY"

# cas SUP_DEM / SUP_PRE
theta$TARGET[
  grepl("^SUP_DEM",theta$MODEL,ignore.case=TRUE)
] <- "DEMAND"

theta$TARGET[
  grepl("^SUP_PRE",theta$MODEL,ignore.case=TRUE)
] <- "PRICE"

# ============================================================
# 5. FONCTION DE RESUME
# ============================================================

summary_effect <- function(d,label) {

  if(nrow(d)==0) {
    return(data.frame(
      Hypothesis=label,
      N=0,
      Significant_5pct=0,
      Significant_10pct=0,
      Positive=0,
      Negative=0,
      Significant_Positive=0,
      Significant_Negative=0,
      Mean_Estimate=NA,
      Median_Estimate=NA,
      Mean_PValue=NA,
      Status="NOT TESTABLE",
      stringsAsFactors=FALSE
    ))
  }

  sig5 <- !is.na(d$P_VALUE) & d$P_VALUE < .05
  sig10 <- !is.na(d$P_VALUE) & d$P_VALUE < .10
  pos <- !is.na(d$ESTIMATE) & d$ESTIMATE > 0
  neg <- !is.na(d$ESTIMATE) & d$ESTIMATE < 0

  data.frame(
    Hypothesis=label,
    N=nrow(d),
    Significant_5pct=sum(sig5),
    Significant_10pct=sum(sig10),
    Positive=sum(pos),
    Negative=sum(neg),
    Significant_Positive=sum(sig5 & pos),
    Significant_Negative=sum(sig5 & neg),
    Mean_Estimate=mean(d$ESTIMATE,na.rm=TRUE),
    Median_Estimate=median(d$ESTIMATE,na.rm=TRUE),
    Mean_PValue=mean(d$P_VALUE,na.rm=TRUE),
    Status="DESCRIPTIVE",
    stringsAsFactors=FALSE
  )
}

# ============================================================
# 6. H1 - GPR ET VOLATILITE / PRIX
# ============================================================

H1_data <- theta[
  theta$SHOCK %in% c("GPRT","GPRA") &
  theta$TARGET=="PRICE",
]

H1 <- summary_effect(H1_data,"H1")

# ============================================================
# 7. H2 - GPR ET DEMANDE
# ============================================================

H2_data <- theta[
  theta$SHOCK %in% c("GPRT","GPRA") &
  theta$TARGET=="DEMAND",
]

H2 <- summary_effect(H2_data,"H2")

# ============================================================
# 8. H3 - GPR ET OFFRE
# ============================================================

H3_data <- theta[
  theta$SHOCK %in% c("GPRT","GPRA") &
  theta$TARGET=="SUPPLY",
]

H3 <- summary_effect(H3_data,"H3")

# ============================================================
# 9. H4 - EXPORTATEURS / IMPORTATEURS
# ============================================================

# La classification exacte des 11 pays n'est pas présente
# dans le fichier de coefficients.
# On ne fabrique donc PAS artificiellement une classification.

H4 <- data.frame(
  Hypothesis="H4",
  N=nrow(theta),
  Significant_5pct=NA,
  Significant_10pct=NA,
  Positive=NA,
  Negative=NA,
  Significant_Positive=NA,
  Significant_Negative=NA,
  Mean_Estimate=NA,
  Median_Estimate=NA,
  Mean_PValue=NA,
  Status="NOT TESTABLE - exporter/importer classification required",
  stringsAsFactors=FALSE
)

# ============================================================
# 10. H5 - DEMANDE ET HORIZON TEMPOREL
# ============================================================

H5 <- data.frame(
  Hypothesis="H5",
  N=nrow(H2_data),
  Significant_5pct=sum(H2_data$P_VALUE<.05,na.rm=TRUE),
  Significant_10pct=sum(H2_data$P_VALUE<.10,na.rm=TRUE),
  Positive=sum(H2_data$ESTIMATE>0,na.rm=TRUE),
  Negative=sum(H2_data$ESTIMATE<0,na.rm=TRUE),
  Significant_Positive=sum(H2_data$P_VALUE<.05 & H2_data$ESTIMATE>0,na.rm=TRUE),
  Significant_Negative=sum(H2_data$P_VALUE<.05 & H2_data$ESTIMATE<0,na.rm=TRUE),
  Mean_Estimate=mean(H2_data$ESTIMATE,na.rm=TRUE),
  Median_Estimate=median(H2_data$ESTIMATE,na.rm=TRUE),
  Mean_PValue=mean(H2_data$P_VALUE,na.rm=TRUE),
  Status="DESCRIPTIVE - horizon comparison required",
  stringsAsFactors=FALSE
)

# ============================================================
# 11. H6 - OFFRE ET HORIZON TEMPOREL
# ============================================================

H6 <- data.frame(
  Hypothesis="H6",
  N=nrow(H3_data),
  Significant_5pct=sum(H3_data$P_VALUE<.05,na.rm=TRUE),
  Significant_10pct=sum(H3_data$P_VALUE<.10,na.rm=TRUE),
  Positive=sum(H3_data$ESTIMATE>0,na.rm=TRUE),
  Negative=sum(H3_data$ESTIMATE<0,na.rm=TRUE),
  Significant_Positive=sum(H3_data$P_VALUE<.05 & H3_data$ESTIMATE>0,na.rm=TRUE),
  Significant_Negative=sum(H3_data$P_VALUE<.05 & H3_data$ESTIMATE<0,na.rm=TRUE),
  Mean_Estimate=mean(H3_data$ESTIMATE,na.rm=TRUE),
  Median_Estimate=median(H3_data$ESTIMATE,na.rm=TRUE),
  Mean_PValue=mean(H3_data$P_VALUE,na.rm=TRUE),
  Status="DESCRIPTIVE - horizon comparison required",
  stringsAsFactors=FALSE
)

# ============================================================
# 12. H7-H9 : GPRT vs GPRA
# ============================================================

# On peut identifier les deux familles mais une vraie comparaison
# nécessite une comparaison statistique des coefficients correspondants.

compare_shocks <- function(target,label) {

  d <- theta[
    theta$TARGET==target &
    theta$SHOCK %in% c("GPRT","GPRA"),
  ]

  if(nrow(d)==0) {
    return(data.frame(
      Hypothesis=label,
      Target=target,
      N_GPRT=0,
      N_GPRA=0,
      Mean_GPRT=NA,
      Mean_GPRA=NA,
      Difference=NA,
      Status="NOT TESTABLE"
    ))
  }

  a <- d[d$SHOCK=="GPRT","ESTIMATE"]
  b <- d[d$SHOCK=="GPRA","ESTIMATE"]

  mg <- if(length(a)>0) mean(a,na.rm=TRUE) else NA
  ma <- if(length(b)>0) mean(b,na.rm=TRUE) else NA

  data.frame(
    Hypothesis=label,
    Target=target,
    N_GPRT=length(a),
    N_GPRA=length(b),
    Mean_GPRT=mg,
    Mean_GPRA=ma,
    Difference=mg-ma,
    Status="DESCRIPTIVE - formal paired comparison required"
  )
}

H7 <- compare_shocks("PRICE","H7")
H8 <- compare_shocks("DEMAND","H8")
H9 <- compare_shocks("SUPPLY","H9")

# ============================================================
# 13. TABLEAU PRINCIPAL
# ============================================================

H1$Target <- "PRICE"
H2$Target <- "DEMAND"
H3$Target <- "SUPPLY"
H4$Target <- "EXPORTERS/IMPORTERS"
H5$Target <- "DEMAND/HORIZON"
H6$Target <- "SUPPLY/HORIZON"

H1$Target <- "PRICE"
H2$Target <- "DEMAND"
H3$Target <- "SUPPLY"

common_cols <- c(
  "Hypothesis","N","Significant_5pct","Significant_10pct",
  "Positive","Negative","Significant_Positive",
  "Significant_Negative","Mean_Estimate",
  "Median_Estimate","Mean_PValue","Status"
)

H1x <- H1[,common_cols]
H2x <- H2[,common_cols]
H3x <- H3[,common_cols]
H4x <- H4[,common_cols]
H5x <- H5[,common_cols]
H6x <- H6[,common_cols]

# H7-H9 ont une structure différente : elles sont sauvegardées séparément.

main <- rbind(H1x,H2x,H3x,H4x,H5x,H6x)

write.csv(
  main,
  "results/analysis/FINAL_H1_H6_TABLE.csv",
  row.names=FALSE
)

write.csv(
  rbind(H7,H8,H9),
  "results/analysis/FINAL_H7_H9_COMPARISON.csv",
  row.names=FALSE
)

# ============================================================
# 14. SIGNIFICATIVITE DETAILLEE PAR PAYS ET MODELE
# ============================================================

theta_detail <- theta[
  order(theta$COUNTRY,theta$MODEL,theta$PARAMETER),
]

write.csv(
  theta_detail,
  "results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv",
  row.names=FALSE
)

# ============================================================
# 15. MODELES ET PAYS
# ============================================================

model_summary <- aggregate(
  ESTIMATE ~ MODEL,
  data=theta,
  FUN=function(z) mean(z,na.rm=TRUE)
)

names(model_summary)[2] <- "Mean_Theta"

write.csv(
  model_summary,
  "results/analysis/FINAL_MODEL_THETA_SUMMARY.csv",
  row.names=FALSE
)

# ============================================================
# 16. AUDIT
# ============================================================

audit <- data.frame(
  Metric=c(
    "Coefficient rows",
    "Countries",
    "Models",
    "Theta rows",
    "Significant 5%",
    "Significant 10%",
    "Positive coefficients",
    "Negative coefficients"
  ),
  Value=c(
    nrow(x),
    length(countries),
    length(models),
    nrow(theta),
    sum(x$SIG5,na.rm=TRUE),
    sum(x$SIG10,na.rm=TRUE),
    sum(x$SIGN=="POSITIVE",na.rm=TRUE),
    sum(x$SIGN=="NEGATIVE",na.rm=TRUE)
  )
)

write.csv(
  audit,
  "results/analysis/FINAL_AUDIT.csv",
  row.names=FALSE
)

# ============================================================
# 17. RAPPORT FINAL TXT
# ============================================================

report <- "results/analysis/FINAL_H1_H9_REPORT.txt"

sink(report)

cat("============================================================\n")
cat("RIMEH - RAPPORT FINAL H1-H9\n")
cat("============================================================\n\n")

cat("Date :",as.character(Sys.time()),"\n\n")

cat("DONNEES\n")
cat("-------\n")
cat("Coefficients :",nrow(x),"\n")
cat("Pays :",length(countries),"\n")
cat("Modeles :",length(models),"\n")
cat("Theta :",nrow(theta),"\n\n")

cat("PAYS\n----\n")
print(countries)

cat("\nMODELLES\n--------\n")
print(models)

cat("\n\nRESULTATS H1-H6\n================\n")
print(main)

cat("\n\nRESULTATS H7-H9\n================\n")
print(rbind(H7,H8,H9))

cat("\n\nINTERPRETATION METHODOLOGIQUE\n")
cat("============================\n")

cat("\nH1 :\n")
cat("Analyse descriptive des effets GPR/GPRT/GPRA dans les modeles PRICE.\n")
cat("Une significativite statistique doit etre interpretee avec le signe du theta.\n")

cat("\nH2 :\n")
cat("Analyse descriptive des effets dans les modeles DEMAND.\n")

cat("\nH3 :\n")
cat("Analyse descriptive des effets dans les modeles SUPPLY.\n")

cat("\nH4 :\n")
cat("NON TESTABLE automatiquement avec le fichier actuel :\n")
cat("la classification exportateur/importateur n'est pas fournie dans les coefficients.\n")

cat("\nH5 :\n")
cat("Une analyse descriptive est disponible, mais une comparaison formelle des horizons\n")
cat("doit être définie à partir de la spécification exacte des theta.\n")

cat("\nH6 :\n")
cat("Une analyse descriptive est disponible, mais une comparaison formelle des horizons\n")
cat("doit être définie à partir de la spécification exacte des theta.\n")

cat("\nH7 :\n")
cat("GPRT vs GPRA - comparaison descriptive générée.\n")
cat("Une différence de moyennes ne constitue PAS à elle seule un test statistique.\n")

cat("\nH8 :\n")
cat("GPRT vs GPRA pour DEMAND - comparaison descriptive générée.\n")

cat("\nH9 :\n")
cat("GPRT vs GPRA pour SUPPLY - comparaison descriptive générée.\n")

cat("\n\nMODELE EN ECHEC CONNU\n")
cat("======================\n")
cat("GBR / SUP_PRE reste un échec numérique dans l'estimation initiale.\n")
cat("Il n'est pas remplacé artificiellement dans les résultats.\n")

cat("\n\nIMPORTANT SUR LES HYPOTHESES\n")
cat("============================\n")
cat("Les formulations H1-H9 utilisées dans le workflow sont une reconstruction\n")
cat("analytique et ne doivent pas être présentées comme les formulations originales\n")
cat("des hypothèses sans le document source correspondant.\n")

sink()

# ============================================================
# 18. FIN
# ============================================================

cat("\n============================================================\n")
cat(" ANALYSE FINALE TERMINEE\n")
cat("============================================================\n")

cat("\nFichiers créés :\n")
cat("1. results/analysis/FINAL_AUDIT.csv\n")
cat("2. results/analysis/FINAL_ALL_COEFFICIENTS_AUDIT.csv\n")
cat("3. results/analysis/FINAL_THETA_DETAIL.csv\n")
cat("4. results/analysis/FINAL_H1_H6_TABLE.csv\n")
cat("5. results/analysis/FINAL_H7_H9_COMPARISON.csv\n")
cat("6. results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv\n")
cat("7. results/analysis/FINAL_MODEL_THETA_SUMMARY.csv\n")
cat("8. results/analysis/FINAL_H1_H9_REPORT.txt\n")

cat("\nLe modèle GBR/SUP_PRE reste séparé et non inventé.\n")
cat("============================================================\n")
