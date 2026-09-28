base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

cat("\n============================================================\n")
cat("        AUDIT FINAL H1-H9 - VERSION 2\n")
cat("============================================================\n\n")

# ============================================================
# 1. FICHIER PRINCIPAL
# ============================================================

f <- file.path(
  base,
  "results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv"
)

d <- read.csv(
  f,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("Fichier principal :", f, "\n")
cat("Dimensions :", nrow(d), "x", ncol(d), "\n\n")

# Normalisation des noms
names(d) <- tolower(names(d))

# ============================================================
# 2. H1-H3 PAR MODELE
# ============================================================

cat("\n============================================================\n")
cat(" H1-H3 : RECONSTRUCTION PAR MODELE\n")
cat("============================================================\n")

# H1 = GPRT_VIX
h1 <- d[d$model == "GPRT_VIX", , drop=FALSE]

# H2 = GPRT_DEM + SUP_DEM
h2 <- d[d$model %in% c("GPRT_DEM","SUP_DEM"), , drop=FALSE]

# H3 = GPRA_SUP + GPRT_SUP
h3 <- d[d$model %in% c("GPRA_SUP","GPRT_SUP"), , drop=FALSE]

audit_basic <- function(x, nom) {

  cat("\n---------------- ", nom, " ----------------\n", sep="")
  cat("Nombre de tests :", nrow(x), "\n")

  if (nrow(x) == 0) return(NULL)

  cat("\nMod?les :\n")
  print(table(x$model))

  cat("\nPays :\n")
  print(table(x$country))

  cat("\nParam?tres :\n")
  print(table(x$parameter))

  pv <- as.numeric(x$p_value)

  cat("\nRejets H0 ? 5% :", sum(pv < 0.05, na.rm=TRUE), "\n")
  cat("Non-rejets :", sum(pv >= 0.05, na.rm=TRUE), "\n")
  cat("P minimale :", min(pv, na.rm=TRUE), "\n")
  cat("P m?diane :", median(pv, na.rm=TRUE), "\n")

  cat("\nLignes significatives :\n")
  print(
    x[pv < 0.05,
      intersect(
        c("country","model","parameter","estimate",
          "std_error","t_value","p_value","shock","target"),
        names(x)
      ),
      drop=FALSE
    ]
  )
}

audit_basic(h1, "H1")
audit_basic(h2, "H2")
audit_basic(h3, "H3")

# ============================================================
# 3. H5/H6
# ============================================================

cat("\n============================================================\n")
cat(" H5-H6 : AUDIT DU FICHIER PAIRED THETA\n")
cat("============================================================\n")

f56 <- file.path(
  base,
  "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv"
)

if (!file.exists(f56)) {

  cat("FICHIER H5/H6 INTROUVABLE\n")

} else {

  x56 <- read.csv(
    f56,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  cat("Dimensions :", nrow(x56), "x", ncol(x56), "\n")

  cat("\nMod?les :\n")
  print(table(x56$model))

  cat("\nPays :\n")
  print(table(x56$country))

  cat("\nParam?tres pr?sents :\n")
  cat("theta_1 :", sum(!is.na(x56$estimate.theta_1)), "\n")
  cat("theta_2 :", sum(!is.na(x56$estimate.theta_2)), "\n")

  pv <- as.numeric(x56$p_ind)

  cat("\nNombre de diff?rences significatives ? 5% :",
      sum(pv < 0.05, na.rm=TRUE), "\n")

  cat("P minimale :", min(pv, na.rm=TRUE), "\n")
  cat("P m?diane :", median(pv, na.rm=TRUE), "\n")

  cat("\n--- LIGNES SIGNIFICATIVES ---\n")

  print(
    x56[
      pv < 0.05,
      c(
        "country",
        "model",
        "estimate.theta_1",
        "estimate.theta_2",
        "diff",
        "se_ind",
        "z_ind",
        "p_ind"
      ),
      drop=FALSE
    ]
  )

  cat("\n--- STRUCTURE DES TESTS ---\n")

  cat(
    "Test utilis? : comparaison theta_1 - theta_2\n",
    "SE utilis?e : sqrt(SE(theta_1)^2 + SE(theta_2)^2)\n",
    "Hypoth?se de covariance nulle entre theta_1 et theta_2.\n",
    sep=""
  )
}

# ============================================================
# 4. H9
# ============================================================

cat("\n============================================================\n")
cat(" H9 : AUDIT GPRT VS GPRA\n")
cat("============================================================\n")

f9 <- file.path(
  base,
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H9_GPRT_GPRA_DETAIL.csv"
)

if (!file.exists(f9)) {

  cat("FICHIER H9 INTROUVABLE\n")

} else {

  x9 <- read.csv(
    f9,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  cat("Dimensions :", nrow(x9), "x", ncol(x9), "\n")

  cat("\nPays :\n")
  print(table(x9$country))

  cat("\nParam?tres :\n")
  print(table(x9$parameter))

  pv <- as.numeric(x9$p)

  cat("\nRejets ? 5% :", sum(pv < 0.05, na.rm=TRUE), "\n")
  cat("Non-rejets :", sum(pv >= 0.05, na.rm=TRUE), "\n")
  cat("P minimale :", min(pv, na.rm=TRUE), "\n")
  cat("P m?diane :", median(pv, na.rm=TRUE), "\n")

  cat("\n--- LIGNES SIGNIFICATIVES ---\n")

  print(
    x9[
      pv < 0.05,
      c(
        "country",
        "parameter",
        "estimate.GPRA_SUP",
        "estimate.GPRT_SUP",
        "diff",
        "se_diff",
        "z",
        "p"
      ),
      drop=FALSE
    ]
  )
}

# ============================================================
# 5. H4
# ============================================================

cat("\n============================================================\n")
cat(" H4 : AUDIT\n")
cat("============================================================\n")

f4 <- file.path(
  base,
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H4.csv"
)

if (file.exists(f4)) {

  h4 <- read.csv(
    f4,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  print(h4)

} else {
  cat("Fichier H4 introuvable\n")
}

# ============================================================
# 6. H7-H8
# ============================================================

cat("\n============================================================\n")
cat(" H7-H8 : AUDIT\n")
cat("============================================================\n")

f78 <- file.path(
  base,
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H7_H8.csv"
)

if (file.exists(f78)) {

  h78 <- read.csv(
    f78,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  print(h78)

} else {
  cat("Fichier H7-H8 introuvable\n")
}

# ============================================================
# 7. VERIFICATION H9 VERSION INDEPENDANCE
# ============================================================

cat("\n============================================================\n")
cat(" H9 : VERIFICATION VERSION INDEPENDANCE\n")
cat("============================================================\n")

f9i <- file.path(
  base,
  "results/analysis/FORMAL_H1_H9/H9_GPRT_VS_GPRA_DETAIL.csv"
)

if (file.exists(f9i)) {

  h9i <- read.csv(
    f9i,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  cat("Dimensions :", nrow(h9i), "x", ncol(h9i), "\n")

  pv <- h9i$p_ind

  cat("Rejets :", sum(pv < 0.05, na.rm=TRUE), "\n")
  cat("P min :", min(pv, na.rm=TRUE), "\n")
  cat("P m?diane :", median(pv, na.rm=TRUE), "\n")

  cat("\nLignes significatives :\n")

  print(
    h9i[
      pv < 0.05,
      ,
      drop=FALSE
    ]
  )

} else {
  cat("Fichier H9 ind?pendance introuvable\n")
}

# ============================================================
# 8. CONCLUSION AUTOMATIQUE
# ============================================================

cat("\n============================================================\n")
cat(" CONCLUSION DE L'AUDIT\n")
cat("============================================================\n")

cat("
H1 : tests individuels sur GPRT_VIX
H2 : tests individuels sur GPRT_DEM + SUP_DEM
H3 : tests individuels sur GPRA_SUP + GPRT_SUP

H5/H6 : fichier commun de 64 comparaisons theta_1 - theta_2.
        Il faut v?rifier la d?finition th?orique distincte de H5 et H6
        avant de pr?senter deux conclusions s?par?es.

H9 : comparaison GPRT vs GPRA par pays et param?tre.

ATTENTION :
Les tests H5/H6 et H9 utilisent une SE sous hypoth?se
d'ind?pendance/covariance nulle. Ils ne constituent pas un
test de Wald exact si la covariance entre estimateurs est disponible
mais non utilis?e.

Une moyenne de p-values ne constitue PAS une p-value globale.
")

cat("\n============================================================\n")
cat(" FIN AUDIT FINAL V2\n")
cat("============================================================\n")
