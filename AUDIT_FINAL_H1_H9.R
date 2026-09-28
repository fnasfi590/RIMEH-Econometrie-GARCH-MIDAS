base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

cat("\n============================================================\n")
cat(" AUDIT FINAL H1-H9\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1. Fichiers principaux
# ------------------------------------------------------------

f_detail <- file.path(
  base,
  "results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv"
)

if (!file.exists(f_detail)) {
  stop("Fichier introuvable : ", f_detail)
}

d <- read.csv(f_detail, stringsAsFactors = FALSE, check.names = FALSE)

cat("Fichier charg? :", f_detail, "\n")
cat("Dimensions :", nrow(d), "lignes x", ncol(d), "colonnes\n\n")

# ------------------------------------------------------------
# 2. V?rification des colonnes
# ------------------------------------------------------------

cat("COLONNES DISPONIBLES :\n")
print(names(d))

# ------------------------------------------------------------
# 3. H1-H3 : structure exacte
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" H1-H3 : STRUCTURE DES TESTS INDIVIDUELS\n")
cat("============================================================\n")

for (h in c("H1","H2","H3")) {

  x <- d[d$Hypothese == h, , drop = FALSE]

  cat("\n---", h, "---\n")
  cat("Nombre de tests :", nrow(x), "\n")

  if ("Modele" %in% names(x)) {
    cat("\nMod?les :\n")
    print(table(x$Modele))
  }

  if ("Pays" %in% names(x)) {
    cat("\nPays :\n")
    print(table(x$Pays))
  }

  if ("Parametre" %in% names(x)) {
    cat("\nParam?tres :\n")
    print(table(x$Parametre))
  }

  if ("p_value" %in% names(x)) {
    pv <- as.numeric(x$p_value)
    cat("\nRejet H0 ? 5% :", sum(pv < 0.05, na.rm = TRUE), "\n")
    cat("Non-rejet :", sum(pv >= 0.05, na.rm = TRUE), "\n")
    cat("P minimale :", min(pv, na.rm = TRUE), "\n")
    cat("P m?diane :", median(pv, na.rm = TRUE), "\n")
  }
}

# ------------------------------------------------------------
# 4. AUDIT H5-H6
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" H5-H6 : VERIFICATION DE LA DUPLICATION\n")
cat("============================================================\n")

h5 <- d[d$Hypothese == "H5", , drop = FALSE]
h6 <- d[d$Hypothese == "H6", , drop = FALSE]

cat("\nNombre H5 :", nrow(h5), "\n")
cat("Nombre H6 :", nrow(h6), "\n")

# colonnes communes importantes
cols_compare <- intersect(
  c("Modele","Pays","Parametre","Estimation","SE",
    "Statistique","p_value","H0","H1","Test"),
  intersect(names(h5), names(h6))
)

cat("\nColonnes compar?es :\n")
print(cols_compare)

if (nrow(h5) == nrow(h6) && nrow(h5) > 0) {

  h5c <- h5[, cols_compare, drop = FALSE]
  h6c <- h6[, cols_compare, drop = FALSE]

  # Comparaison ligne par ligne
  same <- mapply(
    function(a,b) {
      if (is.na(a) && is.na(b)) TRUE
      else if (is.na(a) || is.na(b)) FALSE
      else as.character(a) == as.character(b)
    },
    h5c,
    h6c
  )

  identical_structure <- all(same)

  cat("\nH5 et H6 sont-ils identiques sur les colonnes test?es ? : ",
      identical_structure, "\n", sep="")

  if (identical_structure) {
    cat("\n*** ALERTE : H5 ET H6 SONT DUPLIQUES ***\n")
    cat("Les deux hypoth?ses utilisent actuellement exactement\n")
    cat("les m?mes observations, mod?les et tests.\n")
  }
}

# ------------------------------------------------------------
# 5. V?rification d?taill?e H5
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" DETAIL H5\n")
cat("============================================================\n")

if (nrow(h5) > 0) {

  if ("Modele" %in% names(h5)) {
    cat("\nMod?les H5 :\n")
    print(table(h5$Modele))
  }

  if ("Pays" %in% names(h5)) {
    cat("\nPays H5 :\n")
    print(table(h5$Pays))
  }

  if ("Parametre" %in% names(h5)) {
    cat("\nParam?tres H5 :\n")
    print(table(h5$Parametre))
  }

  if ("p_value" %in% names(h5)) {
    pv <- as.numeric(h5$p_value)
    cat("\nRejets H5 :", sum(pv < 0.05, na.rm=TRUE), "\n")
    cat("P min H5 :", min(pv, na.rm=TRUE), "\n")
    cat("P m?diane H5 :", median(pv, na.rm=TRUE), "\n")
  }
}

# ------------------------------------------------------------
# 6. V?rification d?taill?e H6
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" DETAIL H6\n")
cat("============================================================\n")

if (nrow(h6) > 0) {

  if ("Modele" %in% names(h6)) {
    cat("\nMod?les H6 :\n")
    print(table(h6$Modele))
  }

  if ("Pays" %in% names(h6)) {
    cat("\nPays H6 :\n")
    print(table(h6$Pays))
  }

  if ("Parametre" %in% names(h6)) {
    cat("\nParam?tres H6 :\n")
    print(table(h6$Parametre))
  }

  if ("p_value" %in% names(h6)) {
    pv <- as.numeric(h6$p_value)
    cat("\nRejets H6 :", sum(pv < 0.05, na.rm=TRUE), "\n")
    cat("P min H6 :", min(pv, na.rm=TRUE), "\n")
    cat("P m?diane H6 :", median(pv, na.rm=TRUE), "\n")
  }
}

# ------------------------------------------------------------
# 7. H9
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" H9 : VERIFICATION GPRT VS GPRA\n")
cat("============================================================\n")

h9 <- d[d$Hypothese == "H9", , drop = FALSE]

cat("Nombre de tests H9 :", nrow(h9), "\n")

if ("Modele" %in% names(h9)) {
  cat("\nMod?les :\n")
  print(table(h9$Modele))
}

if ("Pays" %in% names(h9)) {
  cat("\nPays :\n")
  print(table(h9$Pays))
}

if ("Parametre" %in% names(h9)) {
  cat("\nParam?tres :\n")
  print(table(h9$Parametre))
}

if ("p_value" %in% names(h9)) {
  pv <- as.numeric(h9$p_value)

  cat("\nRejets H0 ? 5% :", sum(pv < 0.05, na.rm=TRUE), "\n")
  cat("Non-rejets :", sum(pv >= 0.05, na.rm=TRUE), "\n")
  cat("P minimale :", min(pv, na.rm=TRUE), "\n")
  cat("P m?diane :", median(pv, na.rm=TRUE), "\n")
}

# ------------------------------------------------------------
# 8. H4-H7-H8
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" H4 / H7 / H8 : VERIFICATION DES FICHIERS FORMELS\n")
cat("============================================================\n")

formal_files <- c(
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H4.csv",
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H7_H8.csv",
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H9_GPRT_GPRA_DETAIL.csv",
  "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv",
  "results/analysis/FORMAL_H1_H9/H9_GPRT_VS_GPRA_DETAIL.csv"
)

for (f in formal_files) {

  path <- file.path(base, f)

  cat("\nFichier :", f, "\n")

  if (file.exists(path)) {

    z <- tryCatch(
      read.csv(path, stringsAsFactors=FALSE, check.names=FALSE),
      error=function(e) NULL
    )

    if (is.null(z)) {
      cat("Lecture impossible.\n")
    } else {
      cat("Existe : OUI\n")
      cat("Dimensions :", nrow(z), "x", ncol(z), "\n")
      cat("Colonnes :\n")
      print(names(z))
    }

  } else {
    cat("Existe : NON\n")
  }
}

# ------------------------------------------------------------
# 9. R?sum? final
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" RESUME FINAL DE L'AUDIT\n")
cat("============================================================\n")

cat("\nH1 : 13 tests individuels\n")
cat("H2 : 44 tests individuels\n")
cat("H3 : 42 tests individuels\n")
cat("H4 : v?rifier le fichier formel\n")
cat("H5 : 64 tests de diff?rence sous ind?pendance\n")
cat("H6 : 64 tests de diff?rence sous ind?pendance\n")
cat("H7 : non testable formellement\n")
cat("H8 : non testable formellement\n")
cat("H9 : 21 tests de diff?rence sous ind?pendance\n")

cat("\n*** FIN AUDIT ***\n")
