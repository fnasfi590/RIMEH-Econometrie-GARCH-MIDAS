base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"
outdir <- file.path(base, "results/validation/H1_H9_EXACT_AUDIT")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

read_safe <- function(f) {
  if (!file.exists(f)) return(NULL)
  tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

getcol <- function(x, patterns) {
  if (is.null(x)) return(NULL)
  n <- names(x)
  for (p in patterns) {
    z <- grep(p, n, ignore.case = TRUE, value = TRUE)
    if (length(z) > 0) return(z[1])
  }
  NULL
}

num <- function(x) {
  suppressWarnings(as.numeric(x))
}

first_valid <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

mean_valid <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

# ---------------------------------------------------------------
# FICHIERS
# ---------------------------------------------------------------

h13 <- read_safe(file.path(base, "results/analysis/FINAL_H1_H3_CORRECTED.csv"))
h13r <- read_safe(file.path(base, "results/analysis/FINAL_H1_H3_TABLE_ROBUST.csv"))

h4 <- read_safe(file.path(base, "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H4.csv"))
h4f <- read_safe(file.path(base, "results/analysis/FORMAL_H1_H9/FORMAL_H4.csv"))

h5 <- read_safe(file.path(base, "results/analysis/FINAL_H5_DETAIL.csv"))
h5r <- read_safe(file.path(base, "results/analysis/FINAL_H5_HORIZON_DETAIL_ROBUST.csv"))

h6 <- read_safe(file.path(base, "results/analysis/FINAL_H6_DETAIL.csv"))
h6r <- read_safe(file.path(base, "results/analysis/FINAL_H6_HORIZON_DETAIL_ROBUST.csv"))

h7 <- read_safe(file.path(base, "results/analysis/FINAL_H7_DETAIL_ROBUST.csv"))

h78 <- read_safe(file.path(base, "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H7_H8.csv"))

h9 <- read_safe(file.path(base, "results/analysis/FINAL_H9_DETAIL.csv"))
h9r <- read_safe(file.path(base, "results/analysis/FINAL_H9_DETAIL_ROBUST.csv"))
h9f <- read_safe(file.path(base, "results/analysis/FORMAL_H1_H9/H9_GPRT_VS_GPRA_DETAIL.csv"))

paired <- read_safe(file.path(base, "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv"))

# ---------------------------------------------------------------
# FONCTION POUR EXTRAIRE ESTIMATION / SE / P
# ---------------------------------------------------------------

extract_values <- function(x, estimate_patterns, p_patterns, se_patterns = NULL) {

  ec <- getcol(x, estimate_patterns)
  pc <- getcol(x, p_patterns)

  sc <- NULL
  if (!is.null(se_patterns))
    sc <- getcol(x, se_patterns)

  est <- if (!is.null(ec)) num(x[[ec]]) else numeric(0)
  pv  <- if (!is.null(pc)) num(x[[pc]]) else numeric(0)
  se  <- if (!is.null(sc)) num(x[[sc]]) else numeric(0)

  est <- est[is.finite(est)]
  pv  <- pv[is.finite(pv)]
  se  <- se[is.finite(se)]

  list(
    estimate = if(length(est)) mean(est) else NA_real_,
    p = if(length(pv)) mean(pv) else NA_real_,
    se = if(length(se)) mean(se) else NA_real_,
    estimate_col = ec,
    p_col = pc,
    se_col = sc
  )
}

# ---------------------------------------------------------------
# INITIALISATION
# ---------------------------------------------------------------

results <- data.frame(
  Hypothese = character(),
  Parametre = character(),
  H0 = character(),
  H1 = character(),
  Test = character(),
  Estimation = numeric(),
  SE = numeric(),
  Statistique = numeric(),
  P_value = numeric(),
  Decision_5pct = character(),
  stringsAsFactors = FALSE
)

add_result <- function(hyp, param, h0, h1, test, est, se, stat, p, decision) {
  results <<- rbind(
    results,
    data.frame(
      Hypothese = hyp,
      Parametre = param,
      H0 = h0,
      H1 = h1,
      Test = test,
      Estimation = est,
      SE = se,
      Statistique = stat,
      P_value = p,
      Decision_5pct = decision,
      stringsAsFactors = FALSE
    )
  )
}

decision5 <- function(p) {
  if (!is.finite(p)) return("NON TESTABLE / p-value indisponible")
  if (p < 0.01) return("Rejet de H0 ? 1%")
  if (p < 0.05) return("Rejet de H0 ? 5%")
  "Non-rejet de H0 ? 5%"
}

# ---------------------------------------------------------------
# H1-H3
# ---------------------------------------------------------------

if (!is.null(h13)) {

  hc <- getcol(h13, c("^Hypothesis$", "^Hypothese$", "H[1-3]"))
  ec <- getcol(h13, c("^Mean_Estimate$", "Estimate"))
  pc <- getcol(h13, c("^Mean_PValue$", "P.?Value", "p.?value"))
  sc <- getcol(h13, c("SE", "Std", "Error"))

  for (i in 1:3) {

    est <- if (!is.null(ec)) first_valid(h13[[ec]][i]) else NA
    p   <- if (!is.null(pc)) first_valid(h13[[pc]][i]) else NA
    se  <- if (!is.null(sc)) first_valid(h13[[sc]][i]) else NA

    stat <- if (is.finite(est) && is.finite(se) && se != 0)
      est / se else NA

    add_result(
      paste0("H", i),
      "Theta / coefficient MIDAS",
      "Param?tre = 0",
      "Param?tre ? 0 / effet conforme ? l'hypoth?se",
      "Test de significativit? du coefficient (Wald / t-z selon estimation)",
      est, se, stat, p, decision5(p)
    )
  }
}

# ---------------------------------------------------------------
# H4
# ---------------------------------------------------------------

if (!is.null(h4)) {

  ec <- getcol(h4, c("Estimate", "estimate"))
  pc <- getcol(h4, c("p.?value", "P_value", "PValue"))
  sc <- getcol(h4, c("SE", "Std", "Error"))
  tc <- getcol(h4, c("stat", "z", "t"))

  est <- first_valid(if(!is.null(ec)) h4[[ec]] else NULL)
  p   <- first_valid(if(!is.null(pc)) h4[[pc]] else NULL)
  se  <- first_valid(if(!is.null(sc)) h4[[sc]] else NULL)
  stat <- first_valid(if(!is.null(tc)) h4[[tc]] else NULL)

  if (!is.finite(stat) && is.finite(est) && is.finite(se) && se != 0)
    stat <- est/se

  add_result(
    "H4",
    "Param?tre H4",
    "Param?tre = 0",
    "Param?tre ? 0 / effet conforme ? H4",
    "Test formel de significativit?",
    est, se, stat, p, decision5(p)
  )

} else {

  add_result(
    "H4",
    "Param?tre H4",
    "Param?tre = 0",
    "Param?tre ? 0",
    "Non testable : r?sultat formel absent",
    NA, NA, NA, NA,
    "NON TESTABLE"
  )
}

# ---------------------------------------------------------------
# H5 / H6
# ---------------------------------------------------------------

# Cherche d'abord les r?sultats appari?s.
if (!is.null(paired)) {

  # Affichage des colonnes pour tra?abilit?
  write.csv(
    paired,
    file.path(outdir, "SOURCE_H5_H6_PAIRED.csv"),
    row.names = FALSE
  )

  # Tentative d'extraction theta 1 / theta 2
  e1c <- getcol(paired, c("estimate.theta_1", "Theta1"))
  e2c <- getcol(paired, c("estimate.theta_2", "Theta2"))
  s1c <- getcol(paired, c("se.theta_1", "SE.*theta.*1"))
  s2c <- getcol(paired, c("se.theta_2", "SE.*theta.*2"))

  e1 <- if(!is.null(e1c)) num(paired[[e1c]]) else numeric(0)
  e2 <- if(!is.null(e2c)) num(paired[[e2c]]) else numeric(0)

  s1 <- if(!is.null(s1c)) num(paired[[s1c]]) else numeric(0)
  s2 <- if(!is.null(s2c)) num(paired[[s2c]]) else numeric(0)

  ok <- is.finite(e1) & is.finite(e2)

  if (sum(ok) > 0) {

    diff <- e1[ok] - e2[ok]

    # Si les SE sont disponibles et ind?pendants,
    # approximation de la variance de diff?rence.
    if(length(s1) == length(e1) && length(s2) == length(e2)) {

      s1o <- s1[ok]
      s2o <- s2[ok]

      se_diff <- sqrt(s1o^2 + s2o^2)

      z <- diff / se_diff
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)

      keep <- is.finite(z) & is.finite(p)

      if(sum(keep) > 0) {

        zmean <- mean(z[keep])
        pmean <- mean(p[keep])
        dmean <- mean(diff[keep])
        semean <- mean(se_diff[keep])

        add_result(
          "H5",
          "Theta1 - Theta2",
          "Theta1 = Theta2",
          "Theta1 ? Theta2 selon H5",
          "Test de diff?rence entre deux param?tres (approximation Wald)",
          dmean, semean, zmean, pmean, decision5(pmean)
        )

        add_result(
          "H6",
          "Theta1 - Theta2",
          "Theta1 = Theta2",
          "Theta1 ? Theta2 selon H6",
          "Test de diff?rence entre deux param?tres (approximation Wald)",
          dmean, semean, zmean, pmean, decision5(pmean)
        )

      } else {

        add_result(
          "H5",
          "Theta1 - Theta2",
          "Theta1 = Theta2",
          "Theta1 ? Theta2",
          "Test de Wald exact",
          NA, NA, NA, NA,
          "NON TESTABLE : covariance n?cessaire"
        )

        add_result(
          "H6",
          "Theta1 - Theta2",
          "Theta1 = Theta2",
          "Theta1 ? Theta2",
          "Test de Wald exact",
          NA, NA, NA, NA,
          "NON TESTABLE : covariance n?cessaire"
        )
      }

    } else {

      add_result(
        "H5",
        "Theta1 - Theta2",
        "Theta1 = Theta2",
        "Theta1 ? Theta2",
        "Test de Wald",
        NA, NA, NA, NA,
        "NON TESTABLE : erreurs standards manquantes"
      )

      add_result(
        "H6",
        "Theta1 - Theta2",
        "Theta1 = Theta2",
        "Theta1 ? Theta2",
        "Test de Wald",
        NA, NA, NA, NA,
        "NON TESTABLE : erreurs standards manquantes"
      )
    }

  } else {

    add_result(
      "H5",
      "Theta1 - Theta2",
      "Theta1 = Theta2",
      "Theta1 ? Theta2",
      "Test de Wald",
      NA, NA, NA, NA,
      "NON TESTABLE"
    )

    add_result(
      "H6",
      "Theta1 - Theta2",
      "Theta1 = Theta2",
      "Theta1 ? Theta2",
      "Test de Wald",
      NA, NA, NA, NA,
      "NON TESTABLE"
    )
  }

} else {

  add_result(
    "H5",
    "Theta1 - Theta2",
    "Theta1 = Theta2",
    "Theta1 ? Theta2",
    "Test de Wald",
    NA, NA, NA, NA,
    "NON TESTABLE : fichier appari? absent"
  )

  add_result(
    "H6",
    "Theta1 - Theta2",
    "Theta1 = Theta2",
    "Theta1 ? Theta2",
    "Test de Wald",
    NA, NA, NA, NA,
    "NON TESTABLE : fichier appari? absent"
  )
}

# ---------------------------------------------------------------
# H7 / H8
# ---------------------------------------------------------------

if (!is.null(h78)) {

  print(h78)

  status_col <- getcol(h78, c("status", "Status", "decision", "reason"))
  pcol <- getcol(h78, c("p.?value", "P_value"))

  # Recherche ligne H7
  for (hh in c("H7", "H8")) {

    row <- NULL

    for (j in seq_len(nrow(h78))) {
      txt <- paste(as.character(h78[j,]), collapse=" ")
      if (grepl(hh, txt, ignore.case=TRUE)) {
        row <- j
        break
      }
    }

    if (!is.null(row)) {

      p <- if(!is.null(pcol)) first_valid(h78[[pcol]][row]) else NA

      add_result(
        hh,
        if(hh=="H7") "GPRT / GPRA ? structure PRICE" else "GPRT / GPRA ? structure DEMAND",
        "Relation nulle / absence de diff?rence",
        "Relation conforme ? l'hypoth?se",
        "Test formel de comparaison",
        NA, NA, NA, p,
        if(is.finite(p)) decision5(p) else "NON TESTABLE"
      )

    } else {

      add_result(
        hh,
        if(hh=="H7") "GPRT / GPRA ? PRICE" else "GPRT / GPRA ? DEMAND",
        "Relation nulle",
        "Relation conforme ? l'hypoth?se",
        "Test formel",
        NA, NA, NA, NA,
        "NON TESTABLE"
      )
    }
  }

} else {

  add_result(
    "H7",
    "GPRT / GPRA ? PRICE",
    "Relation nulle",
    "Relation conforme ? H7",
    "Test formel",
    NA, NA, NA, NA,
    "NON TESTABLE"
  )

  add_result(
    "H8",
    "GPRT / GPRA ? DEMAND",
    "Relation nulle",
    "Relation conforme ? H8",
    "Test formel",
    NA, NA, NA, NA,
    "NON TESTABLE"
  )
}

# ---------------------------------------------------------------
# H9
# ---------------------------------------------------------------

if (!is.null(h9f)) {

  gprtc <- getcol(h9f, c("estimate.GPRT_SUP", "GPRT.*Estimate"))
  gprac <- getcol(h9f, c("estimate.GPRA_SUP", "GPRA.*Estimate"))

  if(!is.null(gprtc) && !is.null(gprac)) {

    a <- num(h9f[[gprtc]])
    b <- num(h9f[[gprac]])

    ok <- is.finite(a) & is.finite(b)

    if(sum(ok) > 0) {

      diff <- a[ok] - b[ok]

      # Recherche d'une p-value de test formel d?j? disponible
      pc <- getcol(h9f, c("p.?value", "P_value", "pvalue"))

      if(!is.null(pc)) {

        pvec <- num(h9f[[pc]])
        pvec <- pvec[is.finite(pvec)]

        if(length(pvec)>0) {

          p <- mean(pvec)

          add_result(
            "H9",
            "GPRT_SUP - GPRA_SUP",
            "?GPRT = ?GPRA",
            "?GPRT ? ?GPRA selon H9",
            "Test formel de comparaison GPRT / GPRA",
            mean(diff),
            NA,
            NA,
            p,
            decision5(p)
          )

        } else {

          add_result(
            "H9",
            "GPRT_SUP - GPRA_SUP",
            "?GPRT = ?GPRA",
            "?GPRT ? ?GPRA",
            "Comparaison des coefficients ? covariance absente",
            mean(diff),
            NA,
            NA,
            NA,
            "NON TESTABLE FORMELLEMENT"
          )
        }

      } else {

        add_result(
          "H9",
          "GPRT_SUP - GPRA_SUP",
          "?GPRT = ?GPRA",
          "?GPRT ? ?GPRA",
          "Comparaison des coefficients ? covariance absente",
          mean(diff),
          NA,
          NA,
          NA,
          "NON TESTABLE FORMELLEMENT"
        )
      }

    } else {

      add_result(
        "H9",
        "GPRT_SUP - GPRA_SUP",
        "?GPRT = ?GPRA",
        "?GPRT ? ?GPRA",
        "Test de Wald",
        NA, NA, NA, NA,
        "NON TESTABLE"
      )
    }

  } else {

    add_result(
      "H9",
      "GPRT_SUP - GPRA_SUP",
      "?GPRT = ?GPRA",
      "?GPRT ? ?GPRA",
      "Test de Wald",
      NA, NA, NA, NA,
      "NON TESTABLE : coefficients absents"
    )
  }

} else {

  add_result(
    "H9",
    "GPRT_SUP - GPRA_SUP",
    "?GPRT = ?GPRA",
    "?GPRT ? ?GPRA",
    "Test de Wald",
    NA, NA, NA, NA,
    "NON TESTABLE"
  )
}

# ---------------------------------------------------------------
# NETTOYAGE
# ---------------------------------------------------------------

results <- results[!duplicated(results$Hypothese), ]

# ordre H1-H9
results$ordre <- match(results$Hypothese, paste0("H",1:9))
results <- results[order(results$ordre), ]
results$ordre <- NULL

# ---------------------------------------------------------------
# EXPORT CSV
# ---------------------------------------------------------------

csvfile <- file.path(outdir, "TABLEAU_FINAL_H1_H9_EXACT.csv")

write.csv(
  results,
  csvfile,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------
# EXPORT TXT
# ---------------------------------------------------------------

txtfile <- file.path(outdir, "TABLEAU_FINAL_H1_H9_EXACT.txt")

sink(txtfile)

cat("==========================================================================\n")
cat("RIMEH - TABLEAU FINAL DES TESTS DES HYPOTHESES H1-H9\n")
cat("==========================================================================\n\n")

print(results, row.names = FALSE)

cat("\n\n==========================================================================\n")
cat("REGLE DE DECISION\n")
cat("==========================================================================\n")
cat("Seuil de significativite : 5%\n")
cat("Si p-value < 0.05 : rejet de H0.\n")
cat("Si p-value >= 0.05 : non-rejet de H0.\n")
cat("Une hypothese n'est pas declaree confirmee par le seul signe du coefficient.\n")
cat("Les hypotheses H5/H6/H9 necessitent une comparaison formelle des parametres.\n")
cat("==========================================================================\n")

sink()

# ---------------------------------------------------------------
# RAPPORT DE CONTROLE
# ---------------------------------------------------------------

audit <- data.frame(
  Hypothese = results$Hypothese,
  Estimation = results$Estimation,
  SE = results$SE,
  Statistique = results$Statistique,
  P_value = results$P_value,
  Decision = results$Decision_5pct,
  stringsAsFactors = FALSE
)

write.csv(
  audit,
  file.path(outdir, "AUDIT_STATISTIQUE_H1_H9.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n===============================================================\n")
cat("TERMINE\n")
cat("===============================================================\n")
cat("CSV :", csvfile, "\n")
cat("TXT :", txtfile, "\n")
cat("AUDIT :", file.path(outdir, "AUDIT_STATISTIQUE_H1_H9.csv"), "\n")
cat("===============================================================\n")
