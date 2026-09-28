base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

files <- c(
  "results/analysis/FINAL_ALL_COEFFICIENTS_AUDIT.csv",
  "results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv",
  "results/analysis/FINAL_THETA_DETAIL.csv",
  "results/analysis/FINAL_THETA_DETAIL_ROBUST.csv",
  "results/analysis/FINAL_H1_H3_CORRECTED.csv",
  "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H4.csv",
  "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv",
  "results/analysis/FORMAL_H1_H9/H9_GPRT_VS_GPRA_DETAIL.csv"
)

for (f in files) {

  path <- file.path(base, f)

  cat("\n")
  cat("============================================================\n")
  cat("FICHIER :", f, "\n")
  cat("============================================================\n")

  if (!file.exists(path)) {
    cat("FICHIER ABSENT\n")
    next
  }

  d <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error=function(e) NULL
  )

  if (is.null(d)) {
    cat("ERREUR DE LECTURE\n")
    next
  }

  cat("Dimensions :", nrow(d), "lignes x", ncol(d), "colonnes\n\n")

  cat("COLONNES :\n")
  print(names(d))

  cat("\nPREMIERES LIGNES :\n")
  print(utils::head(d, 5))

  cat("\n------------------------------------------------------------\n")
  cat("COLONNES POTENTIELLEMENT STATISTIQUES\n")
  cat("------------------------------------------------------------\n")

  nms <- names(d)

  for (nm in nms) {

    low <- tolower(nm)

    if (
      grepl("estimate|coef|theta|beta|alpha|gamma", low) ||
      grepl("se|std|error|erreur", low) ||
      grepl("p.value|p_value|pvalue|p-value|signif", low) ||
      grepl("stat|wald|z|t", low) ||
      grepl("cov|variance", low)
    ) {

      cat("\n>>> ", nm, "\n", sep="")

      x <- d[[nm]]

      if (is.numeric(x)) {
        cat(
          "min =", suppressWarnings(min(x, na.rm=TRUE)),
          " | median =", suppressWarnings(median(x, na.rm=TRUE)),
          " | max =", suppressWarnings(max(x, na.rm=TRUE)),
          " | NA =", sum(is.na(x)),
          "\n"
        )
      } else {
        print(utils::head(unique(x), 10))
      }
    }
  }
}

cat("\n\n============================================================\n")
cat("INSPECTION TERMINEE\n")
cat("============================================================\n")
